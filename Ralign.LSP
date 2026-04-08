(vl-load-com)

(defun _2dpt (p)
  (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
)

(defun _pt+ (a b)
  (mapcar '+ a b)
)

(defun _pt- (a b)
  (mapcar '- a b)
)

(defun _pt* (p s)
  (mapcar '(lambda (x) (* x s)) p)
)

(defun _dot2d (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)))
)

(defun _dist2d (a b)
  (distance (list (car a) (cadr a) 0.0) (list (car b) (cadr b) 0.0))
)

(defun _closest-point-on-seg (a b p / ab ap len2 fac)
  (setq a (_2dpt a)
        b (_2dpt b)
        p (_2dpt p)
        ab (_pt- b a)
        ap (_pt- p a)
        len2 (_dot2d ab ab))
  (cond
    ((equal len2 0.0 1e-12) a)
    (t
      (setq fac (/ (_dot2d ap ab) len2))
      (if (< fac 0.0) (setq fac 0.0))
      (if (> fac 1.0) (setq fac 1.0))
      (_pt+ a (_pt* ab fac))
    )
  )
)

(defun _entity-type (e)
  (cdr (assoc 0 (entget e)))
)

(defun _line-segment-from-entity (e pick / ed p1 p2)
  (setq ed (entget e)
        p1 (_2dpt (cdr (assoc 10 ed)))
        p2 (_2dpt (cdr (assoc 11 ed))))
  (list e p1 p2 (_closest-point-on-seg p1 p2 pick))
)

(defun _lwpoly-pts (e / d pts)
  (setq d (entget e)
        pts nil)
  (foreach itm d
    (if (= (car itm) 10)
      (setq pts (append pts (list (_2dpt (cdr itm)))))
    )
  )
  pts
)

(defun _lwpoly-closed-p (e / d f)
  (setq d (entget e)
        f (cdr (assoc 70 d)))
  (= 1 (logand 1 f))
)

(defun _oldpoly-pts (e / en ed pts done)
  (setq en (entnext e)
        pts nil
        done nil)
  (while (and en (not done))
    (setq ed (entget en))
    (cond
      ((= (cdr (assoc 0 ed)) "VERTEX")
        (setq pts (append pts (list (_2dpt (cdr (assoc 10 ed))))))
      )
      ((= (cdr (assoc 0 ed)) "SEQEND")
        (setq done T)
      )
    )
    (if (not done) (setq en (entnext en)))
  )
  pts
)

(defun _oldpoly-closed-p (e / d f)
  (setq d (entget e)
        f (cdr (assoc 70 d)))
  (= 1 (logand 1 f))
)

(defun _best-seg-from-pts (pts closed pick / best bestd i a b cp d n)
  (setq n (length pts)
        i 0
        best nil
        bestd nil)
  (while (< i (1- n))
    (setq a (nth i pts)
          b (nth (1+ i) pts)
          cp (_closest-point-on-seg a b pick)
          d (_dist2d cp pick))
    (if (or (null bestd) (< d bestd))
      (setq best (list a b cp)
            bestd d)
    )
    (setq i (1+ i))
  )
  (if (and closed (> n 1))
    (progn
      (setq a (nth (1- n) pts)
            b (nth 0 pts)
            cp (_closest-point-on-seg a b pick)
            d (_dist2d cp pick))
      (if (or (null bestd) (< d bestd))
        (setq best (list a b cp)
              bestd d)
      )
    )
  )
  best
)

(defun _polyline-segment-from-pick (e pick / typ pts closed seg)
  (setq typ (_entity-type e))
  (cond
    ((= typ "LWPOLYLINE")
      (setq pts (_lwpoly-pts e)
            closed (_lwpoly-closed-p e)
            seg (_best-seg-from-pts pts closed pick))
      (if seg (list e (car seg) (cadr seg) (caddr seg)))
    )
    ((= typ "POLYLINE")
      (setq pts (_oldpoly-pts e)
            closed (_oldpoly-closed-p e)
            seg (_best-seg-from-pts pts closed pick))
      (if seg (list e (car seg) (cadr seg) (caddr seg)))
    )
  )
)

(defun _get-picked-segment (msg / sel e pick typ)
  (setq sel (entsel msg))
  (if sel
    (progn
      (setq e (car sel)
            pick (_2dpt (cadr sel))
            typ (_entity-type e))
      (cond
        ((= typ "LINE") (_line-segment-from-entity e pick))
        ((or (= typ "LWPOLYLINE") (= typ "POLYLINE")) (_polyline-segment-from-pick e pick))
        (t nil)
      )
    )
  )
)

(defun _choose-align-pair (refA refB srcA srcB / d1 d2)
  ;; Result: (srcP1 dstP1 srcP2 dstP2)
  (setq d1 (+ (_dist2d srcA refA) (_dist2d srcB refB))
        d2 (+ (_dist2d srcA refB) (_dist2d srcB refA)))
  (if (<= d1 d2)
    (list srcA refA srcB refB)
    (list srcA refB srcB refA)
  )
)

(defun _perp-correction (refA refB p / cp)
  (setq cp (_closest-point-on-seg refA refB p))
  (_pt- cp p)
)

(defun _move-entity-by (e vec)
  (command "_.MOVE" e "" '(0.0 0.0 0.0) (_2dpt vec))
)

(defun c:RALIGN (/ *error* olderr cmdecho osm ref src refA refB srcEnt srcA srcB pair midS pickAfter corr)
  (setq olderr *error*
        cmdecho (getvar "CMDECHO")
        osm     (getvar "OSMODE"))

  (defun *error* (msg)
    (setvar "CMDECHO" cmdecho)
    (setvar "OSMODE" osm)
    (if (and msg (/= msg "Function cancelled"))
      (princ (strcat "\nError: " msg))
    )
    (setq *error* olderr)
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (prompt "\nRALIGN - align one line/polyline side to another.")
  (prompt "\nSupported objects: LINE, LWPOLYLINE, POLYLINE.")
  (prompt "\nPick exactly on the side you want to use.")

  (setq ref (_get-picked-segment "\nPick REFERENCE line or rectangle/polyline side: "))
  (if (not ref)
    (progn
      (prompt "\nWrong reference. Pick a LINE or a straight polyline side.")
      (*error* nil)
      (exit)
    )
  )

  (setq src (_get-picked-segment "\nPick OBJECT line or rectangle/polyline side to align: "))
  (if (not src)
    (progn
      (prompt "\nWrong object. Pick a LINE or a straight polyline side.")
      (*error* nil)
      (exit)
    )
  )

  (setq refA   (nth 1 ref)
        refB   (nth 2 ref)
        srcEnt (nth 0 src)
        srcA   (nth 1 src)
        srcB   (nth 2 src))

  ;; Exact rotate + move using native ALIGN.
  (setq pair (_choose-align-pair refA refB srcA srcB))
  (command "_.ALIGN"
           srcEnt
           ""
           (nth 0 pair)
           (nth 1 pair)
           (nth 2 pair)
           (nth 3 pair)
           ""
           "N")

  ;; Tiny residual correction normal to the reference side.
  ;; This fixes the visual micro-gap that can remain after ALIGN in some drawings.
  (setq midS (_pt* (_pt+ (nth 1 pair) (nth 3 pair)) 0.5)
        pickAfter (_closest-point-on-seg refA refB midS)
        corr (_perp-correction refA refB midS))

  (if (> (_dist2d '(0.0 0.0 0.0) corr) 1e-8)
    (_move-entity-by srcEnt corr)
  )

  (setvar "CMDECHO" cmdecho)
  (setvar "OSMODE" osm)
  (prompt "\nAligned successfully.")
  (setq *error* olderr)
  (princ)
)

(prompt "\nType RALIGN to start.")
(princ)
