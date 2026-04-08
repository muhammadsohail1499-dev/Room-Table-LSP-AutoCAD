; ============================================================
; ROOMTABLE.LSP (Revised)
; ------------------------------------------------------------
; Workflow:
;   1) Run command: ROOMTABLE
;   2) Pick table insertion point
;   3) Enter the FIRST room label (e.g. 1, A, AA, 101)
;   4) Click inside each enclosed room (press Enter to finish)
;      -> A temporary boundary polyline is created from the internal point
;      -> Area is read
;      -> A row is added to the table: [Room Label, Area]
;      -> (Optional) label text is placed at the clicked point
;
; Notes:
; - Rooms must be truly enclosed (no gaps).
; - Uses -BOUNDARY when available; falls back to BPOLY.
; - Temporary boundary is deleted after area is extracted.
; - Table is created in ModelSpace or PaperSpace depending on active viewport.
; ============================================================

(vl-load-com)

;; ------------------------------
;; Helpers
;; ------------------------------

(defun rt:doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object))
)

(defun rt:get-space (/ doc)
  (setq doc (rt:doc))
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)
  )
)

(defun rt:pt->var (pt)
  (vlax-3d-point (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0)))
)

(defun rt:enttype (e)
  (cdr (assoc 0 (entget e)))
)

(defun rt:format-area (a prec)
  (rtos a 2 prec)
)

(defun rt:str-upper (s) (vl-string-translate "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" s))

(defun rt:all-digits-p (s / i c ok)
  (setq ok T i 1)
  (while (and ok (<= i (strlen s)))
    (setq c (substr s i 1))
    (if (not (wcmatch c "[0-9]")) (setq ok nil))
    (setq i (1+ i))
  )
  ok
)

(defun rt:all-letters-p (s / i c ok)
  (setq ok T i 1)
  (while (and ok (<= i (strlen s)))
    (setq c (substr s i 1))
    (if (not (wcmatch c "[A-Za-z]")) (setq ok nil))
    (setq i (1+ i))
  )
  ok
)

;; Base-26 alpha increment:
;;  A -> B ... Z -> AA ... AZ -> BA ... ZZ -> AAA
(defun rt:alpha-next (s / u chars i carry n out c v)
  (setq u (rt:str-upper s))
  (setq chars '())
  (setq i 1)
  (while (<= i (strlen u))
    (setq chars (cons (substr u i 1) chars))
    (setq i (1+ i))
  )
  (setq carry T)
  (setq out '())
  (while chars
    (setq c (car chars))
    (setq chars (cdr chars))
    (if carry
      (progn
        (setq v (ascii c))
        (if (< v (ascii "Z"))
          (progn
            (setq out (cons (chr (1+ v)) out))
            (setq carry nil)
          )
          (progn
            (setq out (cons "A" out))
            (setq carry T)
          )
        )
      )
      (setq out (cons c out))
    )
  )
  (if carry
    (setq out (cons "A" out))
  )
  (apply 'strcat out)
)

(defun rt:label-next (label)
  (cond
    ((or (null label) (= label "")) "1")
    ((rt:all-digits-p label)
      (itoa (1+ (atoi label)))
    )
    ((rt:all-letters-p label)
      (rt:alpha-next label)
    )
    (T
      ;; Mixed labels (e.g. "1A" or "A-1") -> no safe auto-increment.
      ;; Return the same label; user can type a new one if they want.
      label
    )
  )
)

(defun rt:yesno (msg def / a)
  ;; def: T or nil
  (initget "Yes No")
  (setq a (getkword (strcat msg (if def " [Yes/No] <Yes>: " " [Yes/No] <No>: "))))
  (cond
    ((= a "Yes") T)
    ((= a "No") nil)
    (T def)
  )
)

;; ------------------------------
;; Units (Area)
;; ------------------------------

(defun rt:insunits->meters (iu)
  ;; Returns linear meters per drawing unit (INSUNITS).
  ;; Supports the most common values.
  (cond
    ((= iu 1)  0.0254)   ;; inches
    ((= iu 2)  0.3048)   ;; feet
    ((= iu 4)  0.001)    ;; millimeters
    ((= iu 5)  0.01)     ;; centimeters
    ((= iu 6)  1.0)      ;; meters
    ((= iu 14) 0.9144)   ;; yards
    (T nil)
  )
)

(defun rt:area-factor-to (target / iu u2m a2m2)
  ;; Returns multiplier to convert raw area (drawing units^2) -> target units.
  ;; target: "M2" | "SQFT" | "SQIN" | "RAW"
  (setq iu (getvar "INSUNITS"))
  (setq u2m (rt:insunits->meters iu))
  (if (null u2m)
    (setq u2m 1.0) ;; unitless/unknown: treat drawing unit as meter
  )
  (setq a2m2 (* u2m u2m))
  (cond
    ((= target "M2")   a2m2)
    ((= target "SQFT") (* a2m2 10.763910416709722))
    ((= target "SQIN") (* a2m2 1550.0031000062))
    (T 1.0)
  )
)

(defun rt:prompt-area-units (/ k)
  ;; Returns dotted pair: (factor . label)
  ;; label is used in table header.
  (initget "Raw SQIN SQFT M2")
  (setq k (getkword "
Area units [Raw/SQIN/SQFT/M2] <Raw>: "))
  (if (null k) (setq k "Raw"))
  (cond
    ((= k "SQIN") (cons (rt:area-factor-to "SQIN") "sq in"))
    ((= k "SQFT") (cons (rt:area-factor-to "SQFT") "sq ft"))
    ((= k "M2")   (cons (rt:area-factor-to "M2")   "m^2"))
    (T             (cons 1.0                         "sq units"))
  )
)

;; ------------------------------
;; Table
;; ------------------------------

(defun rt:make-table (ins rowh colw areaLbl / sp tbl)
  (setq sp (rt:get-space))
  ;; 0: title row, 1: header row, 2: first data row placeholder
  (setq tbl (vla-AddTable sp (rt:pt->var ins) 3 2 rowh colw))

  (vla-put-RegenerateTableSuppressed tbl :vlax-true)

  (vla-SetText tbl 0 0 "ROOM AREAS")
  (vla-MergeCells tbl 0 0 0 1)

  (vla-SetText tbl 1 0 "Room")
  (vla-SetText tbl 1 1 (strcat "Area (" areaLbl ")"))

  (vla-put-RegenerateTableSuppressed tbl :vlax-false)
  (vla-Update tbl)
  tbl
)

(defun rt:ensure-row (tbl row / rows)
  (setq rows (vla-get-Rows tbl))
  (while (>= row rows)
    (vla-InsertRows tbl rows (vla-GetRowHeight tbl (1- rows)) 1)
    (setq rows (vla-get-Rows tbl))
  )
)

(defun rt:add-text (pt txt hgt / sp)
  (setq sp (rt:get-space))
  (vla-AddText sp txt (rt:pt->var pt) hgt)
)

;; ------------------------------
;; Boundary + Area
;; ------------------------------

(defun rt:last-new-poly (eBefore / e)
  ;; Find the newest polyline created after eBefore.
  (setq e (entlast))
  (if (or (null e) (eq e eBefore))
    nil
    (progn
      (while (and e (not (eq e eBefore))
                  (not (member (rt:enttype e) '("LWPOLYLINE" "POLYLINE"))))
        (setq e (entprev e))
      )
      (if (and e (not (eq e eBefore))) e nil)
    )
  )
)

(defun rt:try-boundary (pickpt / eBefore eNew)
  ;; Prefer -BOUNDARY (more modern); fall back to BPOLY.
  ;; Returns new polyline ename or nil.
  (setq eBefore (entlast))

  ;; Try -BOUNDARY (sends: internal point, accept defaults, finish)
  (vl-cmdf "_.-BOUNDARY" pickpt "" "" "" "")
  (setq eNew (rt:last-new-poly eBefore))

  (if (null eNew)
    (progn
      (setq eBefore (entlast))
      (vl-cmdf "_.BPOLY" pickpt)
      (setq eNew (rt:last-new-poly eBefore))
    )
  )

  eNew
)

(defun rt:get-enclosing-area (pickpt / oldCmde oldOsm oldErr oldPedit eNew obj a)
  ;; Creates temp boundary, reads area, deletes boundary.
  (setq oldCmde (getvar "CMDECHO"))
  (setq oldOsm  (getvar "OSMODE"))
  (setq oldPedit (getvar "PEDITACCEPT"))
  (setq oldErr  *error*)

  (defun *error* (m)
    (setvar "CMDECHO" oldCmde)
    (setvar "OSMODE" oldOsm)
    (setvar "PEDITACCEPT" oldPedit)
    (setq *error* oldErr)
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "PEDITACCEPT" 1)

  (setq a nil)
  (setq eNew (rt:try-boundary pickpt))

  (if eNew
    (progn
      (setq obj (vlax-ename->vla-object eNew))

      ;; Force closed if available
      (if (vlax-property-available-p obj 'Closed)
        (vla-put-Closed obj :vlax-true)
      )

      (setq a (vlax-curve-getArea eNew))

      ;; delete temporary boundary
      (vla-Delete obj)
    )
  )

  (setvar "CMDECHO" oldCmde)
  (setvar "OSMODE" oldOsm)
  (setvar "PEDITACCEPT" oldPedit)
  (setq *error* oldErr)

  a
)

;; ------------------------------
;; Main command
;; ------------------------------

(defun c:ROOMTABLE (/ ins tbl rowh colw thgt prec putTxt label pickpt a row uPair aFactor aLbl aOut)
  (setq ins (getpoint "\nSpecify table insertion point: "))
  (if (not ins)
    (progn (princ "\nCanceled.") (princ) (exit))
  )

  ;; Sizes (edit defaults here)
  (setq rowh 4.0)   ;; row height
  (setq colw 25.0)  ;; column width
  (setq thgt 2.5)   ;; label text height (if placing text)
  (setq prec 2)     ;; area decimals

  (setq putTxt (rt:yesno "\nPlace room label text at pick point?" T))

  (setq label (getstring T "
Enter FIRST room label (e.g. 1, A, AA) <1>: "))
  (if (= label "") (setq label "1"))

  ;; Ask desired area unit (and convert from drawing units based on INSUNITS)
  (setq uPair (rt:prompt-area-units))
  (setq aFactor (car uPair))
  (setq aLbl (cdr uPair))

  (setq tbl (rt:make-table ins rowh colw aLbl))

  (princ "\nClick INSIDE each enclosed room. Press Enter to finish.")

  (while (setq pickpt (getpoint (strcat "\nPick inside room [" label "] (Enter to stop): ")))
    (setq a (rt:get-enclosing-area pickpt))

    (if (or (not a) (<= a 1e-12))
      (princ "\nNo valid closed boundary found. Make sure the room is fully enclosed (no gaps) and try again.")
      (progn
        ;; Table rows: 0 title, 1 headers, 2 first data row.
        ;; Compute data row index based on current count by asking for next empty row:
        ;; We'll append at the bottom each time.
        (setq row (vla-get-Rows tbl))
        (rt:ensure-row tbl row)

        (vla-SetText tbl row 0 label)
        (setq aOut (* a aFactor))
        (vla-SetText tbl row 1 (rt:format-area aOut prec))

        (if putTxt
          (rt:add-text pickpt label thgt)
        )

        (setq label (rt:label-next label))
        (vla-Update tbl)
      )
    )
  )

  (princ "\nDone.")
  (princ)
)

(princ "\nLoaded ROOMTABLE.LSP (Revised). Type ROOMTABLE to run.")
(princ)
