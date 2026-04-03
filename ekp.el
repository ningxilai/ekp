;;; ekp.el --- Knuth-Plass line breaking for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: emacs-kp contributors
;; Keywords: text, typesetting, CJK
;; Package-Requires: ((emacs "28.1") (websocket "1.15"))

;;; Commentary:

;; Implementation of the Knuth-Plass optimal line breaking algorithm
;; with support for CJK text and hyphenation.
;;
;; Uses deno-bridge for high-performance DP computation via TypeScript/Deno.
;; Falls back to pure Elisp when deno-bridge is unavailable.
;;
;; Reference: Knuth & Plass, "Breaking Paragraphs into Lines" (1981)
;;
;; Usage:
;;   (ekp-pixel-justify "Your text here" 600)
;;   (ekp-pixel-range-justify "Text" 500 700)

;;; Code:

(require 'cl-lib)
(require 'ekp-bridge nil t)

;;; ============================================================
;;; Core Variables
;;; ============================================================

(defconst ekp--load-file (or load-file-name (buffer-file-name))
  "Path to this file, for locating dictionaries.")

(defvar ekp-latin-lang "en_US"
  "Language code for hyphenation (e.g., 'en_US', 'de_DE').")

(defvar ekp-use-deno-bridge t
  "When non-nil, use deno-bridge for DP computation if available.
Set to nil to force pure Elisp implementation.")

(defvar ekp--deno-bridge-ready nil
  "Non-nil if deno-bridge is connected and ready.")

;;; ============================================================
;;; Root Directory
;;; ============================================================

(defun ekp-root-dir ()
  "Return directory containing ekp files."
  (when ekp--load-file
    (file-name-directory ekp--load-file)))

;;; ============================================================
;;; Font Detection (from ekp-utils.el)
;;; ============================================================

(defsubst ekp-cjk-char-p (char)
  "Return non-nil if CHAR is a CJK character."
  (let ((entry (aref (category-table) char)))
    (or (aref entry ?c) ; Chinese
        (aref entry ?h) ; Korean
        (aref entry ?j) ; Japanese
        )))

(defun ekp-font-family (string &optional position)
  (format "%s" (font-get (font-at (or position 0) nil string) :family)))

(defun ekp-font-monospace-p (font-family)
  (let* ((font (find-font (font-spec :family font-family)))
         (font-name (font-xlfd-name font))
         (type (nth 10 (split-string font-name "-" t))))
    (or (or (string= "m" type) (string= "c" type))
        (let ((info (font-info font-name)))
          (and info (> (length info) 4)
               (= (aref info 7) (aref info 11)))))))

(defun ekp-get-latin-letter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (let ((char (char-after)))
                  (not (or (and (>= char ?a) (<= char ?z))
                           (and (>= char ?A) (<= char ?Z))))))
      (forward-char 1))
    (unless (eobp)
      (buffer-substring (point) (1+ (point))))))

(defun ekp-get-cjk-letter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (let* ((char (char-after))
                       (width (char-width char)))
                  (or (or (= 1 width) (= 0 width))
                      (not (ekp-cjk-char-p char)))))
      (forward-char 1))
    (unless (eobp)
      (buffer-substring (point) (1+ (point))))))

(defun ekp-monospace-p (string)
  "Return monospace font family for STRING's Latin letters, or nil."
  (if-let* ((letter (ekp-get-latin-letter string))
            (font-family (ekp-font-family letter)))
      (when (ekp-font-monospace-p font-family)
        font-family)
    (face-attribute 'default :family)))

(defun ekp-word-spacing-pixel (string)
  (if-let ((font-family (ekp-monospace-p string)))
      (string-pixel-width
       (propertize " " 'face `(:family ,font-family)))
    (let* ((letter (ekp-get-latin-letter string))
           (font-family (ekp-font-family letter)))
      (string-pixel-width
       (propertize " " 'face `(:family ,font-family))))))

(defun ekp-latin-font (string)
  (if-let ((letter (ekp-get-latin-letter string)))
      (ekp-font-family letter)
    (face-attribute 'default :family)))

(defun ekp-cjk-font (string)
  (if-let ((letter (ekp-get-cjk-letter string)))
      (ekp-font-family letter)
    (ekp-font-family "牛")))

(defun ekp-pixel-spacing (pixel)
  "Return a pixel spacing with a PIXEL pixel width."
  (if (= pixel 0)
      ""
    (propertize " " 'display `(space :width (,pixel)))))

(defun ekp-cjk-fw-punct-p (str)
  "Return if STR starts with a CJK full-width punctuation."
  (let ((char (seq-first str)))
    (or (equal (char-syntax char) ?.)
        (and (>= char #x3000) (<= char #x303F))
        (and (>= char #xFF00) (<= char #xFF60)))))

;;; ============================================================
;;; Box Splitting (from ekp-utils.el)
;;; ============================================================

(defun ekp--flush-latin-word (word boxes)
  (if word (cons word boxes) boxes))

(defun ekp--flush-cjk-char (char boxes)
  (if char (cons char boxes) boxes))

(defun ekp--flush-spaces (spaces boxes prev-state next-width)
  (when (and spaces (not (string-empty-p spaces)))
    (let ((cjk-involved (or (= prev-state 2) (= next-width 2))))
      (cond
       ((null boxes)
        (setq boxes (cons spaces boxes)))
       (cjk-involved
        (setq boxes (cons spaces boxes)))
       ((> (length spaces) 1)
        (setq boxes (cons (substring spaces 0 -1) boxes)))
       (t nil))))
  boxes)

(defun ekp--flush-trailing-spaces (spaces boxes)
  (if (and spaces (not (string-empty-p spaces)))
      (cons spaces boxes)
    boxes))

(defun ekp--handle-latin-char (str state latin-word cjk-char boxes)
  (if (= state 1)
      (list 1 (concat latin-word str) nil boxes)
    (list 1 str nil (ekp--flush-cjk-char cjk-char boxes))))

(defun ekp--handle-cjk-char (str state latin-word cjk-char boxes)
  (if (= state 1)
      (list 2 nil nil (cons str (ekp--flush-latin-word latin-word boxes)))
    (if (ekp-cjk-fw-punct-p str)
        (list 2 nil nil (cons (concat cjk-char str) boxes))
      (list 2 nil str (ekp--flush-cjk-char cjk-char boxes)))))

(defun ekp-split-to-boxes (string)
  "Split STRING into typographic boxes.
Latin words become single boxes; CJK chars are individual boxes.
Whitespace runs are preserved as separate boxes; CJK punctuation attaches to preceding char."
  (if (string-blank-p string)
      (vector string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((state (char-width (seq-first string)))
            (prev-state 1)
            latin-word cjk-char spaces boxes)
        (while (not (eobp))
          (let* ((str (buffer-substring (point) (1+ (point))))
                 (width (string-width str)))
            (cond
             ((or (string-blank-p str) (= 0 width))
              (setq boxes (ekp--flush-cjk-char cjk-char boxes))
              (when cjk-char (setq prev-state 2))
              (setq cjk-char nil)
              (setq boxes (ekp--flush-latin-word latin-word boxes))
              (when latin-word (setq prev-state 1))
              (setq latin-word nil)
              (setq spaces (concat spaces str)))
             (t
              (setq boxes (ekp--flush-spaces spaces boxes prev-state width))
              (setq spaces nil)
              (cond
               ((= 1 width)
                (pcase-let ((`(,s ,lw ,cc ,bx)
                             (ekp--handle-latin-char
                              str state latin-word cjk-char boxes)))
                  (setq state s latin-word lw cjk-char cc boxes bx)))
               ((= 2 width)
                (pcase-let ((`(,s ,lw ,cc ,bx)
                             (ekp--handle-cjk-char
                              str state latin-word cjk-char boxes)))
                  (setq state s latin-word lw cjk-char cc boxes bx)))))))
          (forward-char 1))
        (setq boxes (ekp--flush-cjk-char cjk-char boxes))
        (setq boxes (ekp--flush-latin-word latin-word boxes))
        (setq boxes (ekp--flush-trailing-spaces spaces boxes))
        (vconcat (nreverse boxes))))))

;;; ============================================================
;;; Hyphenation (from ekp-hyphen.el)
;;; ============================================================

(cl-defstruct (ekp-hyphen (:constructor ekp-hyphen--create))
  "Hyphenator object."
  patterns cache maxlen left right)

(defvar ekp-hyphen--cache (make-hash-table :test 'equal)
  "Cache: dictionary path -> compiled ekp-hyphen.")

(defvar ekp-hyphen--langs (make-hash-table :test 'equal)
  "Registry: language code -> dictionary file path.")

(defvar ekp-hyphen--langs-short (make-hash-table :test 'equal)
  "Fallback: short code -> first matching dict path.")

(defun ekp-hyphen-load-languages (dir)
  "Scan DIR for .dic files, populate language registry."
  (dolist (file (directory-files dir t "\\.dic\\'"))
    (let* ((name (file-name-nondirectory file))
           (lang (replace-regexp-in-string "\\(^hyph_\\|\\.dic$\\)" "" name))
           (short (car (split-string lang "_"))))
      (puthash lang file ekp-hyphen--langs)
      (unless (gethash short ekp-hyphen--langs-short)
        (puthash short file ekp-hyphen--langs-short)))))

(ekp-hyphen-load-languages (expand-file-name "dictionaries" (ekp-root-dir)));; Load dictionaries on require

(defun ekp-hyphen--resolve-lang (lang)
  "Resolve LANG to dictionary path, trying exact then short forms."
  (or (gethash lang ekp-hyphen--langs)
      (let* ((norm (downcase (replace-regexp-in-string "-" "_" lang)))
             (parts (split-string norm "_"))
             found)
        (while (and parts (not found))
          (setq found (gethash (string-join parts "_")
                               ekp-hyphen--langs-short)
                parts (butlast parts)))
        found)))

(defun ekp-hyphen--parse-pattern (pat)
  "Parse PAT like 'hy3ph' into (letters offset . values)."
  (let ((pos 0) (len (length pat)) letters values)
    (while (< pos len)
      (let ((digit 0))
        (when (and (< pos len)
                   (>= (aref pat pos) ?0)
                   (<= (aref pat pos) ?9))
          (setq digit (- (aref pat pos) ?0))
          (cl-incf pos))
        (push digit values)
        (when (and (< pos len)
                   (or (< (aref pat pos) ?0) (> (aref pat pos) ?9)))
          (push (aref pat pos) letters)
          (cl-incf pos))))
    (setq letters (apply #'string (nreverse letters))
          values (vconcat (nreverse values)))
    (let ((start 0) (end (length values)))
      (while (and (< start end) (= (aref values start) 0)) (cl-incf start))
      (while (and (> end start) (= (aref values (1- end)) 0)) (cl-decf end))
      (when (> end start)
        (list letters start (cl-subseq values start end))))))

(defun ekp-hyphen--compile (path)
  "Compile dictionary at PATH into ekp-hyphen struct."
  (let ((patterns (make-hash-table :test 'equal))
        (maxlen 0))
    (with-temp-buffer
      (insert-file-contents path)
      (forward-line 1)
      (while (not (eobp))
        (let* ((line (string-trim (buffer-substring-no-properties
                                   (point) (line-end-position))))
               (skip (or (string-empty-p line)
                         (string-match-p "^[%#]\\|HYPHENMIN" line)
                         (string-match-p "/" line))))
          (unless skip
            (setq line (replace-regexp-in-string
                        "\\^\\^\\([0-9a-fA-F]\\{2\\}\\)"
                        (lambda (m) (string (string-to-number
                                             (match-string 1 m) 16)))
                        line))
            (when-let ((parsed (ekp-hyphen--parse-pattern line)))
              (puthash (car parsed) (cdr parsed) patterns)
              (setq maxlen (max maxlen (length (car parsed)))))))
        (forward-line 1)))
    (ekp-hyphen--create :patterns patterns
                        :cache (make-hash-table :test 'equal)
                        :maxlen maxlen
                        :left 2 :right 2)))

(defun ekp-hyphen--compute (h word)
  "Compute break positions for WORD using hyphenator H."
  (let* ((padded (concat "." (downcase word) "."))
         (len (length padded))
         (maxlen (ekp-hyphen-maxlen h))
         (patterns (ekp-hyphen-patterns h))
         (prio (make-vector (1+ len) 0)))
    (dotimes (i (1- len))
      (cl-loop for j from (1+ i) to (min (+ i maxlen) len)
               for pat = (gethash (substring padded i j) patterns)
               when pat do
               (let ((off (car pat)) (vals (cadr pat)))
                 (dotimes (k (length vals))
                   (let ((pos (+ i off k)))
                     (when (< pos (length prio))
                       (aset prio pos (max (aref prio pos)
                                           (aref vals k)))))))))
    (let (result)
      (dotimes (i (length prio))
        (when (cl-oddp (aref prio i))
          (push (1- i) result)))
      (nreverse result))))

(defun ekp-hyphen--positions (h word)
  "Get cached break positions for WORD."
  (let* ((key (downcase word))
         (cache (ekp-hyphen-cache h)))
    (or (gethash key cache)
        (puthash key (ekp-hyphen--compute h word) cache))))

(defun ekp-hyphen-create (&optional lang file left right)
  "Create hyphenator for LANG or dictionary FILE."
  (let ((path (or (and lang (ekp-hyphen--resolve-lang lang)) file)))
    (unless path (error "No dictionary for: %s" lang))
    (let ((h (or (gethash path ekp-hyphen--cache)
                 (puthash path (ekp-hyphen--compile path)
                          ekp-hyphen--cache))))
      (if (or left right)
          (ekp-hyphen--create :patterns (ekp-hyphen-patterns h)
                              :cache (ekp-hyphen-cache h)
                              :maxlen (ekp-hyphen-maxlen h)
                              :left (or left 2) :right (or right 2))
        h))))

(defun ekp-hyphen-positions (h word)
  "Return valid break positions in WORD, respecting margins."
  (let ((left (ekp-hyphen-left h))
        (right (- (length word) (ekp-hyphen-right h))))
    (cl-remove-if-not (lambda (p) (and (>= p left) (<= p right)))
                      (ekp-hyphen--positions h word))))

(defun ekp-hyphen-inserted (h word &optional hyphen)
  "Return WORD with HYPHEN inserted at break points."
  (let ((hyphen (or hyphen "-")) (result word) (off 0))
    (dolist (pos (ekp-hyphen-positions h word))
      (setq result (concat (substring result 0 (+ pos off))
                           hyphen
                           (substring result (+ pos off)))
            off (+ off (length hyphen))))
    result))

(defun ekp-hyphen-boxes (h word)
  "Split WORD into syllables at break points."
  (split-string (ekp-hyphen-inserted h word " ") " "))

;;; ============================================================
;;; Glue Parameters
;;; ============================================================

(defvar ekp-lws-ideal-pixel nil "Ideal Latin word spacing (pixels).")
(defvar ekp-lws-stretch-pixel nil "Max stretch for Latin spacing.")
(defvar ekp-lws-shrink-pixel nil "Max shrink for Latin spacing.")
(defvar ekp-mws-ideal-pixel nil "Ideal mixed (Latin-CJK) spacing.")
(defvar ekp-mws-stretch-pixel nil "Max stretch for mixed spacing.")
(defvar ekp-mws-shrink-pixel nil "Max shrink for mixed spacing.")
(defvar ekp-cws-ideal-pixel nil "Ideal CJK character spacing.")
(defvar ekp-cws-stretch-pixel nil "Max stretch for CJK spacing.")
(defvar ekp-cws-shrink-pixel nil "Max shrink for CJK spacing.")

(defvar ekp-lws-max-pixel nil)
(defvar ekp-lws-min-pixel nil)
(defvar ekp-mws-max-pixel nil)
(defvar ekp-mws-min-pixel nil)
(defvar ekp-cws-max-pixel nil)
(defvar ekp-cws-min-pixel nil)

(defvar ekp-default-cws-stretch-pixel 2
  "Max stretched pixel of whitespace between CJK chars.")

(defvar ekp-line-penalty 10 "Penalty for each line break.")
(defvar ekp-hyphen-penalty 50 "Penalty for hyphenated breaks.")
(defvar ekp-adjacent-fitness-penalty 100
  "Penalty when adjacent lines differ in tightness by >1 class.")
(defvar ekp-consecutive-hyphen-penalty 100
  "Base penalty multiplier for consecutive hyphenated lines.")
(defvar ekp-forced-break-penalty 10000
  "Base penalty for forced breaks where no valid break exists.")
(defvar ekp-last-line-short-penalty 50
  "Penalty multiplier for underfilled last lines.")
(defvar ekp-last-line-min-ratio 0.5 "Minimum fill ratio for last line.")
(defvar ekp-looseness 0 "Target line count offset.")
(defvar ekp-threshold-factor 0 "Threshold factor for early pruning.")
(defvar ekp-flagged-penalty -10000 "Penalty for flagged (forced) breaks.")

;;; ============================================================
;;; Paragraph Cache Structure
;;; ============================================================

(cl-defstruct (ekp-para (:constructor ekp-para--create))
  "Preprocessed paragraph data."
  string latin-font cjk-font
  boxes boxes-widths boxes-types glues-types
  hyphen-pixel hyphen-positions
  flagged-positions
  ideal-prefixs min-prefixs max-prefixs
  glue-params
  (dp-cache nil :type hash-table))

(defvar ekp--para-cache nil
  "Cache: hash-key -> ekp-para struct.")

(defvar ekp--use-default-params t
  "Internal flag for parameter initialization.")

;;; ============================================================
;;; Parameter Management
;;; ============================================================

(defun ekp--params-set-p ()
  "Return non-nil if all spacing parameters are set."
  (and ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
       ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
       ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))

(defun ekp-param-set-default (string)
  "Set default spacing parameters based on STRING's font."
  (let* ((lws (ekp-word-spacing-pixel string))
         (mws (- lws 1)))
    (ekp-param-set lws (ceiling (/ (float lws) 2)) (ceiling (/ (float lws) 3))
                   mws (ceiling (/ (float mws) 2)) (ceiling (/ (float mws) 3))
                   0 ekp-default-cws-stretch-pixel 0)))

(defun ekp-param-set (lws-i lws-+ lws-- mws-i mws-+ mws-- cws-i cws-+ cws--)
  "Set all spacing parameters."
  (setq ekp-lws-ideal-pixel lws-i ekp-lws-stretch-pixel lws-+
        ekp-lws-shrink-pixel lws-- ekp-mws-ideal-pixel mws-i
        ekp-mws-stretch-pixel mws-+ ekp-mws-shrink-pixel mws--
        ekp-cws-ideal-pixel cws-i ekp-cws-stretch-pixel cws-+
        ekp-cws-shrink-pixel cws--)
  (unless (ekp--params-set-p)
    (error "All spacing parameters must be non-nil"))
  (setq ekp-lws-max-pixel (+ lws-i lws-+) ekp-lws-min-pixel (- lws-i lws--)
        ekp-mws-max-pixel (+ mws-i mws-+) ekp-mws-min-pixel (- mws-i mws--)
        ekp-cws-max-pixel (+ cws-i cws-+) ekp-cws-min-pixel (- cws-i cws--))
  (setq ekp--use-default-params nil))

;;; ============================================================
;;; Text Analysis
;;; ============================================================

(defconst ekp--latin-regexp
  "[A-Za-z'\\-\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF\u0100-\u024F\u1E00-\u1EFF]"
  "Regexp matching Latin characters including accented forms.")

(defun ekp--split-with-hyphen (string)
  "Split STRING into boxes with hyphenation points marked.
Returns (boxes-vector . hyphen-positions-vector)."
  (let* ((boxes (ekp-split-to-boxes string))
         (idx 0) new-boxes hyphen-idxs)
    (dolist (box (append boxes nil))
      (if (string-match
           (format "^\\([[{<„‚¿¡*@\"']*\\)\\(%s+\\)\\([]}>.,*?\"']*\\)$"
                   ekp--latin-regexp)
           box)
          (let* ((left (match-string 1 box))
                 (word (match-string 2 box))
                 (right (match-string 3 box))
                 (parts (ekp-hyphen-boxes
                         (ekp-hyphen-create ekp-latin-lang) word))
                 (n (length parts)))
            (when left (setcar parts (concat left (car parts))))
            (when right (setcar (last parts)
                                (concat (car (last parts)) right)))
            (push parts new-boxes)
            (dotimes (i n)
              (when (< i (1- n)) (push idx hyphen-idxs))
              (cl-incf idx)))
        (push (list box) new-boxes)
        (cl-incf idx)))
    (cons (vconcat (apply #'append (nreverse new-boxes)))
          (vconcat (nreverse hyphen-idxs)))))

(defun ekp--str-type (str)
  "STR should be single letter string."
  (cond
   ((or (string-blank-p str) (= (string-width str) 0)) 'space)
   ((or (string= "\u201C" str) (string= "\u201D" str)) 'cjk)
   ((= (string-width str) 1) 'latin)
   ((= (string-width str) 2)
    (if (ekp-cjk-fw-punct-p str) 'cjk-punct 'cjk))
   (t (error "Abnormal string width %s for %s"
             (string-width str) str))))

(defun ekp--box-type (box)
  (unless (or (null box) (string-empty-p box))
    (if (or (string-blank-p box) (= (string-width box) 0))
        '(space . space)
      (cons (ekp--str-type (substring box 0 1))
            (ekp--str-type (substring box -1))))))

(defun ekp--glue-type (prev-box-type curr-box-type)
  (let ((before (cdr prev-box-type))
        (after (car curr-box-type)))
    (if before
        (cond
         ((or (eq before 'space) (eq after 'space)) 'nws)
         ((and (eq before 'latin) (eq after 'latin)) 'lws)
         ((and (eq before 'cjk) (eq after 'cjk)) 'cws)
         ((or (and (eq before 'cjk) (eq after 'latin))
              (and (eq before 'latin) (eq after 'cjk)))
          'mws)
         ((or (eq before 'cjk-punct) (eq after 'cjk-punct)) 'cws))
      'nws)))

(defun ekp--compute-glue-types (boxes boxes-types hyphen-positions)
  "Compute glue types for BOXES."
  (let* ((n (length boxes))
         (glues (make-vector n nil))
         prev-type)
    (dolist (i (append hyphen-positions nil))
      (aset glues (1+ i) 'nws))
    (dotimes (i n)
      (unless (aref glues i)
        (let ((curr-type (aref boxes-types i)))
          (aset glues i (ekp--glue-type prev-type curr-type))
          (setq prev-type curr-type))))
    glues))

(defun ekp-glue-ideal-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-ideal-pixel)
        ((eq 'mws type) ekp-mws-ideal-pixel)
        ((eq 'cws type) ekp-cws-ideal-pixel)))

(defun ekp-glue-min-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-min-pixel)
        ((eq 'mws type) ekp-mws-min-pixel)
        ((eq 'cws type) ekp-cws-min-pixel)))

(defun ekp-glue-max-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) ekp-lws-max-pixel)
        ((eq 'mws type) ekp-mws-max-pixel)
        ((eq 'cws type) ekp-cws-max-pixel)))

(defun ekp--para-glue-ideal (para type)
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-ideal))
          ((eq 'mws type) (plist-get params :mws-ideal))
          ((eq 'cws type) (plist-get params :cws-ideal)))))

(defun ekp--para-glue-shrink (para type)
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-shrink))
          ((eq 'mws type) (plist-get params :mws-shrink))
          ((eq 'cws type) (plist-get params :cws-shrink)))))

(defun ekp--para-glue-stretch (para type)
  (let ((params (ekp-para-glue-params para)))
    (cond ((or (null type) (eq 'nws type)) 0)
          ((eq 'lws type) (plist-get params :lws-stretch))
          ((eq 'mws type) (plist-get params :mws-stretch))
          ((eq 'cws type) (plist-get params :cws-stretch)))))

(defun ekp--para-glue-min (para type)
  (- (ekp--para-glue-ideal para type)
     (ekp--para-glue-shrink para type)))

(defun ekp--para-glue-max (para type)
  (+ (ekp--para-glue-ideal para type)
     (ekp--para-glue-stretch para type)))

;;; ============================================================
;;; Cache Implementation
;;; ============================================================

(defun ekp--para-hash (string)
  "Compute fast hash key for STRING."
  (let ((latin-font (ekp-latin-font string))
        (cjk-font (ekp-cjk-font string)))
    (sxhash
     (list (sxhash string)
           (sxhash (prin1-to-string (object-intervals string)))
           latin-font cjk-font
           ekp-lws-ideal-pixel ekp-lws-stretch-pixel ekp-lws-shrink-pixel
           ekp-mws-ideal-pixel ekp-mws-stretch-pixel ekp-mws-shrink-pixel
           ekp-cws-ideal-pixel ekp-cws-stretch-pixel ekp-cws-shrink-pixel))))

(defun ekp--make-para (string)
  "Create and fully initialize ekp-para struct for STRING."
  (when (or ekp--use-default-params (null (ekp--params-set-p)))
    (ekp-param-set-default string))
  (setq ekp--use-default-params t)
  (let* ((latin-font (ekp-latin-font string))
         (cjk-font (ekp-cjk-font string))
         (split-result (ekp--split-with-hyphen string))
         (boxes (car split-result))
         (hyphen-positions (cdr split-result))
         (n (length boxes))
         (boxes-widths (vconcat (mapcar #'string-pixel-width boxes)))
         (boxes-types (vconcat (mapcar #'ekp--box-type boxes)))
         (glues-types (ekp--compute-glue-types
                       boxes boxes-types hyphen-positions))
         (hyphen-pixel (string-pixel-width "-"))
         (ideal-prefixs (make-vector (1+ n) 0))
         (min-prefixs (make-vector (1+ n) 0))
         (max-prefixs (make-vector (1+ n) 0)))
    (dotimes (i n)
      (let ((box-w (aref boxes-widths i))
            (glue-type (aref glues-types i)))
        (aset ideal-prefixs (1+ i)
              (+ (aref ideal-prefixs i) box-w
                 (ekp-glue-ideal-pixel glue-type)))
        (aset min-prefixs (1+ i)
              (+ (aref min-prefixs i) box-w
                 (ekp-glue-min-pixel glue-type)))
        (aset max-prefixs (1+ i)
              (+ (aref max-prefixs i) box-w
                 (ekp-glue-max-pixel glue-type)))))
    (ekp-para--create
     :string string
     :latin-font latin-font
     :cjk-font cjk-font
     :boxes boxes
     :boxes-widths boxes-widths
     :boxes-types boxes-types
     :glues-types glues-types
     :hyphen-pixel hyphen-pixel
     :hyphen-positions hyphen-positions
     :ideal-prefixs ideal-prefixs
     :min-prefixs min-prefixs
     :max-prefixs max-prefixs
     :glue-params (list :lws-ideal ekp-lws-ideal-pixel
                        :lws-stretch ekp-lws-stretch-pixel
                        :lws-shrink ekp-lws-shrink-pixel
                        :mws-ideal ekp-mws-ideal-pixel
                        :mws-stretch ekp-mws-stretch-pixel
                        :mws-shrink ekp-mws-shrink-pixel
                        :cws-ideal ekp-cws-ideal-pixel
                        :cws-stretch ekp-cws-stretch-pixel
                        :cws-shrink ekp-cws-shrink-pixel)
     :dp-cache (make-hash-table :test 'eql :size 20))))

(defun ekp--get-para (string)
  "Get or create ekp-para struct for STRING."
  (unless ekp--para-cache
    (setq ekp--para-cache (make-hash-table :test 'eql :size 100)))
  (let ((key (ekp--para-hash string)))
    (or (gethash key ekp--para-cache)
        (let ((para (ekp--make-para string)))
          (puthash key para ekp--para-cache)
          para))))

(defun ekp-clear-caches ()
  "Clear all paragraph caches."
  (interactive)
  (setq ekp--para-cache nil))

;;; ============================================================
;;; Paragraph Accessors
;;; ============================================================

(defun ekp--boxes (string) (ekp-para-boxes (ekp--get-para string)))
(defun ekp--boxes-widths (string) (ekp-para-boxes-widths (ekp--get-para string)))
(defun ekp--glues-types (string) (ekp-para-glues-types (ekp--get-para string)))
(defun ekp--ideal-prefixs (string) (ekp-para-ideal-prefixs (ekp--get-para string)))
(defun ekp--min-prefixs (string) (ekp-para-min-prefixs (ekp--get-para string)))
(defun ekp--max-prefixs (string) (ekp-para-max-prefixs (ekp--get-para string)))
(defun ekp--hyphen-pixel (string) (ekp-para-hyphen-pixel (ekp--get-para string)))
(defun ekp--hyphen-positions (string) (ekp-para-hyphen-positions (ekp--get-para string)))
(defun ekp--hyphen-str (_string) "-")

;;; ============================================================
;;; K-P Badness and Demerits
;;; ============================================================

(defun ekp--compute-badness (adjustment-pixel flexibility-pixel)
  (cond
   ((= adjustment-pixel 0) 0)
   ((<= flexibility-pixel 0) 10000)
   (t (let ((ratio (/ (float adjustment-pixel) flexibility-pixel)))
        (min 10000 (* 100 (expt (abs ratio) 3)))))))

(defun ekp--compute-fitness-class (adjustment-pixel flexibility-pixel)
  (if (<= flexibility-pixel 0)
      1
    (let ((ratio (/ (float adjustment-pixel) flexibility-pixel)))
      (cond
       ((< ratio -0.5) 0)
       ((< ratio 0.5) 1)
       ((< ratio 1.0) 2)
       (t 3)))))

(defun ekp--compute-demerits (badness penalty prev-fitness curr-fitness
                                      end-with-hyphenp prev-hyphen-count)
  (let* ((base (expt (+ ekp-line-penalty badness) 2))
         (with-penalty (+ base (* penalty penalty)))
         (fitness-delta (abs (- prev-fitness curr-fitness)))
         (with-fitness (if (> fitness-delta 1)
                           (+ with-penalty ekp-adjacent-fitness-penalty)
                         with-penalty))
         (hyphen-count (if end-with-hyphenp (1+ prev-hyphen-count) 0))
         (with-hyphen (if end-with-hyphenp
                          (+ with-fitness (* ekp-consecutive-hyphen-penalty
                                             hyphen-count hyphen-count))
                        with-fitness)))
    with-hyphen))

(defun ekp--gaps-list (glues-types)
  (list (seq-count (lambda (it) (eq 'lws it)) glues-types)
        (seq-count (lambda (it) (eq 'mws it)) glues-types)
        (seq-count (lambda (it) (eq 'cws it)) glues-types)))

(defun ekp--compute-stretch-capacity (para gaps-list)
  (let ((params (ekp-para-glue-params para)))
    (+ (* (nth 0 gaps-list) (plist-get params :lws-stretch))
       (* (nth 1 gaps-list) (plist-get params :mws-stretch))
       (* (nth 2 gaps-list) (plist-get params :cws-stretch)))))

(defun ekp--compute-shrink-capacity (para gaps-list)
  (let ((params (ekp-para-glue-params para)))
    (+ (* (nth 0 gaps-list) (plist-get params :lws-shrink))
       (* (nth 1 gaps-list) (plist-get params :mws-shrink)))))

(defun ekp--line-badness-and-fitness (para ideal-pixel line-pixel glues-types)
  (let* ((glues-types (seq-drop glues-types 1))
         (gaps-list (ekp--gaps-list glues-types))
         (adjustment (- line-pixel ideal-pixel))
         (flexibility (if (> adjustment 0)
                          (ekp--compute-stretch-capacity para gaps-list)
                        (ekp--compute-shrink-capacity para gaps-list)))
         (badness (ekp--compute-badness adjustment flexibility))
         (fitness (ekp--compute-fitness-class adjustment flexibility)))
    (list :badness badness
          :fitness fitness
          :gaps gaps-list
          :adjustment adjustment
          :flexibility flexibility)))

(defun ekp--sorted-vector-member-p (vec n)
  (and vec
       (> (length vec) 0)
       (let ((lo 0) (hi (1- (length vec))))
         (while (< lo hi)
           (let ((mid (/ (+ lo hi) 2)))
             (if (< (aref vec mid) n)
                 (setq lo (1+ mid))
               (setq hi mid))))
         (= (aref vec lo) n))))

(defalias 'ekp--hyphenate-p #'ekp--sorted-vector-member-p)
(defalias 'ekp--flagged-p #'ekp--sorted-vector-member-p)

;;; ============================================================
;;; Dynamic Programming Line Breaking (Pure Elisp)
;;; ============================================================

(defun ekp--dp-init-arrays (n)
  (let ((backptrs (make-vector (1+ n) nil))
        (demerits (make-vector (1+ n) nil))
        (rests (make-vector (1+ n) nil))
        (gaps (make-vector (1+ n) nil))
        (hyphen-counts (make-vector (1+ n) 0))
        (fitness-classes (make-vector (1+ n) 1))
        (line-counts (make-vector (1+ n) 0))
        (alt-paths (when (/= ekp-looseness 0)
                     (make-hash-table :test 'equal :size (min 1000 (* n 10))))))
    (aset demerits 0 0.0)
    (when alt-paths
      (puthash (cons 0 0) (cons nil 0.0) alt-paths))
    (list backptrs demerits rests gaps
          hyphen-counts fitness-classes line-counts alt-paths)))

(defun ekp--leading-space-width (i boxes-types boxes-widths)
  (let ((n (length boxes-types)) (width 0) (pos i))
    (while (and (< pos n)
                (let ((box-type (aref boxes-types pos)))
                  (and box-type (eq (car box-type) 'space))))
      (cl-incf width (aref boxes-widths pos))
      (cl-incf pos))
    width))

(defun ekp--trailing-space-width (k boxes-types boxes-widths)
  (let ((width 0) (pos (1- k)))
    (while (and (>= pos 0)
                (let ((box-type (aref boxes-types pos)))
                  (and box-type (eq (car box-type) 'space))))
      (cl-incf width (aref boxes-widths pos))
      (cl-decf pos))
    width))

(defun ekp--dp-line-metrics (para i k glues-types ideal-prefixs min-prefixs max-prefixs)
  (let* ((leading-glue-type (aref glues-types i))
         (boxes-types (ekp-para-boxes-types para))
         (boxes-widths (ekp-para-boxes-widths para))
         (leading-space-w (if (> i 0)
                              (ekp--leading-space-width i boxes-types boxes-widths)
                            0))
         (trailing-space-w (ekp--trailing-space-width k boxes-types boxes-widths))
         (space-w (+ leading-space-w trailing-space-w)))
    (list (- (aref ideal-prefixs k) (aref ideal-prefixs i)
             (ekp--para-glue-ideal para leading-glue-type) space-w)
          (- (aref min-prefixs k) (aref min-prefixs i)
             (ekp--para-glue-min para leading-glue-type) space-w)
          (- (aref max-prefixs k) (aref max-prefixs i)
             (ekp--para-glue-max para leading-glue-type) space-w))))

(defun ekp--dp-force-break (para i k arrays glues-types hyphen-positions ideal-prefixs hyphen-pixel line-pixel)
  (let* ((backptrs (nth 0 arrays))
         (demerits (nth 1 arrays))
         (rests (nth 2 arrays))
         (gaps (nth 3 arrays))
         (fitness-classes (nth 5 arrays))
         (line-counts (nth 6 arrays))
         (break-pos (1- k))
         (hyphenate-p (ekp--hyphenate-p hyphen-positions break-pos))
         (boxes-types (ekp-para-boxes-types para))
         (boxes-widths (ekp-para-boxes-widths para))
         (leading-space-w (if (> i 0)
                              (ekp--leading-space-width i boxes-types boxes-widths)
                            0))
         (trailing-space-w (ekp--trailing-space-width k boxes-types boxes-widths))
         (space-w (+ leading-space-w trailing-space-w))
         (ideal-pixel (- (aref ideal-prefixs break-pos)
                         (aref ideal-prefixs i)
                         (ekp--para-glue-ideal para (aref glues-types i))
                         space-w))
         (rest-pixel (- line-pixel ideal-pixel)))
    (when hyphenate-p (cl-incf ideal-pixel hyphen-pixel))
    (aset demerits break-pos (+ ekp-forced-break-penalty (expt rest-pixel 2)))
    (aset rests break-pos rest-pixel)
    (aset backptrs break-pos i)
    (aset fitness-classes break-pos 3)
    (aset line-counts break-pos (1+ (aref line-counts i)))
    (aset gaps break-pos
          (ekp--gaps-list (seq-drop (cl-subseq glues-types i break-pos) 1)))))

(defun ekp--dp-compute-line-demerits (para j is-last end-with-hyphenp
                                         ideal-pixel line-pixel
                                         glues-types i k
                                         prev-hyphen-count prev-fitness
                                         &optional end-with-flaggedp)
  (cond
   (end-with-flaggedp
    (let* ((result (ekp--line-badness-and-fitness
                    para ideal-pixel line-pixel
                    (seq-subseq glues-types i k)))
           (line-gaps (plist-get result :gaps)))
      (list ekp-flagged-penalty line-gaps 1 0)))
   ((= j 0)
    (let* ((badness (ekp--compute-badness (- line-pixel ideal-pixel) 1))
           (fitness 1)
           (penalty (if end-with-hyphenp ekp-hyphen-penalty 0))
           (new-hyphen (if end-with-hyphenp 1 0))
           (dem (ekp--compute-demerits badness penalty prev-fitness fitness
                                       end-with-hyphenp prev-hyphen-count)))
      (list dem nil fitness new-hyphen)))
   (is-last
    (let* ((fill-ratio (/ (float ideal-pixel) line-pixel))
           (badness (if (< fill-ratio ekp-last-line-min-ratio)
                        (* ekp-last-line-short-penalty (- 1.0 fill-ratio))
                      0))
           (dem (expt (+ ekp-line-penalty badness) 2)))
      (list dem nil 1 0)))
   (t
    (let* ((result (ekp--line-badness-and-fitness
                    para ideal-pixel line-pixel
                    (seq-subseq glues-types i k)))
           (badness (plist-get result :badness))
           (fitness (plist-get result :fitness))
           (line-gaps (plist-get result :gaps))
           (penalty (if end-with-hyphenp ekp-hyphen-penalty 0))
           (new-hyphen (if end-with-hyphenp (1+ prev-hyphen-count) 0))
           (dem (ekp--compute-demerits badness penalty prev-fitness fitness
                                       end-with-hyphenp prev-hyphen-count)))
      (list dem line-gaps fitness new-hyphen)))))

(defun ekp--dp-trace-breaks (backptrs n)
  (let ((breaks (list n)) (index n))
    (while (> index 0)
      (let ((prev (aref backptrs index)))
        (if prev
            (progn (push prev breaks) (setq index prev))
          (setq index (1- index)))))
    (cdr breaks)))

(defun ekp--dp-trace-breaks-with-looseness (backptrs line-counts n target-lines
                                                      &optional alt-paths)
  (if (or (= ekp-looseness 0) (null alt-paths))
      (ekp--dp-trace-breaks backptrs n)
    (let* ((optimal-lines (aref line-counts n))
           (target (+ optimal-lines ekp-looseness))
           (best-path nil) (best-diff most-positive-fixnum))
      (maphash
       (lambda (key value)
         (when (= (car key) n)
           (let* ((line-count (cdr key))
                  (diff (abs (- line-count target))))
             (when (< diff best-diff)
               (setq best-diff diff)
               (setq best-path (cons line-count (car value)))))))
       alt-paths)
      (if best-path
          (ekp--dp-trace-alt-path alt-paths n (car best-path))
        (ekp--dp-trace-breaks backptrs n)))))

(defun ekp--dp-trace-alt-path (alt-paths n target-lines)
  (let ((breaks (list n)) (index n) (lines target-lines)
        (max-iterations (* n 2)))
    (while (and (> index 0) (> max-iterations 0))
      (let* ((key (cons index lines))
             (entry (gethash key alt-paths)))
        (if entry
            (let ((prev (car entry)))
              (when (> prev 0) (push prev breaks))
              (setq index prev)
              (cl-decf lines))
          (setq index 0)))
      (cl-decf max-iterations))
    (cdr breaks)))

;;; ============================================================
;;; deno-bridge Integration
;;; ============================================================

(defvar ekp--ts-result nil
  "Holds result from TypeScript DP computation.")

(defvar ekp--ts-batch-result nil
  "Holds batch result from TypeScript DP computation.")

(defun ekp--deno-bridge-ts-path ()
  "Return path to ekp.ts."
  (expand-file-name "ekp.ts" (ekp-root-dir)))

(defun ekp--start-deno-bridge ()
  "Start ekp-bridge for ekp."
  (when (and (featurep 'ekp-bridge)
             (featurep 'websocket))
    (let ((ts-path (ekp--deno-bridge-ts-path)))
      (when (file-exists-p ts-path)
        (condition-case err
            (progn
              (ekp-bridge-start "ekp" ts-path)
              (setq ekp--deno-bridge-ready t)
              (message "[EKP] deno-bridge started"))
          (error
           (message "[EKP] Failed to start deno-bridge: %s" err)
           (setq ekp--deno-bridge-ready nil)))))))

(defun ekp--bridge-call (func-name &rest args)
  "Call TypeScript function via ekp-bridge."
  (when ekp--deno-bridge-ready
    (condition-case nil
        (progn
          (ekp-bridge-call "ekp" func-name args)
          t)
      (error nil))))

(defun ekp--dp-cache-via-bridge (para string line-pixel)
  "Compute breaks using deno-bridge with Elisp's pre-computed arrays."
  (let* ((ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (glues-types (ekp-para-glues-types para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (n (length (ekp-para-boxes para)))
         (glue-ideals (make-vector n 0))
         (glue-shrinks (make-vector n 0))
         (glue-stretches (make-vector n 0)))
    (dotimes (i n)
      (let ((type (aref glues-types i)))
        (aset glue-ideals i (ekp--para-glue-ideal para type))
        (aset glue-shrinks i (ekp--para-glue-shrink para type))
        (aset glue-stretches i (ekp--para-glue-stretch para type))))
    ;; Send to TypeScript
    (setq ekp--ts-result nil)
    (when (ekp--bridge-call
           "break-lines"
           (append ideal-prefixs nil)
           (append min-prefixs nil)
           (append max-prefixs nil)
           (append glue-ideals nil)
           (append glue-shrinks nil)
           (append glue-stretches nil)
           (append hyphen-positions nil)
           hyphen-pixel n line-pixel)
      ;; Wait for result (set by TypeScript via evalInEmacs)
      ;; The result is set synchronously by the bridge message handler
      (when ekp--ts-result
        (let* ((breaks (car ekp--ts-result))
               (cost (cdr ekp--ts-result))
               (start 0) lines-rests lines-gaps)
          (dolist (end breaks)
            (let* ((leading-glue-type (aref glues-types start))
                   (end-with-hyphenp (ekp--hyphenate-p hyphen-positions (1- end)))
                   (ideal-pixel (- (aref ideal-prefixs end)
                                   (aref ideal-prefixs start)
                                   (ekp--para-glue-ideal para leading-glue-type))))
              (when end-with-hyphenp (cl-incf ideal-pixel hyphen-pixel))
              (push (- line-pixel ideal-pixel) lines-rests)
              (push (ekp--gaps-list
                     (seq-drop (cl-subseq glues-types start end) 1))
                    lines-gaps)
              (setq start end)))
          (let ((dp-result (list :rests (nreverse lines-rests)
                                 :gaps (nreverse lines-gaps)
                                 :breaks breaks
                                 :cost cost
                                 :line-count (length breaks))))
            (puthash line-pixel dp-result (ekp-para-dp-cache para))
            dp-result))))))

;;; ============================================================
;;; DP Cache: Bridge → Pure Elisp Fallback
;;; ============================================================

(defun ekp--dp-store-cache (string line-pixel dp-result)
  (let ((para (ekp--get-para string)))
    (puthash line-pixel dp-result (ekp-para-dp-cache para))))

(defun ekp--dp-get-cached (para line-pixel)
  (gethash line-pixel (ekp-para-dp-cache para)))

(defun ekp-dp-cache (string line-pixel)
  "Compute optimal line breaks for STRING at LINE-PIXEL width.
If deno-bridge is available, uses it. Otherwise falls back to pure Elisp."
  (let* ((para (ekp--get-para string))
         (cached (ekp--dp-get-cached para line-pixel)))
    (if cached
        cached
      (if (and ekp-use-deno-bridge
               ekp--deno-bridge-ready)
          (or (ekp--dp-cache-via-bridge para string line-pixel)
              (ekp--dp-cache-elisp para string line-pixel))
        (ekp--dp-cache-elisp para string line-pixel)))))

(defun ekp--dp-cache-elisp (para string line-pixel)
  "Pure Elisp DP implementation."
  (ignore string)
  (let* ((glues-types (ekp-para-glues-types para))
         (boxes (ekp-para-boxes para))
         (hyphen-pixel (ekp-para-hyphen-pixel para))
         (hyphen-positions (ekp-para-hyphen-positions para))
         (flagged-positions (ekp-para-flagged-positions para))
         (n (length boxes))
         (ideal-prefixs (ekp-para-ideal-prefixs para))
         (min-prefixs (ekp-para-min-prefixs para))
         (max-prefixs (ekp-para-max-prefixs para))
         (arrays (ekp--dp-init-arrays n))
         (backptrs (nth 0 arrays))
         (demerits (nth 1 arrays))
         (rests (nth 2 arrays))
         (gaps (nth 3 arrays))
         (hyphen-counts (nth 4 arrays))
         (fitness-classes (nth 5 arrays))
         (line-counts (nth 6 arrays))
         (alt-paths (nth 7 arrays))
         (best-end-demerits nil))
    (dotimes (i (1+ n))
      (when (aref demerits i)
        (let ((should-process
               (or (<= ekp-threshold-factor 0)
                   (null best-end-demerits)
                   (<= (aref demerits i)
                       (* best-end-demerits (1+ ekp-threshold-factor))))))
          (when should-process
            (let ((prev-hyphen-count (aref hyphen-counts i))
                  (prev-fitness (aref fitness-classes i))
                  (prev-line-count (aref line-counts i)))
              (catch 'break
                (dotimes (j (- n i))
                  (let* ((k (+ i j 1))
                         (is-last (= k n))
                         (end-with-hyphenp
                          (ekp--hyphenate-p hyphen-positions (1- k)))
                         (end-with-flaggedp
                          (ekp--flagged-p flagged-positions (1- k)))
                         (metrics (ekp--dp-line-metrics
                                   para i k glues-types
                                   ideal-prefixs min-prefixs max-prefixs))
                         (ideal-pixel (nth 0 metrics))
                         (min-pixel (nth 1 metrics))
                         (max-pixel (nth 2 metrics)))
                    (when end-with-hyphenp
                      (cl-incf ideal-pixel hyphen-pixel)
                      (cl-incf max-pixel hyphen-pixel)
                      (cl-incf min-pixel hyphen-pixel))
                    (when (and (not end-with-flaggedp)
                               (or (> min-pixel line-pixel)
                                   (and is-last (> ideal-pixel line-pixel))))
                      (when (null (aref demerits (1- k)))
                        (ekp--dp-force-break
                         para i k arrays glues-types hyphen-positions
                         ideal-prefixs hyphen-pixel line-pixel))
                      (throw 'break nil))
                    (when (or end-with-flaggedp
                              (<= min-pixel line-pixel max-pixel)
                              (and is-last (<= ideal-pixel line-pixel)))
                      (pcase-let ((`(,dem ,line-gaps ,fitness ,new-hyphen)
                                   (ekp--dp-compute-line-demerits
                                    para j is-last end-with-hyphenp
                                    ideal-pixel line-pixel glues-types i k
                                    prev-hyphen-count prev-fitness
                                    end-with-flaggedp)))
                        (let ((total-dem (+ (aref demerits i) dem))
                              (new-line-count (1+ prev-line-count)))
                          (when (or (null (aref demerits k))
                                    (< total-dem (aref demerits k)))
                            (aset rests k (- line-pixel ideal-pixel))
                            (aset gaps k line-gaps)
                            (aset demerits k total-dem)
                            (aset backptrs k i)
                            (aset fitness-classes k fitness)
                            (aset hyphen-counts k new-hyphen)
                            (aset line-counts k new-line-count)
                            (when (= k n)
                              (when (or (null best-end-demerits)
                                        (< total-dem best-end-demerits))
                                (setq best-end-demerits total-dem))))
                          (when alt-paths
                            (let* ((key (cons k new-line-count))
                                   (existing (gethash key alt-paths)))
                              (when (or (null existing)
                                        (< total-dem (cdr existing)))
                                (puthash key (cons i total-dem) alt-paths)))))))))))))))
    (let* ((breaks (ekp--dp-trace-breaks-with-looseness
                    backptrs line-counts n (aref line-counts n) alt-paths))
           (lines-rests (mapcar (lambda (i) (aref rests i)) breaks))
           (lines-gaps (mapcar (lambda (i) (aref gaps i)) breaks))
           (dp-result (list :rests lines-rests
                            :gaps lines-gaps
                            :breaks breaks
                            :cost (aref demerits n)
                            :line-count (aref line-counts n))))
      (puthash line-pixel dp-result (ekp-para-dp-cache para))
      dp-result)))

(defun ekp-dp-data (string line-pixel &optional key)
  (let ((data (ekp-dp-cache string line-pixel)))
    (if key (plist-get data key) data)))

(defun ekp-total-cost (string line-pixel)
  (ekp-dp-data string line-pixel :cost))

(defun ekp-line-breaks (string line-pixel)
  (ekp-dp-data string line-pixel :breaks))

;;; ============================================================
;;; Line Glue Distribution
;;; ============================================================

(defun ekp--distribute-gap-adjustment (para rest-pixel gaps-list stretch-p)
  (let* ((params (ekp-para-glue-params para))
         (latin-gaps (nth 0 gaps-list))
         (mix-gaps (nth 1 gaps-list))
         (cjk-gaps (nth 2 gaps-list))
         (remaining rest-pixel)
         (latin-change (if stretch-p
                           (plist-get params :lws-stretch)
                         (plist-get params :lws-shrink)))
         (mix-change (if stretch-p
                         (plist-get params :mws-stretch)
                       (plist-get params :mws-shrink)))
         (cjk-change (if stretch-p (plist-get params :cws-stretch) 0))
         (latin-adj 0) (latin-extra 0)
         (mix-adj 0) (mix-extra 0)
         (cjk-adj 0) (cjk-extra 0))
    (let ((latin-capacity (* latin-gaps latin-change)))
      (if (< remaining latin-capacity)
          (when (> latin-gaps 0)
            (setq latin-adj (/ remaining latin-gaps))
            (setq latin-extra (% remaining latin-gaps))
            (setq remaining 0))
        (setq latin-adj latin-change)
        (setq remaining (- remaining latin-capacity))))
    (when (> remaining 0)
      (let ((mix-capacity (* mix-gaps mix-change)))
        (if (< remaining mix-capacity)
            (when (> mix-gaps 0)
              (setq mix-adj (/ remaining mix-gaps))
              (setq mix-extra (% remaining mix-gaps))
              (setq remaining 0))
          (setq mix-adj mix-change)
          (setq remaining (- remaining mix-capacity)))))
    (when (and (> remaining 0) (> cjk-gaps 0))
      (setq cjk-adj (/ remaining cjk-gaps))
      (setq cjk-extra (% remaining cjk-gaps)))
    (list (cons latin-adj latin-extra)
          (cons mix-adj mix-extra)
          (cons cjk-adj cjk-extra))))

(defun ekp--compute-glue-pixels (para glues-types gaps-distribution stretch-p)
  (let ((latin-adj (car (nth 0 gaps-distribution)))
        (latin-extra (cdr (nth 0 gaps-distribution)))
        (mix-adj (car (nth 1 gaps-distribution)))
        (mix-extra (cdr (nth 1 gaps-distribution)))
        (cjk-adj (car (nth 2 gaps-distribution)))
        (cjk-extra (cdr (nth 2 gaps-distribution)))
        (latin-idx -1) (mix-idx -1) (cjk-idx -1))
    (mapcar
     (lambda (type)
       (let* ((base (ekp--para-glue-ideal para type))
              (adj (pcase type
                     ('lws (cl-incf latin-idx)
                           (+ latin-adj (if (< latin-idx latin-extra) 1 0)))
                     ('mws (cl-incf mix-idx)
                           (+ mix-adj (if (< mix-idx mix-extra) 1 0)))
                     ('cws (cl-incf cjk-idx)
                           (+ cjk-adj (if (< cjk-idx cjk-extra) 1 0)))
                     ('nws 0) (_ 0))))
         (if stretch-p (+ base adj) (- base adj))))
     glues-types)))

(defun ekp--line-glue-single-box (line-pixel box-width hyphen-p hyphen-pixel)
  (let ((trailing (- line-pixel box-width (if hyphen-p hyphen-pixel 0))))
    (list 0 trailing)))

(defun ekp--line-glue-last-line (para glues-types ideal-pixel line-pixel)
  (append '(0)
          (mapcar (lambda (type) (ekp--para-glue-ideal para type)) glues-types)
          (list (- line-pixel ideal-pixel))))

(defun ekp--line-glue-normal (para glues-types rest-pixel gaps-list)
  (if (= rest-pixel 0)
      (append '(0) (mapcar (lambda (type) (ekp--para-glue-ideal para type)) glues-types) '(0))
    (let* ((stretch-p (> rest-pixel 0))
           (distribution (ekp--distribute-gap-adjustment
                          para (abs rest-pixel) gaps-list stretch-p))
           (glue-pixels (ekp--compute-glue-pixels para glues-types distribution stretch-p)))
      (append '(0) glue-pixels '(0)))))

(defun ekp-line-glues (string line-pixel)
  "Compute glue pixels for each line after breaking STRING at LINE-PIXEL."
  (let* ((para (ekp--get-para string))
         (boxes-widths (ekp--boxes-widths string))
         (boxes-num (length (ekp--boxes string)))
         (glues-types (ekp--glues-types string))
         (hyphen-positions (ekp--hyphen-positions string))
         (ideal-prefixs (ekp--ideal-prefixs string))
         (max-prefixs (ekp--max-prefixs string))
         (breaks (ekp-line-breaks string line-pixel))
         (lines-rests (ekp-dp-data string line-pixel :rests))
         (lines-gaps (ekp-dp-data string line-pixel :gaps))
         (hyphen-pixel (ekp--hyphen-pixel string))
         (line-glues (make-vector (length breaks) nil))
         (start 0))
    (dotimes (i (length breaks))
      (let* ((end (nth i breaks))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-types (seq-drop (cl-subseq glues-types start end) 1))
             (is-last (>= end boxes-num))
             (hyphen-p (ekp--hyphenate-p hyphen-positions (1- end)))
             (ideal-pixel (- (aref ideal-prefixs end)
                             (aref ideal-prefixs start)
                             (ekp--para-glue-ideal para (aref glues-types start))))
             (max-pixel (+ (- (aref max-prefixs end)
                              (aref max-prefixs start)
                              (ekp--para-glue-max para (aref glues-types start)))
                           (if hyphen-p hyphen-pixel 0)))
             glue-list)
        (setq glue-list
              (cond
               ((= 1 (length line-boxes-widths))
                (ekp--line-glue-single-box line-pixel
                                           (aref line-boxes-widths 0)
                                           hyphen-p hyphen-pixel))
               (is-last
                (ekp--line-glue-last-line
                 para line-glues-types ideal-pixel line-pixel))
               ((< max-pixel line-pixel)
                (append '(0)
                        (mapcar (lambda (type)
                                  (ekp--para-glue-max para type))
                                line-glues-types)
                        (list (- line-pixel max-pixel))))
               (t
                (ekp--line-glue-normal para line-glues-types
                                       (nth i lines-rests)
                                       (nth i lines-gaps)))))
        (aset line-glues i (vconcat glue-list))
        (setq start end)))
    line-glues))

;;; ============================================================
;;; Final Assembly
;;; ============================================================

(defun ekp--box-space-p (box)
  (and box (not (string-empty-p box))
       (or (string-blank-p box) (= (string-width box) 0))))

(defun ekp--interleave (list1 list2)
  (let (result)
    (while (or list1 list2)
      (when list1 (push (pop list1) result))
      (when list2 (push (pop list2) result)))
    (nreverse result)))

(defun ekp--combine-glues-and-boxes (glues boxes)
  (let* ((glues (append glues nil))
         (last-glue (car (last glues)))
         (glues (butlast glues))
         (boxes (append boxes nil)))
    (if (= (length glues) (length boxes))
        (string-join (append (ekp--interleave glues boxes)
                             (list last-glue)))
      (error "Glues count (%d) must equal boxes count (%d) + 1"
             (1+ (length glues)) (length boxes)))))

(defun ekp--pixel-spacing-width (spacing)
  (if (string-empty-p spacing)
      0
    (let ((display (get-text-property 0 'display spacing)))
      (if (and display (eq (car display) 'space))
          (let ((width-spec (plist-get (cdr display) :width)))
            (if (listp width-spec) (car width-spec) (or width-spec 0)))
        0))))

(defun ekp--redistribute-extra-width (glues extra-width)
  (when (and glues (> extra-width 0))
    (let* ((inner-glues (butlast (cdr glues)))
           (n (length inner-glues)))
      (if (= n 0)
          (let* ((trailing (car (last glues)))
                 (old-width (ekp--pixel-spacing-width trailing))
                 (new-width (+ old-width extra-width)))
            (setf (car (last glues)) (ekp-pixel-spacing new-width)))
        (let ((per-glue (/ extra-width n))
              (remainder (% extra-width n))
              (idx 0))
          (setq glues
                (cons (car glues)
                      (append
                       (mapcar
                        (lambda (g)
                          (let* ((old-w (ekp--pixel-spacing-width g))
                                 (extra (+ per-glue (if (< idx remainder) 1 0)))
                                 (new-w (+ old-w extra)))
                            (cl-incf idx)
                            (ekp-pixel-spacing new-w)))
                        inner-glues)
                       (last glues))))))))
  glues)

(defun ekp--strip-line-spaces (line-boxes line-glues line-boxes-widths
                                &optional strip-leading strip-trailing)
  (let* ((boxes (append line-boxes nil))
         (glues (append line-glues nil))
         (widths (append line-boxes-widths nil))
         (removed-width 0))
    (when (> (length boxes) 0)
      (when strip-trailing
        (while (and boxes (ekp--box-space-p (car (last boxes))))
          (cl-incf removed-width (car (last widths)))
          (setq boxes (butlast boxes))
          (setq widths (butlast widths))
          (when (> (length glues) 1)
            (setq glues (append (butlast (butlast glues)) (last glues))))))
      (when strip-leading
        (while (and boxes (ekp--box-space-p (car boxes)))
          (cl-incf removed-width (car widths))
          (setq boxes (cdr boxes))
          (setq widths (cdr widths))
          (when (> (length glues) 1)
            (setq glues (cons (car glues) (cddr glues)))))))
    (when (> removed-width 0)
      (setq glues (ekp--redistribute-extra-width glues removed-width)))
    (cons (vconcat boxes) glues)))

;;; ============================================================
;;; Public API
;;; ============================================================

(defun ekp--pixel-justify (string line-pixel)
  "Justify single STRING to LINE-PIXEL."
  (let* ((boxes (ekp--boxes string))
         (boxes-widths (ekp--boxes-widths string))
         (hyphen (ekp--hyphen-str string))
         (breaks (ekp-line-breaks string line-pixel))
         (num (length breaks))
         (lines-glues (ekp-line-glues string line-pixel))
         (hyphen-positions (ekp--hyphen-positions string))
         (start 0) strings)
    (dotimes (i num)
      (let* ((end (nth i breaks))
             (line-boxes (cl-subseq boxes start end))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-raw (mapcar #'ekp-pixel-spacing
                                     (aref lines-glues i)))
             (is-first-line (= i 0))
             (stripped (ekp--strip-line-spaces line-boxes line-glues-raw
                                               line-boxes-widths
                                               (not is-first-line) t))
             (line-boxes (car stripped))
             (line-glues (cdr stripped))
             (last-box-idx (1- end))
             (need-hyphen
              (and (< i (1- num))
                   (ekp--hyphenate-p hyphen-positions last-box-idx))))
        (when (and need-hyphen (> (length line-boxes) 0))
          (setf (aref line-boxes (1- (length line-boxes)))
                (concat (aref line-boxes (1- (length line-boxes))) hyphen)))
        (when (> (length line-boxes) 0)
          (push (ekp--combine-glues-and-boxes line-glues line-boxes)
                strings))
        (setq start end)))
    (mapconcat 'identity (nreverse strings) "\n")))

(defun ekp-pixel-justify (string line-pixel)
  "Justify multiline STRING to LINE-PIXEL."
  (let* ((strs (split-string string "\n"))
         (non-blank-strs (cl-remove-if #'string-blank-p strs)))
    (mapconcat (lambda (str)
                 (if (string-blank-p str)
                     ""
                   (ekp--pixel-justify str line-pixel)))
               strs "\n")))

;;; ============================================================
;;; Optimal Width Search
;;; ============================================================

(defun ekp--compute-avg-cost (strings pixel)
  (let ((total-cost 0) (count 0))
    (dolist (s strings)
      (unless (string-blank-p s)
        (cl-incf total-cost (abs (ekp-total-cost s pixel)))
        (cl-incf count)))
    (if (> count 0)
        (/ (float total-cost) count)
      most-positive-fixnum)))

(defun ekp--ternary-search-optimal-width (strings min-pixel max-pixel)
  (let ((lo min-pixel) (hi max-pixel))
    (while (> (- hi lo) 2)
      (let* ((mid1 (+ lo (/ (- hi lo) 3)))
             (mid2 (- hi (/ (- hi lo) 3)))
             (cost1 (ekp--compute-avg-cost strings mid1))
             (cost2 (ekp--compute-avg-cost strings mid2)))
        (if (< cost1 cost2)
            (setq hi mid2)
          (setq lo mid1))))
    (let ((best-pixel lo)
          (best-cost (ekp--compute-avg-cost strings lo)))
      (dolist (p (list (1+ lo) hi))
        (when (<= p max-pixel)
          (let ((cost (ekp--compute-avg-cost strings p)))
            (when (< cost best-cost)
              (setq best-cost cost best-pixel p)))))
      best-pixel)))

(defun ekp-pixel-range-justify (string min-pixel max-pixel)
  "Find optimal width for STRING between MIN-PIXEL and MAX-PIXEL.
Returns (justified-text . optimal-pixel)."
  (let* ((strings (split-string string "\n"))
         (_ (dolist (s strings)
              (unless (string-blank-p s)
                (ekp--get-para s))))
         (best-pixel (ekp--ternary-search-optimal-width
                      strings min-pixel max-pixel)))
    (cons (ekp-pixel-justify string best-pixel) best-pixel)))

;;; ============================================================
;;; Initialization
;;; ============================================================

;; (defun ekp--load-dicts ()
;;   "Load hyphenation dictionaries."
;;   (ekp-hyphen-load-languages
;;    (expand-file-name "dictionaries" (ekp-root-dir))))
;;
;; ;; Load dictionaries on require
;; (ekp--load-dicts)

;; Start deno-bridge if available
;; (ekp--start-deno-bridge)

(provide 'ekp)

;;; ekp.el ends here
