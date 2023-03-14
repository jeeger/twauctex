;;; twauctex.el --- Tweaked auctex to write one-sentence-per-line LaTeX files  -*- lexical-binding: t; -*-
;;;
;; Author: Jan Seeger <jan.seeger@thenybble.de>
;; Keywords: languages, convenience
;; Homepage: https://github.com/jeeger/twauctex
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (auctex "12.2.3"))
;; 
;;; Commentary:
;;;
;;; This file activates some tweaks for AUCtex that makes
;;; it easier to write version-controlled LaTeX files
;;; (one-sentence-per-line mode), supresses common mistakes (unescaped
;;; underscore in non-math-mode), and helps with alignment of table
;;; and TikZ environments (supress auto-fill in tables and TikZ pictures,
;;; command to align table columns).
;;;
;;; Code:

(require 'tex)
(require 'latex)
(require 'cl-lib)
(require 'tex-site)
(require 'texmathp)
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defconst twauctex-version (shell-command-to-string "git rev-parse --short master"))

(defgroup twauctex nil "Customization for twauctex" :group 'LaTeX)

(defcustom twauctex-electric-environments '("document" "abstract" "frame")
  "In which environments the space key should be electric."
  :type '(repeat string) :group 'twauctex :safe 'listp)

(defcustom twauctex-inhibited-electric-macros '("url" "texttt" "cite" "todo")
  "For which TeX macros the electric space should be disabled."
  :type '(repeat string) :group 'twauctex :safe 'listp)

(defcustom twauctex-inhibited-auto-fill-environments '("tabular" "tikzpicture" "todo")
  "For which LaTeX environments not to run fill."
  :type '(repeat string) :group 'twauctex
  :safe 'listp)

(defcustom twauctex-inhibited-quote-environments '("tikzpicture")
  "In which LaTeX environments the auctex quote functionality should be disabled."
  :type '(repeat string) :group 'twauctex
  :safe 'listp)

(defcustom twauctex-extra-electric-regexes '()
  "Which regexes should end a sentence, in addition to the value of the `sentence-end' function.

Be careful to correctly escape this!" :type '(repeat string) :group 'twauctex)

(defcustom twauctex-table-environments '("align" "tabular" "matrix" "bmatrix")
  "Which environment should we treat as a table? This means that the ampersand is not escaped, and that `twauctex-edit-table' works for editing this environment."
  :type '(repeat string) :group 'twauctex
  :safe 'stringp)

(defcustom twauctex-insert-sentence-spacing t "Whether twauctex should automatically insert an intersentence spacing macro before a dot at sentence end." :type 'boolean :group 'twauctex :safe 'booleanp)

(defcustom twauctex-insert-word-spacing t "Whether twauctex should automatically insert an interword spacing macro before a dot when an abbreviation is detected." :type 'boolean :group 'twauctex :safe 'booleanp)

(defcustom twauctex-non-break-abbrevs '("et al."
                                        "PhD."
                                        "etc."
                                        "M.Sc."
                                        "B.Sc."
                                        "e.g."
                                        "i.e."
                                        "vs.")
  "A number of case sensitive abbreviations with dots that should not cause the sentence (and thus the line) to end." :type '(repeat string) :group 'twauctex :safe 'listp)

(defcustom twauctex-max-lookback 1 "How far twauctex should look backwards to try and match a sentence ending. Calculate this from the longest possible match to `twauctex-extra-electric-regexes'." :type 'integer)

(defcustom twauctex-use-visual-fill-column t "Whether twauctex should automatically enable `visual-fill-column' mode when enabled. Using one-sentence-per-line mode without some sort of visual line breaking is not recommended, as the long lines are very hard to read. Alternatives to `visual-fill-column-mode' exist, so we provide this as an option should you wish to use another visual line breaking mode." :type 'boolean :group 'twauctex :safe 'booleanp)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweak functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Aligning tables
(defun twauctex-align-table (arg)
  "Align the current environment as a table. If ARG is passed, collapse the table before aligning."
  (interactive "p")
  (when (> arg 1)
    (twauctex-collapse-table))
  (save-mark-and-excursion
    (LaTeX-mark-environment)
    (align-regexp (region-beginning) (region-end) (rx (and (group (+? space)) (or ?& "\\\\"))) 1 1 t)))

(defun twauctex-collapse-table ()
  "Collapse the table by deleting multiple spaces before and after the column identifier."
  (interactive)
  (save-mark-and-excursion
    (LaTeX-mark-environment)
    (while (re-search-forward (rx (and (* space) (group (or ?& "\\\\")) (group (? space)) (* space))) (region-end) t)
      (replace-match " \\1\\2"))))

;; TODO: Define a function to easily save and restore these variables.
(defvar twauctex--old-fill-column nil "Stores old value of `fill-column' when editing a table.")
(defvar twauctex--old-visual-fill-column-mode nil "Stores the old variable `visual-fill-column-mode' when editing a table.")
(defvar twauctex--old-auto-fill-function nil "Stores the old variable `auto-fill-function' when editing a table.")
(defvar twauctex--in-edit-table nil "Stores whether we are currently in edit-table mode.")
(defvar twauctex--old-buffer-face-mode-face nil "Stores whether a custom face was enabled before we entered edit-table mode.")
(defvar twauctex--old-buffer-truncate-lines nil "Stores whether `truncate-lines' was enabled before we entered edit-table mode.")

;; TODO: I should define an extra keymap for these keys, so they can be remapped.
(defun twauctex-edit-table (&optional supress-autoalign)
  "Edit a table.
First, unset the fill column, disable visual fill column mode,
turn off any custom face and narrow buffer to the table. Then,
align it, unless SUPRESS-AUTOALIGN is provided. Now edit the
table, and optionally realign. Finally, exit the table mode
with C-c C-c."
  ;; We can't use KEYMAP here because the key is only bound in
  ;; the minor mode.
  (interactive "P")
  (unless (member (LaTeX-current-environment) twauctex-table-environments)
    (error "Only call in a table environment"))
  (setq-local twauctex--old-fill-column fill-column)
  (setq-local twauctex--old-visual-fill-column-mode visual-fill-column-mode)
  (setq-local twauctex--old-auto-fill-function auto-fill-function)
  (setq fill-column -1)
  (setq auto-fill-function nil)
  (when twauctex-use-visual-fill-column
    (visual-fill-column-mode -1))
  (when buffer-face-mode-face
    (setq-local twauctex--old-buffer-face-mode-face buffer-face-mode-face)
    (buffer-face-toggle))
  (unless truncate-lines
    (setq-local twauctex--old-buffer-truncate-lines truncate-lines)
    (toggle-truncate-lines))
  (unless supress-autoalign
    (twauctex-align-table 0))
  (setq-local twauctex--in-edit-table t)
  (save-mark-and-excursion
    (LaTeX-mark-environment)
    (narrow-to-region (region-beginning) (region-end)))
  (define-key twauctex-mode-map (kbd "C-c C-c") #'twauctex-exit-edit-table)
  (define-key twauctex-mode-map (kbd "C-c C-a") #'twauctex-align-table))

(defmacro twauctex--kill-local-variables (&rest variables)
  "Kill all local variables in VARIABLES."
  `(progn ,@(mapcar (lambda (var) `(kill-local-variable (quote ,var))) variables)))

(defun twauctex-exit-edit-table (&optional supress-collapse)
  "Exit table edit mode.
Restore all old variables, collapse the table and widen the buffer unless SUPRESS-COLLAPSE is provided."
  (interactive "P")
  (unless twauctex--in-edit-table
    (error "Must be in edit table mode to call this"))
  (when twauctex--old-fill-column (setq fill-column twauctex--old-fill-column))
  (when (and twauctex-use-visual-fill-column twauctex--old-visual-fill-column-mode)
    (visual-fill-column-mode twauctex--old-visual-fill-column-mode))
  (when twauctex--old-auto-fill-function (setq auto-fill-function twauctex--old-auto-fill-function))
  (when twauctex--old-buffer-face-mode-face (buffer-face-toggle twauctex--old-buffer-face-mode-face))
  (unless twauctex--old-buffer-truncate-lines (toggle-truncate-lines twauctex--old-buffer-truncate-lines))
  (twauctex--kill-local-variables twauctex--old-fill-column
                                  twauctex--old-visual-fill-column-mode
                                  twauctex--old-auto-fill-function
                                  twauctex--old-buffer-face-mode-face
                                  twauctex--old-buffer-truncate-lines)
  (define-key twauctex-mode-map (kbd "C-c C-c") nil)
  (define-key twauctex-mode-map (kbd "C-c C-a") nil)
  (widen)
  (unless supress-collapse
    (twauctex-collapse-table)))

;; Use correct escaping for underscore
(defun twauctex-underscore-maybe (arg)
  "Insert an underscore. If we are in math mode, or ARG is given, an unescaped underscore is inserted, otherwise, an escaped underscore is inserted. When pressed again, the escaped underscore is replaced with an unescaped one, or vice versa in math mode."
  (interactive "p")
  (let ((repeated (eq last-command 'twauctex-underscore-maybe))
        (in-comment (TeX-in-comment))
        (in-math (texmathp))
        (has-arg (> arg 1)))
    (cond
     ((and (or repeated has-arg) (not in-math))
      (delete-char -2)
      (self-insert-command 1))
     ((and (or repeated has-arg) in-math)
      (delete-char -1)
      (insert "\\_"))
     ((and (not in-comment) (not in-math))
      (insert "\\_"))
     (t (insert "_")))))

;; Escape ampersand if not in table environment.
(defun twauctex-ampersand-maybe (arg)
  "Insert an ampersand. If we are not in a table environment, escape it.
Repeating the command or passing ARG forces insertion
of an ampersand."
  (interactive "p")
  (if (eq last-command 'twauctex-ampersand-maybe)
      (progn
	(delete-char -2)
	(self-insert-command 1))
    (if (or (> arg 1) (member (LaTeX-current-environment) twauctex-table-environments))
	(self-insert-command 1)
      (insert "\\&"))))

(defun twauctex-quote-maybe (arg)
  "Call auctex's TeX-insert-quote if not in a supressed environment."
  (interactive "*P")
  (if (member (LaTeX-current-environment) twauctex-inhibited-quote-environments)
      (self-insert-command 1)
    (TeX-insert-quote arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSPL implementation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; internal variables, do not modify!

(defvar twauctex--abbrev-regexp nil "Optimized regex to detect abbreviations. Do not modify manually.")
(defvar twauctex--max-search-bound nil "Maximum length to search backwards for abbreviations. Length of longest abbreviation. Do not modify manually!")
(defvar twauctex--electric-regexp nil "Alternative of all sentence-ending regexes. Is initialized from `twauctex-extra-electric-regexes' and `sentence-end'. Do not modify manually!")

(defun twauctex--update-max-search-bound (symbol newval op where)
  "Update `twauctex--max-search-bound'  when configuration options are updated."
  (when (eq op 'set)
    (setq twauctex--max-search-bound (or (cl-loop for abbrev in newval maximize (+ (length abbrev) 2))
					 0))))

(defun twauctex--update-abbrev-regexp (symbol newval op where)
  "Update `twauctex--abbrev-regexp'  when configuration options are updated."
  (when (eq op 'set)
    (setq twauctex--abbrev-regexp
          (regexp-opt newval))))

;; Join all regexes into one.
(defun twauctex--update-electric-regexp (symbol newval op where)
  "Update `twauctex--electric-regexp'  when configuration options are updated."
  (when (eq op 'set)
    (setq twauctex--electric-regexp
          (s-concat "\\(?:" (s-join "\\|" (mapcar (lambda (regex) (concat "\\(?:" regex "\\)")) (cons (sentence-end) newval))) "\\)"))))

(add-variable-watcher 'twauctex-non-break-abbrevs #'twauctex--update-max-search-bound)
(add-variable-watcher 'twauctex-non-break-abbrevs #'twauctex--update-abbrev-regexp)
(add-variable-watcher 'twauctex-extra-electric-regexes #'twauctex--update-electric-regexp)

;; OSPL code

(defun twauctex-electric-sentence-end-space (arg)
  "Insert a new line if the last character was electric and we did not detect an abbreviation.

If ARG is given, insert a space without breaking the line. If the
command is repeated, delete the inserted line break.

Behavior only takes place in environments defined in
`twauctex-electric-environments' when not in a macro contained
in `twauctex-inhibited-electric-macros'."
  (interactive "p")
  (self-insert-command 1)
  (let* ((case-fold-search nil)
         (repeated (or (> arg 1) (eq last-command 'twauctex-electric-sentence-end-space)))
	 (in-environment (member (LaTeX-current-environment) twauctex-electric-environments))
	 (inhibited-macro (member (TeX-current-macro) twauctex-inhibited-electric-macros))
         (at-abbrev (twauctex-looking-at-abbrev))
         (at-electric (looking-back twauctex--electric-regexp twauctex-max-lookback))
         (at-lastupper (looking-back (concat "[[:upper:]]" twauctex--electric-regexp) (+ twauctex-max-lookback 1))))
    ;; Delete char we inserted to fool (sentence-end)
    (delete-char -1)
    (cond
     ((and repeated (bolp))
      (delete-char -1)
      (self-insert-command 1))
     ((and in-environment
           (not inhibited-macro)
           (not at-abbrev)
           at-electric
           (not (TeX-in-comment)))
      ;; Inter-sentence space when the last character before the sentence is a capital character.
      (when (and
             at-lastupper
             twauctex-insert-sentence-spacing)
        (backward-char 1)
        (insert "\\@")
        (forward-char 1))
      (newline))
     ;; Inter-word space when we are at an abbreviation.
     ((and at-abbrev
           at-electric
           twauctex-insert-word-spacing)
      (insert "\\ "))
     (t (self-insert-command 1)))))

(defun twauctex-looking-at-abbrev ()
  "Whether we are looking at an abbreviation in `twauctex-non-break-abbrevs'."
  ;; If we have to search backwards for longer than the longest abbrevation for a space, it's not an abbrevation.
  (looking-back
   (concat "\\<" twauctex--abbrev-regexp "\s?") (- (point) twauctex--max-search-bound)))

;; Modified version from http://www.cs.au.dk/~abizjak/emacs/2016/03/06/latex-fill-paragraph.html
(defun twauctex-fill-ospl (&optional P)
  "Unfill the paragraph into one-sentence-per-line format.

If P is provided, just call the regular fill function."
  (interactive "P")
  (when (and (not (member (LaTeX-current-environment) twauctex-inhibited-auto-fill-environments))
             (not (member (TeX-current-macro) twauctex-inhibited-electric-macros))
	     (not (and (bolp) (eolp))))
    (if (not P)
	(save-excursion
	  (let ((fill-column most-positive-fixnum) ;; relies on dynamic binding
                (sentence-end twauctex--electric-regexp))
	    (fill-paragraph)
	    (let ((end (save-excursion
			 (forward-paragraph 1)
			 (backward-sentence)
			 (point-marker))))  ;; remember where to stop
	      (beginning-of-line)
	      (while (progn (forward-sentence)
			    (<= (point) (marker-position end)))
		(unless (twauctex-looking-at-abbrev) ;; Don't break on common abbreviations.
		  (just-one-space) ;; leaves only one space, point is after it
		  (delete-char -1) ;; delete the space
		  (newline)        ;; and insert a newline
		  (LaTeX-indent-line)))))) ;; I only use this in combination with latex, so this makes sense
      ;; otherwise do ordinary fill paragraph
      (fill-paragraph P))))

(defun twauctex-latex-kill-sentence (&optional arg)
  "Kill the sentence.
If called with ARG, or already at end of line, kill the line instead."
  (interactive "p")
  (if (or (> arg 1) (not (member (LaTeX-current-environment) twauctex-electric-environments)))
      (kill-line)
    (if (eolp)
	(kill-line)
      (kill-sentence))))

(defun twauctex-dont-break-on-nbsp ()
  "Don't allow `LaTeX-mode' to break on a nonbreaking space."
  (and (eq major-mode 'latex-mode)
       (eq (char-before (- (point) 1)) ?\\)))

(defun twauctex--fill-column-hack ()
  (when twauctex-use-visual-fill-column
    (visual-fill-column-mode 1)))

;;;###autoload
(define-minor-mode twauctex-mode "Extend latex mode to make it easier to write one sentence per line. Makes sentence-end characters (.?!:) electric to insert a newline, and supresses spaces at the beginning of the line."
  :init-value nil
  :lighter " twauc"
  :keymap (list (cons (kbd "_") #'twauctex-underscore-maybe)
                (cons (kbd "\"") #'twauctex-quote-maybe)
                (cons (kbd "&") #'twauctex-ampersand-maybe)
                (cons (kbd "C-c e") #'twauctex-edit-table)
                (cons (kbd "M-q") #'twauctex-fill-ospl)
                (cons (kbd "SPC") #'twauctex-electric-sentence-end-space))
  (if twauctex-mode
      (progn
        ;; Activating
        (auto-fill-mode -1)
        (visual-line-mode 1)
        (set (make-local-variable 'fill-nobreak-predicate) #'twauctex-dont-break-on-nbsp)
        (twauctex--update-abbrev-regexp nil twauctex-non-break-abbrevs 'set nil)
        (twauctex--update-max-search-bound nil twauctex-non-break-abbrevs 'set nil)
        (twauctex--update-electric-regexp nil twauctex-extra-electric-regexes 'set nil)
        ;; We use hack-local-variables, because we want to take the
        ;; file-local fill column into account when setting the visual
        ;; fill column.
        (add-hook 'hack-local-variables-hook #'twauctex--fill-column-hack nil t ))
    (progn
      ;; Deactivating
      (visual-line-mode -1)
      (kill-local-variable 'fill-nobreak-predicate)
      (remove-hook 'hack-local-variables-hook #'twauctex--fill-column-hack))))

;;;###autoload
(defun twauctex-enable ()
  "Enable twauctex mode."
  (when (and (buffer-file-name) (equal (file-name-extension (buffer-file-name)) "tex"))
    (twauctex-mode 1)))

;;;###autoload
(defun twauctex-global-mode ()
  "Automatically turn on twauctex mode in all LaTeX buffers."
  (interactive)
  (cl-pushnew #'twauctex-enable LaTeX-mode-hook))

(provide 'twauctex)

;;; twauctex.el ends here
