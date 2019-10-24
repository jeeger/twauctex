;;; twauctex --- Tweaks for auctex to simplify writing latex. -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; This file activates some tweaks for AUCtex that makes
;;; it easier to write version-controlled LaTeX files
;;; (one-sentence-per-line mode), supresses common mistakes (unescaped
;;; underscore in non-math-mode), and helps with alignment of table
;;; and TikZ environments (supress auto-fill in tables and TikZ pictures,
;;; command to align table columns).

(require 'tex)
(require 'latex)
(require 'cl-lib)
(require 'tex-site)
(require 'texmathp)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup twauctex nil "Customization for twauctex" :group 'LaTeX)

(defcustom twauctex-electric-environments '("document" "abstract" "frame")
  "In which environments the period should be electric."
  :type '(repeat string) :group 'twauctex :safe 'listp)

(defcustom twauctex-inhibited-electric-macros '("url" "texttt" "cite")
  "For which TeX macros the electric period should be disabled."
  :type '(repeat string) :group 'twauctex :safe 'listp)

(defcustom twauctex-inhibited-auto-fill-environments '("tabular" "tikzpicture")
  "For which LaTeX environments not to run fill."
  :type '(repeat string) :group 'twauctex
  :safe 'listp)

(defcustom twauctex-electric-chars '(?\. ?\? ?\!) "Which sentence end characters should be electrified to start a new line." :type '(repeat character) :group 'twauctex)

(defcustom twauctex-table-environments '("align" "tabular" "matrix" "bmatrix")
  "In which environments should we not escape the ampersand?"
  :type '(repeat string) :group 'twauctex
  :safe 'stringp)

(defcustom twauctex-insert-sentence-spacing t "Whether twauctex should automatically insert an intersentence spacing macro before a dot at sentence end." :type 'boolean :group 'twauctex :safe 'booleanp)

(defcustom twauctex-insert-word-spacing t "Whether twauctex should automatically insert an interword spacing macro before a dot at one of the suppressed words." :type 'boolean :group 'twauctex :safe 'booleanp)

(defcustom twauctex-non-break-abbrevs '("et al."
				       "PhD."
				       "etc."
				       "M.Sc."
				       "B.Sc."
				       "e.g."
				       "vs.")
  "A number of abbreviations with dots that should not cause the sentence (and thus the line) to end. Must include final electric character." :type '(repeat string) :group 'twauctex :safe 'listp)


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweak functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Aligning tables
(defun twauctex-align-environment (arg)
  "Align the current environment as a table. If ARG is passed, collapse the table before aligning."
  (interactive "p")
  (when (> arg 1)
    (twauctex-collapse-table))
  (save-mark-and-excursion
    (LaTeX-mark-environment)
    (align (region-beginning) (region-end))))

(defun twauctex-collapse-table ()
  "Collapse the table by deleting multiple spaces after the column identifier."
  (interactive)
  (save-mark-and-excursion
    (LaTeX-mark-environment)
    ;; Avert your eyes lest you contract leaning toothpick syndrome.
    (while (re-search-forward "[[:space:]]+\\(&\\|\\\\\\\\\\)" (region-end) t)
      (replace-match " \\1"))))

;; Use correct escaping for underscore
(defun twauctex-underscore-maybe (arg)
  "Insert an underscore. Unless we are in math mode, or ARG is given, an escaped underscore is inserted, otherwise, an unescaped underscore is inserted."
  (interactive "p")
  (if (eq last-command 'twauctex-underscore-maybe)
      (progn
	(delete-char -2)
	(self-insert-command 1))
    (if (or (> arg 1)
	    (texmathp)
	    (member (TeX-current-macro) twauctex-inhibited-electric-macros)
	    (TeX-in-comment))
	(self-insert-command 1)
      (insert "\\_"))))

;; Escape ampersand if not in table environment.
(defun twauctex-ampersand-maybe (arg)
  "Insert an ampersand. If we are not in a table environment, escape it.
Repeating the command or passing ARG forces insertion
of an ampersand."
  (interactive "p")
  (if (eq last-command 'twauctex-ampersand-maybe)
      (progn
	(delete-char -1)
	(self-insert-command 1))
    (if (or (> arg 1) (member (LaTeX-current-environment) twauctex-table-environments))
	(self-insert-command 1)
      (insert "\\&"))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSPL implementation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; internal variables

(defvar twauctex--abbrev-regexp nil)
(defvar twauctex--max-search-bound nil)

(defun twauctex--update-max-search-bound (symbol newval op where)
  (when (eq op 'set)
    (setq twauctex--max-search-bound (or (cl-loop for abbrev in newval maximize (+ (length abbrev) 2))
					 0))))

(defun twauctex--update-abbrev-regexp (symbol newval op where)
  (when (eq op 'set)
    (let ((split-regexp (regexp-opt (mapcar (lambda (c) (char-to-string c)) twauctex-electric-chars))))
      ;; Split abbreviations containing internal punctuation (like "M.Sc.").
      ;; We also need to match prefixes (like "M" for M.Sc.), so we don't electric-break the line when typing the abbreviation.
      (setq twauctex--abbrev-regexp
	    (regexp-opt (apply #'append (mapcar (lambda (s) (split-string s split-regexp t)) newval)))))))
  

(add-variable-watcher 'twauctex-non-break-abbrevs 'twauctex--update-max-search-bound)
(add-variable-watcher 'twauctex-non-break-abbrevs 'twauctex--abbrev-regexp)

;; OSPL code

(defun twauctex-electric-sentence-end-char (arg)
"Insert the electric character and break for a new line.

If ARG is given, insert a character without breaking the line. If
the command is repeated, delete the inserted line break.

Behavior only takes place in environments defined in
`twauctex-electric-environments' when not in a macro contained
in `twauctex-inhibited-electric-macros'."
  (interactive "p")
  (let ((repeated (or (> arg 1) (eq last-command 'twauctex-electric-sentence-end-char)))
	(in-environment (member (LaTeX-current-environment) twauctex-electric-environments))
	(inhibited-macro (member (TeX-current-macro) twauctex-inhibited-electric-macros))
	(case-fold-search nil))
    (if (and in-environment (not inhibited-macro) (not (twauctex-looking-at-abbrev t)) (not (TeX-in-comment)))
	(progn
	  (when (and repeated (bolp))
	    (delete-char -2))			; Delete the newline and the dot before that.
	  (self-insert-command 1)
	  (when (and (equal (this-command-keys) ".")	; Automatically insert inter-sentence spacing in relevant contexts (i.e. capital letter before period.
		     (save-excursion
		       (backward-char 2)
		       (looking-at "[A-Z]"))
		     twauctex-insert-sentence-spacing)
	    (insert "\\@"))
	  (unless repeated
	    (newline)))
      (self-insert-command 1))))

(defun twauctex-electric-space (arg)
  "Ignore extraneous typed spaces.

When the command before this was an electric space and the line
was broken, supress the space after the sentence ending
character. If ARG is passed, insert a simple non-electric space."
  (interactive "p")
  (cond
   ((> arg 1) (self-insert-command 1))
   ((eq (char-before) ? ))
   ((and (bolp) (eq last-command 'twauctex-electric-sentence-end-char)))    ;; Do nothing
   ((and twauctex-insert-word-spacing (twauctex-looking-at-abbrev)) (insert "\\ "))
   (t (self-insert-command 1))))


(defun LaTeX-limited-auto-fill ()
  "Function to suppress auto fill when in an environment in `twauctex-inhibited-auto-fill-environments'."
  (when (not (member (LaTeX-current-environment) twauctex-inhibited-auto-fill-environments))
    (do-auto-fill)))


(defun twauctex-looking-at-abbrev (&optional electric-char)
  "Whether we are looking at an abbreviation in `twauctex-non-break-abbrevs'.

If ELECTRIC-CHAR is provided, don't try to match the
abbreviation with an ending dot. This is useful for checking
whether to break the line on an electric period. To use in
auto-fill, ELECTRIC-CHAR is nil."
  ;; If we have to search backwards for longer than the longest abbrevation for a space, it's not an abbrevation.
  (looking-back
   (concat "\\<" twauctex--abbrev-regexp (if electric-char "\\=" "\\.")) (- (point) twauctex--max-search-bound)))

;; Modified version from http://www.cs.au.dk/~abizjak/emacs/2016/03/06/latex-fill-paragraph.html
(defun twauctex-fill-ospl (&optional P)
  "Unfill the paragraph into one-sentence-per-line format.

If P is provided, just call the regular fill function."
  (interactive "P")
  (when (and (not (member (LaTeX-current-environment) twauctex-inhibited-auto-fill-environments))
	     (not (and (bolp) (eolp))))
    (if (not P)
	(save-excursion
	  (let ((fill-column most-positive-fixnum)) ;; relies on dynamic binding
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

(define-minor-mode twauctex-mode "Extend latex mode to make it easier to write one sentence per line. Makes sentence-end characters (.?!:) electric to insert a newline, and supresses spaces at the beginning of the line."
  nil
  " twauc"
  (append
   (list (cons (kbd "SPC") #'twauctex-electric-space)
	 (cons (kbd "_") #'twauctex-underscore-maybe)
	 (cons (kbd "&") #'twauctex-ampersand-maybe)
	 (cons (kbd "C-c f") #'twauctex-align-environment)
	 (cons (kbd "M-q") #'twauctex-fill-ospl))
   (mapcar (lambda (key) (cons (kbd (char-to-string key)) #'twauctex-electric-sentence-end-char)) twauctex-electric-chars))
  (when twauctex-mode
    ;; Activating
    (auto-fill-mode -1)
    (visual-line-mode 1)
    (set (make-local-variable 'fill-nobreak-predicate) #'twauctex-dont-break-on-nbsp)
    (twauctex--update-abbrev-regexp nil twauctex-non-break-abbrevs 'set nil)
    (twauctex--update-max-search-bound nil twauctex-non-break-abbrevs 'set nil)
    ;; We use hack-local-variables, because we want to take the
    ;; file-local fill column into account when setting the visual
    ;; fill column.
    (add-hook 'hack-local-variables-hook
	      (lambda () (visual-fill-column-mode 1))
	      nil t)))

(defun twauctex-enable ()
  "Enable twauctex mode."
  (when (and (buffer-file-name) (equal (file-name-extension (buffer-file-name)) "tex"))
    (twauctex-mode 1)))

(defun twauctex-global-mode ()
    "Automatically turn on twauctex mode in all LaTeX buffers."
    (interactive)
  (cl-pushnew #'twauctex-enable LaTeX-mode-hook))

(provide 'twauctex)
;;; twauctex.el ends here
