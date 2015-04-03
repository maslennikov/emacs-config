;; global function definitions
;;
;; -*- lexical-binding: t -*-

(defun ecfg-end-buffer-with-blank-line ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (open-line 1)
    (delete-blank-lines)))

(defun ecfg--is-mark-mode ()
  (and mark-active transient-mark-mode))

(defun ecfg--normalize-region-point-mark (&optional reverse)
  "in transient-mark-mode place point after mark"
  (if reverse
      (if (> (point) (mark)) (exchange-point-and-mark))
    (if (< (point) (mark)) (exchange-point-and-mark))))

(defun ecfg-end-of-line-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun ecfg-kill-current-line ()
  (interactive)
  (back-to-indentation)
  (let ((useful-text (buffer-substring (point) (line-end-position)))
        (kill-whole-line t))
    (beginning-of-line)
    (kill-line)
    ;; replacing in the kill-ring first item with useful text
    (kill-new useful-text "replace")))

(defun ecfg-kill-line-or-selection ()
  (interactive)
  (if (ecfg--is-mark-mode)
      (clipboard-kill-region (point) (mark))
    (ecfg-kill-current-line)))

(defun ecfg-copy-line-or-selection ()
  (interactive)
  (if (ecfg--is-mark-mode)
      (clipboard-kill-ring-save (point) (mark))
    (save-excursion
      (back-to-indentation)
      (clipboard-kill-ring-save (point) (line-end-position)))))

(defun ecfg-reformat-text ()
  (defun ecfg--reformat-region (start end)
    (if (> start end)
        (ecfg--reformat-region end start)
      (if indent-tabs-mode
          (tabify start end)
        (untabify start end))
      (indent-region start end)))

  (interactive)
  (if (ecfg--is-mark-mode)
      (ecfg--reformat-region (point) (mark))
    (ecfg--reformat-region (point-min) (point-max))))

(defun ecfg-toggle-comment-on-lines ()
  "Commenting out all lines that include region, or current line"
  (interactive)
  (cond
   ((ecfg--is-mark-mode)
    ;; commenting all lines containing region
    (comment-or-uncomment-region
     (progn
       (ecfg--normalize-region-point-mark t)
       (line-beginning-position))
     (progn
       (ecfg--normalize-region-point-mark)
       (line-end-position))))
   (t ;; commenting current line
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position)))))

(defun ecfg-toggle-comment ()
  "Commenting region or current line"
  (interactive)
  (cond
   ((ecfg--is-mark-mode)
    (ecfg--normalize-region-point-mark)
    (comment-or-uncomment-region (mark) (point)))
   (t ;; commenting current line
    (comment-or-uncomment-region
       (line-beginning-position) (line-end-position)))))


(defun ecfg--tag-under-cursor ()
  "Return tag under cursor using mode-specific find-tag-default-function"
  (or (and transient-mark-mode mark-active
           (/= (point) (mark))
           (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
                   (get major-mode 'find-tag-default-function)
                   'find-tag-default))
      ""))

(defun ecfg--prompt-read-regexp (prompt &optional default)
  "Read regexp arg for interactive usage"
    (read-regexp
     (concat prompt
         (if (and default (> (length default) 0))
         (format " (default \"%s\"): " default) ": "))
     default nil))

(defun ecfg-unhighlight-all ()
  (interactive)
  (when (boundp 'hi-lock-interactive-patterns)
      ;; unhighlight everything from the highlight history
      (mapcar (lambda (item) (unhighlight-regexp (car item)))
              hi-lock-interactive-patterns)))

(defun ecfg-highlight-occurences (regexp)
  "Highlight all occurences of the given symbol in the file"
    (interactive
       (list (ecfg--prompt-read-regexp "Highlight" (ecfg--tag-under-cursor))))

    (ecfg-unhighlight-all)
    (highlight-regexp regexp 'hi-red-b))

(defun ecfg-highlight-current-tag-occurences ()
  "Highlight all occurences of the symbol under cursor in the file"
    (interactive)
    (ecfg-unhighlight-all)
    (highlight-regexp (ecfg--tag-under-cursor) 'hi-red-b))


(defun ecfg-two-dots ()
  "Replacing '..' with '->' for better C++ coding"

  ;; TODO: make it undoable
  (let ((_point (point))
        (_start (max (point-min) (- (point) 3))))
    (goto-char _start)
    ;; replacing two dots coming after an identifier or after a closing brace/bracket
    (if (search-forward-regexp "\\([])a-zA-Z_0-9]\\)\\.\\." _point t)
        (replace-match "\\1->"))
    ;; save-excursion will not work with replace-match
    (goto-char _point)))


(defmacro ecfg-with-named-params (args keys other-keys-allowed &rest body)
  "Execute BODY in the context of the parsed ARGS proplist.
Every key mentioned in KEYS will be searched in ARGS. KEYS
consist of keywords or cons cells from keyword and default value.
If OTHER-KEYS-ALLOWED is t, it won't complain about garbage props
in args.

Example:
 (ecfg-with-named-params
  '(:hello 1)                    ; <-- args
  (:hello (:beautiful 0) :world) ; <-- keys
  ()                             ; <-- other-keys-allowed
  (format \"hello: %s beautiful: %s world: %s\" hello beautiful world))
"

  (defun ecfg--argkey (keydef)
    "Wrapping argument keyword descriptor KEYDEF. It can be a
keyword symbol or a cons of keyword symbol and argument's default value."

    (let* ((normalized (if (consp keydef) keydef (list keydef nil)))
           (argkey-symbol (car normalized))
           (argkey-defval (car (cdr normalized))))

      (or (keywordp argkey-symbol) (error "Bad keyword: %s" argkey-symbol))

      ;; returning 'monad' providing a getter with a keyword an default value
      `(lambda (getter)
           (funcall getter ,argkey-symbol ,argkey-defval))))

  ;; getters for `ecfg--argkey' output, for example:
  ;; (ecfg--argkey-symbol (ecfg--argkey :hello)) --> :hello
  (defun ecfg--argkey-symbol (argkey) (funcall argkey (lambda (k v) k)))
  (defun ecfg--argkey-defval (argkey) (funcall argkey (lambda (k v) v)))


  `(let
    ,(mapcar
      (lambda (keydef)
        (let* ((argkey (ecfg--argkey keydef))
               (keyname (ecfg--argkey-symbol argkey))
               (keyval `(plist-get ,args ',keyname)))

          (list (intern (substring (symbol-name keyname) 1))
                (if (ecfg--argkey-defval argkey)
                    `(or ,keyval ,(ecfg--argkey-defval argkey))
                  keyval))))
      keys)

    ,@(and
       (not (eq other-keys-allowed t))
       `((let ((keys-temp ,args))
           (while keys-temp
             (or
              (memq (car keys-temp)
                    ',(mapcar
                       (lambda (keydef) (ecfg--argkey-symbol (ecfg--argkey keydef)))
                       (append other-keys-allowed keys)))
              (error "Unexpected keyword argument %s" (car keys-temp)))
             (setq keys-temp (cdr (cdr keys-temp)))))))
    ,@body))
