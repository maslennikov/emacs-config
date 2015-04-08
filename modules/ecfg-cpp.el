;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-cpp-module-init ()
  (add-hook 'c-mode-hook 'ecfg--cpp-hook)
  (add-hook 'c++-mode-hook 'ecfg--cpp-hook))

;;;###autoload (ecfg-auto-module "\\.c$" cpp)
;;;###autoload (ecfg-auto-module "\\.cc$" cpp)
;;;###autoload (ecfg-auto-module "\\.cpp$" cpp)
;;;###autoload (ecfg-auto-module "\\.h$" cpp)
;;;###autoload (ecfg-auto-module "\\.hh$" cpp)
;;;###autoload (ecfg-auto-module "\\.hpp$" cpp)

(defun ecfg--cpp-hook ()
  (semantic-mode)
  (add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function)

  (setq c-basic-offset 4)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-cont '+)
  (c-set-offset 'arglist-cont-nonempty '+)

  (local-set-key (kbd "<f8>") 'ecfg--toggle-file)

  ;; (setq ac-sources
  ;;       (append ac-sources
  ;;               '(ac-source-words-in-buffer
  ;;                 ac-source-files-in-current-dir)))

  (eval-after-load "company"
    '(progn
       (set
        (make-local-variable 'company-backends)
        '((company-semantic
           company-dabbrev-code
           company-keywords
           company-dabbrev
           company-yasnippet)))))

  (yas-minor-mode)
  (add-hook 'post-self-insert-hook 'ecfg-two-dots nil t))



(defun ecfg--toggle-file ()
  (interactive)

  (require 'find-file-in-project)

  (defvar ecfg--toggle-cache '())

  (defun lookup-in-cache (current-path)
    "Returns cons (result . cache-cell) or nil"
    (let ((left (assoc current-path ecfg--toggle-cache))
          (right (rassoc current-path ecfg--toggle-cache)))

      (cond
       (left `(,(cdr left) . ,left))
       (right `(,(car right) . ,right))
       (t '()))))

  (defun invalidate-cache (cache-result)
    (setq ecfg--toggle-cache (delq (cdr cache-result) ecfg--toggle-cache)))

  (defun validated-cache-result (cache-result)
    (if (or (not cache-result) (file-exists-p (car cache-result)))
        cache-result
      (invalidate-cache cache-result)
      '()))

  (defun cached (key val)
      (if (and key val)
          (let ((entry `(,key . ,val)))
            (add-to-list 'ecfg--toggle-cache entry)
            `(,val . ,entry))
        '()))

  (defun cached-fun (arg fun)
    (car (or (validated-cache-result (lookup-in-cache arg))
             (cached arg (funcall fun arg)))))

  (defun find-toggle-ext (current-ext)
    (cond
     ((string-match-p "^c\\(pp\\)?$" current-ext) "h\\(pp\\)?")
     ((string-match-p "^h\\(pp\\)?$" current-ext) "c\\(pp\\)?")
     (t "")))

  (defun finder (current-path)
    (let* ((current-file (file-name-nondirectory current-path))
           (target-regex
            (concat (file-name-base current-file) "\\."
                    (find-toggle-ext (file-name-extension current-file))))
           (ffip-find-options (concat "-regex \".*/" target-regex "\"")))
      (cdar (ffip-project-files))))

  (defun find-toggle-file (current-path)
    (cached-fun
     current-path
     'finder))

  (let ((toggle-file (find-toggle-file (or (buffer-file-name) ""))))
    (if toggle-file (find-file toggle-file))))
