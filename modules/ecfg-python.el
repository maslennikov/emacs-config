;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-python-module-init ()
  (add-hook 'python-mode-hook 'ecfg--py-hook))
;;;###autoload (ecfg-auto-module "\\.py$" python)

(defun ecfg--py-hook ()
  ;;specifying underscore as a member of symbol class instead of word class
  ;; (modify-syntax-entry ?_ ".")
  (eval-after-load "company"
    '(progn
       (set
        (make-local-variable 'company-backends)
        '((company-dabbrev-code
           company-keywords
           company-dabbrev
           company-yasnippet)))))

  (yas-minor-mode)
  )
