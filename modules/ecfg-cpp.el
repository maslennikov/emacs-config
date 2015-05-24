;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-cpp-module-init ()
  (add-hook 'c-mode-hook 'ecfg--cpp-hook)
  (add-hook 'c++-mode-hook 'ecfg--cpp-hook)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

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

  ;; navigating through code with ctrl-mouse
  (global-set-key '[(C-down-mouse-1)] 'semantic-ia-fast-mouse-jump)

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
