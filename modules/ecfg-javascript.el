;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-javascript-module-init ()
  (ecfg-install js2-mode
    (ecfg-with-local-autoloads
   ;; (autoload 'js2-mode "js2-mode" nil t)
   ;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
   (add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))

   (eval-after-load 'js2-mode
     '(progn
        (setq
         ;; TODO a lot of new goodies appeared in js2-mode,
         ;; research customize-group
         js2-cleanup-whitespace t
         js2-auto-indent-p t
         js2-bounce-indent-p nil
         js2-idle-timer-delay 0.5
         js2-mirror-mode nil
         js2-missing-semi-one-line-override t
         js2-allow-keywords-as-property-names nil
         js2-pretty-multiline-declarations t)

        (define-key js2-mode-map (kbd "RET") 'js2-line-break)))

   (add-hook 'js2-mode-hook 'ecfg--js-hook)
   ;; (add-hook 'js2-jsx-mode-hook #'(lambda ()
     ;; (setq-local sgml-basic-offset js2-basic-offset)))
   )))

;;;###autoload (ecfg-auto-module "\\.js$" javascript)

(defun ecfg--manually-bounce-indent ()
  (interactive)
  (let ((js2-bounce-indent-p t))
    (js2-indent-line)))

(defun ecfg--js-hook ()
  ;; (setq ac-sources
  ;;       (append ac-sources
  ;;               '(ac-source-yasnippet
  ;;                 ac-source-words-in-buffer
  ;;                 ac-source-words-in-same-mode-buffers
  ;;                 ac-source-files-in-current-dir)))

  (eval-after-load "company"
    '(progn
       (set
        (make-local-variable 'company-backends)
        '((company-dabbrev-code
           company-keywords
           company-dabbrev
           company-yasnippet)))))

  (yas-minor-mode)
  (local-set-key (kbd "C-j") (kbd "<return>"))
  (local-set-key (kbd "<s-tab>") 'ecfg--manually-bounce-indent)
  (setq mode-name "JS2"))
