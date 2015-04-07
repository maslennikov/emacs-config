;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-work-module-init ()
  ;; (setq ack-command "ack-grep --nocolor --nogroup --ignore-dir=uploads ")

  (eval-after-load "grep"
    '(progn
       (grep-compute-defaults)
       (add-to-list 'grep-find-ignored-directories "build*")))

  (eval-after-load "find-file-in-project"
    '(progn
       (setq ffip-limit 3000)
       (setq ffip-find-options "-not -path \"*/build*/*\"")))

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))
