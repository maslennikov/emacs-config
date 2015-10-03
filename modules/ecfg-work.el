;; -*- lexical-binding: t -*-
;;
;; Mode for setting-up miscellaneous working environment. Currently loaded
;; automatically by start-up. As alternative, could be loaded from .emacs
;; manually.

;;;###autoload
(defun ecfg-work-module-init ()
  ;; (setq ack-command "ack-grep --nocolor --nogroup --ignore-dir=uploads ")

  (eval-after-load "grep"
    '(progn
       (grep-compute-defaults)
       (add-to-list 'grep-find-ignored-directories "build*")
       (add-to-list 'grep-find-ignored-directories "contrib")
       (add-to-list 'grep-find-ignored-directories "bin")
       (add-to-list 'grep-find-ignored-directories "*cache")

       (add-to-list 'grep-find-ignored-directories "node_modules")
       (add-to-list 'grep-find-ignored-directories "bower_components")
       (add-to-list 'grep-find-ignored-directories "uploads")

       (add-to-list 'grep-find-ignored-files "*.min.js")))

  (eval-after-load "projectile"
    '(progn
       (add-to-list 'projectile-globally-ignored-directories "node_modules")
       (add-to-list 'projectile-globally-ignored-directories "bower_components")
       (add-to-list 'projectile-globally-ignored-directories "client/bower_components")))
  )

;; autoloading it immediately
;;;###autoload (ecfg-work-module-init)
