;; core emacs setup to be done in the early init phase
;;
;; -*- lexical-binding: t -*-

(defun ecfg-core-module-init ()
  (ecfg--setup-compatibility)
  (ecfg--setup-variables)
  (ecfg--setup-coding-systems)
  (ecfg--setup-backup)
)


(defun ecfg--setup-compatibility ()
  ;; Some workaround for emacs version < 24.0, thanks Silthanis@github.
  (if (< emacs-major-version 24)
      (defun file-name-base (&optional filename)
        "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
        (file-name-sans-extension
         (file-name-nondirectory (or filename (buffer-file-name))))))

  (when (version< emacs-version "24.3")
    (el-get 'sync '(cl-lib))
    (add-to-list 'load-path
                 (expand-file-name "el-get/cl-lib" user-emacs-directory))))


(defun ecfg--setup-variables ()
  (setq-default inhibit-startup-screen t)

  ;; we don't need menubar (execpt OSX), toolbar nor scrollbar
  (and (fboundp 'menu-bar-mode)
       (not (eq system-type 'darwin))
       (menu-bar-mode -1))
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))


  (delete-selection-mode)
  (setq-default x-select-enable-primary t)

  (setq-default tab-width 4)
  (setq-default standard-indent 4)
  (setq kill-whole-line t)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 80)

  (setq-default grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -e <R>")

  ;mac has troubles with "pasteboard doesn't contain valid data"
  ;; (setq save-interprogram-paste-before-kill t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info t))


(defun ecfg--setup-coding-systems ()
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'windows-1251)
  (prefer-coding-system 'utf-8))


(defun ecfg--setup-backup ()
  ;;Change backup behavior to save in a directory,
  ;;not in a miscellany of files all over the place.
  (setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist `(("." . ,(locate-user-emacs-file "backups")))
   delete-old-versions t
   kept-new-versions 3
   kept-old-versions 2
   version-control t       ; use versioned backups
   ;; make-backup-files nil)
   auto-save-default nil
   auto-save-list-file-prefix (expand-file-name
                               "auto-save-list/save-" user-emacs-directory)))
