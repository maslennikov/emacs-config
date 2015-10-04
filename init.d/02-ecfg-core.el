;; core emacs setup to be done in the early init phase
;;
;; -*- lexical-binding: t -*-

(defun ecfg-core-module-init ()

;;; GUI
  (setq-default inhibit-startup-screen t)

  ;; we don't need menubar (execpt OSX), toolbar nor scrollbar
  (and (fboundp 'menu-bar-mode)
       (not (eq system-type 'darwin))
       (menu-bar-mode -1))
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

;;; Killing
  (setq-default x-select-enable-primary t)
  (delete-selection-mode t)
  (setq kill-whole-line t)
    ;; mac has troubles with "pasteboard doesn't contain valid data"
  ;; (setq save-interprogram-paste-before-kill t)

;;; Indenting
  (setq-default tab-width 4)
  (setq-default standard-indent 4)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 80)

;;; Whitespace
  ;; always add new line to the end of a file
  (setq require-final-newline t)
  ;; fixing trailing whitespace
  (add-hook 'before-save-hook 'whitespace-cleanup)

;;; Shell-mode settings
  (unless (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "/bin/bash")
    (setq shell-file-name "/bin/bash"))
  ;; always insert at the bottom
  (setq comint-scroll-to-bottom-on-input t)
  ;; no duplicates in command history
  (setq comint-input-ignoredups t)
  ;; what to run when i press enter on a line above the current prompt
  (setq comint-get-old-input (lambda () ""))
  ;; max shell history size
  (setq comint-input-ring-size 1000)
  ;; show all in emacs interactive output
  (setenv "PAGER" "cat")
  ;; set lang to enable Chinese display in shell-mode
  (setenv "LANG" "en_US.UTF-8")

;;; Coding systems and localization
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'windows-1251)
  (prefer-coding-system 'utf-8)

  (add-hook 'calendar-load-hook (lambda () (calendar-set-date-style 'european)))
  (setq calendar-week-start-day 1)

;;; auto-revert everything
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info t)

;;; Backups

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
                               "auto-save-list/save-" user-emacs-directory))


;;; Misc
  (setq-default grep-find-template
   "find <D> <X> -type f <F> -print0 | xargs -0 grep <C> -nH -e <R>")

  ;; subword mode everywhere
  (global-subword-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
)
