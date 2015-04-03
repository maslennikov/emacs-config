;; -*- lexical-binding: t -*-

(defun ecfg-global-module-init ()
  "Entry function for ecfg init system."

  (ecfg--setup-variables)
  (ecfg--setup-coding-systems)
  (ecfg--setup-backup)
  (ecfg--setup-uniquify)
  (ecfg--setup-autocomplete)
  (ecfg--setup-yasnippet)
  (ecfg--setup-emacs-nav)
  ;; (ecfg--setup-tramp)
  (ecfg--setup-ido)
  (ecfg--setup-ffip)
  (ecfg--setup-autopair)
  (ecfg--setup-markdown)
  (ecfg--setup-nuke-trailing-whitespace)
  (ecfg--setup-recentf)

  ;; getting rid of annoying auto-opened buffers
  (if (get-buffer ".emacs.elc") (kill-buffer ".emacs.elc"))
  (if (get-buffer ".emacs") (kill-buffer ".emacs"))

  ;; (add-hook 'kill-buffer-hook 'prompt-to-compile-dotemacs)
)


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
                               "auto-save-list/save-"
                               user-emacs-directory)))

(defun ecfg--setup-uniquify ()
  ;;uniquify is distributed with emacs
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (setq frame-title-format
        '(buffer-file-name
          "%f"
          (dired-directory dired-directory "%b"))))

(defun ecfg--setup-markdown ()
  (ecfg-install markdown-mode))

(defun ecfg--setup-autocomplete ()
  ;; todo: there is another module called pos-tip
  ;; todo: see how autocomplete in OME is implemented

  (ecfg-install auto-complete
   ;; ;; Load the default configuration
   ;; (require 'auto-complete-config)
   ;; (ac-config-default)
   ;; ;; Make sure we can find the dictionaries
   ;; (add-to-list 'ac-dictionary-directories
   ;;              (expand-file-name "auto-complete/dict" ecfg-plugin-root))
   (global-auto-complete-mode t)

   (setq
    ac-comphist-file (locate-user-emacs-file "ac-comphist.hist")
    ac-auto-start 2 ;; start completion after 2 characters of a word
    ac-auto-show-menu 0.5
    ac-ignore-case t)

   ;; when to trigger auto-complete
   (setq ac-trigger-commands
         '(self-insert-command
           delete-backward-char
           backward-delete-char
           autopair-backspace
           backward-delete-char-untabify))

   ;; define keybindings
   (setq ac-use-menu-map t)
   (define-key ac-menu-map "\C-n" 'ac-next)
   (define-key ac-menu-map "\C-p" 'ac-previous)
   (define-key ac-menu-map (kbd "<return>") 'ac-complete)
   (define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)))


(defun ecfg--setup-yasnippet ()
  ;; todo: see how yasnippet in OME is set-up
  (ecfg-install yasnippet

   (eval-after-load "yasnippet"
     '(progn
        (setq yas-snippet-dirs (expand-file-name "snippets" ecfg-dir))
        (yas-reload-all)))))


(defun ecfg--setup-emacs-nav ()
  ;; todo find something better

  (ecfg-install nav
       (nav-disable-overeager-window-splitting)))


(defun ecfg--setup-ido ()
  (require 'ido)

  (setq
   ido-save-directory-list-file (locate-user-emacs-file "ido.hist")
   ido-enable-flex-matching t
   ido-everywhere t
   ido-create-new-buffer 'always
   ido-use-filename-at-point 'guess
   ido-confirm-unique-completion nil
   ido-auto-merge-work-directories-length -1)

  (ido-mode t)

  (ecfg-install ido-ubiquitous
   (ido-ubiquitous-mode))

  (ecfg-install smex
   (setq smex-save-file (locate-user-emacs-file "smex.hist"))
   (global-set-key (kbd "M-x") 'smex)
   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
   ;; This is your old M-x.
   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))


(defun ecfg--setup-ffip ()
  (ecfg-install find-file-in-project
   (eval-after-load "find-file-in-project"
    '(progn
       (mapc (lambda (val) (add-to-list 'ffip-patterns val))
             '("*.c" "*.cpp" "*.h" "*.hpp" "*.cmake" "*.s"))))))

(defun ecfg--setup-autopair ()
  ;; todo: look at cua mode?
  (ecfg-install autopair
      (autopair-global-mode)
      (setq autopair-blink nil)
      (setq autopair-pair-criteria 'always)
      (setq autopair-skip-whitespace 'chomp)))

(defun ecfg--setup-nuke-trailing-whitespace ()
  ;; (eval-after-load "nuke-trailing-whitespace"
  ;;   '(mapc (lambda (val)
  ;;            (add-to-list 'nuke-trailing-whitespace-always-major-modes val))
  ;;          '(python-mode cmake-mode javascript-mode js2-mode css-mode org-mode)))
  (add-hook 'before-save-hook 'whitespace-cleanup))

(defun ecfg--setup-recentf ()
  (require 'recentf)
  (recentf-mode t)
  (setq
   recentf-max-menu-items 25
   recentf-save-file (locate-user-emacs-file "recentf.hist")))
