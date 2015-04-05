;; setting-up global modes and core plugins
;;
;; -*- lexical-binding: t -*-

(defun ecfg-global-module-init ()
  (ecfg--setup-drag-stuff)
  (ecfg--setup-uniquify)
  (ecfg--setup-autocomplete)
  (ecfg--setup-yasnippet)
  ;; (ecfg--setup-tramp)
  (ecfg--setup-ido)
  (ecfg--setup-ffip)
  (ecfg--setup-autopair)
  (ecfg--setup-nuke-trailing-whitespace)
  (ecfg--setup-recentf)

  ;; getting rid of annoying auto-opened buffers
  (if (get-buffer ".emacs.elc") (kill-buffer ".emacs.elc"))
  (if (get-buffer ".emacs") (kill-buffer ".emacs"))

  ;; (add-hook 'kill-buffer-hook 'prompt-to-compile-dotemacs)
)


(defun ecfg--setup-drag-stuff ()
  (ecfg-install drag-stuff
    (autoload 'drag-stuff-up "drag-stuff")
    (autoload 'drag-stuff-down "drag-stuff")
    ))

(defun ecfg--setup-uniquify ()
  ;;uniquify is distributed with emacs
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (setq frame-title-format
        '(buffer-file-name
          "%f"
          (dired-directory dired-directory "%b"))))


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
   (autoload 'yas-minor-mode "yasnippet" "turns yasnippet minor mode on" t)
   (eval-after-load "yasnippet"
     '(progn
        (setq yas-snippet-dirs (expand-file-name "snippets" ecfg-dir))
        (yas-reload-all)))))


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
   (require 'ido-ubiquitous)
   (ido-ubiquitous-mode))

  (ecfg-install smex
   (setq smex-save-file (locate-user-emacs-file "smex.hist"))
   (global-set-key (kbd "M-x") 'smex)
   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
   ;; This is your old M-x.
   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))


(defun ecfg--setup-ffip ()
  (ecfg-install find-file-in-project
   (autoload 'find-file-in-project "find-file-in-project" "triggers ffip search" t)
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
  (add-hook 'before-save-hook 'whitespace-cleanup))

(defun ecfg--setup-recentf ()
  (require 'recentf)
  (recentf-mode t)
  (setq
   recentf-max-menu-items 25
   recentf-save-file (locate-user-emacs-file "recentf.hist")))
