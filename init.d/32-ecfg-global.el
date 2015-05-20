;; setting-up global modes and core plugins
;;
;; -*- lexical-binding: t -*-

(defun ecfg-global-module-init ()
  (ecfg--setup-drag-stuff)
  (ecfg--setup-uniquify)
  (ecfg--setup-autocomplete)
  (ecfg--setup-yasnippet)
  ;; (ecfg--setup-ido)
  (ecfg--setup-helm)
  (ecfg--setup-autopair)
  (ecfg--setup-recentf)
  (ecfg--setup-undo)

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
  (setq
   uniquify-buffer-name-style 'forward

   ;; The value may be nil, a string, a symbol or a list.
   ;;
   ;; A list whose car is a string or list is processed by processing each of
   ;; the list elements recursively, as separate mode line constructs,and
   ;; concatenating the results.
   ;;
   ;; A list whose car is a symbol is processed by examining the symbol's value,
   ;; and, if that value is non-nil, processing the cadr of the list
   ;; recursively; and if that value is nil, processing the caddr of the list
   ;; recursively.
   frame-title-format
   '(buffer-file-name "%b  -  %f" (dired-directory dired-directory "%b"))))


(defun ecfg--setup-autocomplete ()
  ;; todo: there is another module called pos-tip
  ;; todo: see how autocomplete in OME is implemented

  ;; use icomplete in minibuffer
  ;; (icomplete-mode t) ;ido handles this already

;;; Basic completion-at-point setup
  ;; (add-to-list 'completion-styles 'substring)
  (setq completion-cycle-threshold 5)

  (ecfg-install company-mode
   (ecfg-with-local-autoloads
    (autoload 'company-complete "company" nil t)

    (eval-after-load "company"
      '(progn
         (add-to-list 'company-begin-commands 'backward-delete-char)
         (setq
          company-dabbrev-code-ignore-case t
          company-dabbrev-downcase nil
          company-minimum-prefix-length 2
          ;;something universally applied
          company-backends '((company-capf
                              :with
                              company-dabbrev-code
                              company-keywords)
                             company-dabbrev))

         ;; using it instead of `company-complete-common'
         (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
         ;; getting the normal 'C-w' behaviour back
         (define-key company-active-map (kbd "C-w") nil)
         ;; not turning it on until the first usage (is it ok?)
         (global-company-mode))))))


(defun ecfg--setup-yasnippet ()
  ;; todo: see how yasnippet in OME is set-up
  (ecfg-install yasnippet
   (autoload 'yas-minor-mode "yasnippet" "turns yasnippet minor mode on" t)
   (eval-after-load "yasnippet"
     '(progn
        (setq yas-snippet-dirs (expand-file-name "snippets" ecfg-dir))
        (yas-reload-all)))))


(defun ecfg--setup-helm ()
  (ecfg-install helm
   (require 'helm-config)
   ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
   ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
   ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
   (global-set-key (kbd "C-q") 'helm-command-prefix)
   (global-unset-key (kbd "C-x c"))

   (ecfg-with-local-autoloads
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "s-f") 'helm-find-files)
    (global-set-key (kbd "s-b") 'helm-mini)
    (global-set-key (kbd "C-q g") 'ecfg-helm-do-grep-recursive)
    (global-set-key (kbd "C-q o") 'helm-occur)

    (eval-after-load "helm" '(ecfg--helm-hook))))

;;; set-up projectile
  (ecfg-install projectile
   (ecfg-with-local-autoloads
    (global-set-key (kbd "C-s-f") 'helm-projectile-find-file-dwim)
    (global-set-key (kbd "<C-s-268632070>") 'helm-projectile-find-file-dwim)
    (global-set-key (kbd "<f8>") 'helm-projectile-find-other-file)
    ;; todo consider using helm-projectile-ag
    (global-set-key (kbd "<f9>") 'helm-projectile-grep))))


(defun ecfg--helm-hook ()
  "Is called when helm is loaded."

  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

;;; helm keymap
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (helm-autoresize-mode 1)
;;; helm variables
  (setq
   ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-in-side-p t
   ;; move to end or beginning of source when reaching top or bottom of source.
   ;; helm-move-to-line-cycle-in-source t
   ;; search for library in `require' and `declare-function' sexp.
   helm-ff-search-library-in-sexp t
   ;; scroll 8 lines other window using M-<next>/M-<prior>
   helm-scroll-amount 8
   helm-ff-file-name-history-use-recentf t
   ;; fuzzy matching
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-display-header-line nil
   ;; limiting the window height (works with autoresize-mode)
   helm-autoresize-max-height 35
   helm-autoresize-min-height 35
   )

  )

(defun ecfg-helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))


(defun ecfg--setup-ido ()
  (require 'ido)

  (setq
   ido-enable-flex-matching t
   ido-enable-prefix nil
   ido-enable-case nil
   ido-everywhere t
   ido-create-new-buffer 'always
   ;; ido-use-filename-at-point 'guess
   ido-confirm-unique-completion nil
   ;; ido-auto-merge-work-directories-length -1
   )

  (ido-mode t)

  (ecfg-install ido-ubiquitous
   (require 'ido-ubiquitous)
   (ido-ubiquitous-mode))

  ;; (ecfg-install smex
  ;;  (setq smex-save-file (locate-user-emacs-file "smex.hist"))
  ;;  (global-set-key (kbd "M-x") 'smex)
  ;;  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;;  ;; This is your old M-x.
  ;;  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))
  )


(defun ecfg--setup-autopair ()
  ;; todo: look at cua mode?
  (ecfg-install autopair
      (autopair-global-mode)
      (setq autopair-blink nil)
      (setq autopair-pair-criteria 'always)
      (setq autopair-skip-whitespace 'chomp)))


(defun ecfg--setup-recentf ()
  ;; see masteringemacs.org/article/find-files-faster-recent-files-package
  (require 'recentf)
  (setq
   recentf-max-menu-items 25
   recentf-max-saved-items 50
   recentf-save-file (locate-user-emacs-file "recentf.hist")
   ;; clean-up when idling for this many seconds
   recentf-auto-cleanup 60)

  (recentf-mode t)

  (defun ecfg-ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))


(defun ecfg--setup-undo ()
  (ecfg-install undo-tree
   (ecfg-with-local-autoloads

    (eval-after-load "undo-tree"
      '(progn
         (global-set-key (kbd "C-/") 'ecfg-toggle-comment-on-lines)
         (global-set-key (kbd "C-z") 'undo-tree-undo)
         (global-set-key (kbd "M-z") 'undo-tree-redo)
         (global-set-key (kbd "s-z") 'undo-tree-visualize)))

    (defun ecfg--load-undo-tree-hook ()
      (let ((buffer (buffer-name)))
        ;; ignoring buffers like " *temp*", "*helm mini*", etc; keeping scratch
        (unless (and (string-match-p "^[ \t]*\\*.*\\*[ \t]*$" buffer)
                     (not (string= "*scratch*" buffer)))
          (global-undo-tree-mode)
          ;; unsubscribing
          (remove-hook 'first-change-hook 'ecfg--load-undo-tree-hook))))

    ;; applying this only after the window-setup: after *scratch* is already
    ;; there and opened file buffer is populated
    (add-hook 'window-setup-hook
     (lambda () (add-hook 'first-change-hook 'ecfg--load-undo-tree-hook))))))
