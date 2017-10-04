;; Defining global keybindings.
;;
;; Right now most of the global keybindings are defined here. Nevertheless, some
;; of the definitions still pop up in the module configurations (e.g. in
;; modules/ecfg-org.el). Please grep for `global-set-key' entries everywhere
;; before defining a new keybinding here.
;;
;; Mode- or buffer-local stuff is normally defined in hooks to corresponding
;; modes.
;;
;; -*- lexical-binding: t -*-

(defun ecfg-keybindings-module-init ()

;;; Compatibility

  ;; forcing emacs handle winkeys in windows
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)


;;; Windows, files, and buffers

  ;; todo explore helm stuff
  (windmove-default-keybindings 'super)

  (global-set-key (kbd "C-s-b") 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "<C-s-268632066>") 'ibuffer) ;tweak for os-x

  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-S") 'write-file)


;;; Copy, paste, insert

  (global-set-key [?\C-z] 'undo)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key [?\C-w] 'backward-delete-char)
  (global-set-key [?\M-w] 'backward-kill-word)

  (global-set-key (kbd "s-x") 'ecfg-kill-line-or-selection)
  (global-set-key (kbd "s-c") 'ecfg-copy-line-or-selection)
  (global-set-key (kbd "s-d") 'ecfg-duplicate-line)
  (global-set-key "\C-v" 'clipboard-yank)
  (global-set-key "\M-v" 'yank-pop)
  ;; (global-set-key "\C-\M-v" 'popup-yank-menu)

  (global-set-key (kbd "s-i") 'quoted-insert)

  (global-set-key (kbd "C-/") 'ecfg-toggle-comment-on-lines)
  (global-set-key (kbd "C-?") 'ecfg-toggle-comment)
  (global-set-key (kbd "C-M-/") 'comment-indent)


;;; Indentation, line manipulation

  (global-set-key (kbd "s-j") 'delete-indentation)
  (global-set-key (kbd "s-n") 'goto-line)
  (global-set-key (kbd "s-k") 'ecfg-kill-current-line)
  (global-set-key (kbd "<S-return>") 'ecfg-end-of-line-and-indent)
  (global-set-key [C-S-up] 'drag-stuff-up)
  (global-set-key [C-S-down] 'drag-stuff-down)

  (global-set-key (kbd "\C-c i") 'ecfg-reformat-text)


;;; Search and replace

  ;; Use regex searches by default.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (global-set-key (kbd "s-r") 'query-replace-regexp)

  (global-set-key (kbd "s-h") 'ecfg-highlight-current-tag-occurences)
  (global-set-key (kbd "C-S-s-h") 'ecfg-highlight-occurences)
  (global-set-key (kbd "C-s-h") 'ecfg-unhighlight-all)
  (global-set-key (kbd "<C-s-268632072>") 'ecfg-unhighlight-all) ;tweak for os-x

  (global-set-key (kbd "C-c o") 'occur)


;;; Fn keys

  (global-set-key [f5] 'bookmark-set)
  (global-set-key [f6] 'bookmark-jump)


;;; Completion

  ;; hippie expand is dabbrev expand on steroids
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "<C-tab>") 'company-complete)


;;; Font size

  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C-_") 'text-scale-decrease)


;;; Help and reference

  (global-set-key (kbd "C-h a") 'apropos)


;;; Unbinding and aborting

  (global-set-key (kbd "<escape>") 'keyboard-quit)
  (define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  (global-unset-key (kbd "s-m"))        ;on mac runs iconify-frame
  (global-unset-key (kbd "s-q"))        ;on mac runs kill-emacs
  (global-unset-key (kbd "s-t"))        ;os-x assigns ns-popup-font-panel for it
  (global-unset-key (kbd "C-x C-z"))    ;don't need to minimize it with shortcut


;;; Kill buffer hook

  ;; Providing a universal keybinding for ending the work with buffer for both
  ;; standard and client modes
  (add-hook 'server-switch-hook (lambda ()
     (local-set-key (kbd "C-x k") '(lambda ()
         (interactive)
         (if server-buffer-clients (server-edit) (ido-kill-buffer))))))

)
