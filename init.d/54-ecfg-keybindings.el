;; defining global keybindings
;;
;; -*- lexical-binding: t -*-

;;window navigation M-<arrow>
(windmove-default-keybindings 'meta)

(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "C-q") 'keyboard-quit)
(global-set-key (kbd "s-i") 'quoted-insert)
(define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-q") 'minibuffer-keyboard-quit)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "C-s-f") 'find-file-in-project)
(global-set-key (kbd "<C-s-268632070>") 'find-file-in-project)
(global-set-key (kbd "s-F") 'recentf-open-files)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "C-s-b") 'ibuffer)
(global-set-key (kbd "s-n") 'goto-line)
(global-unset-key (kbd "s-m"))        ;on mac runs iconify-frame
(global-unset-key (kbd "s-q"))        ;on mac runs kill-emacs
(global-set-key (kbd "s-j") 'delete-indentation)
(global-set-key (kbd "s-r") 'query-replace-regexp)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)

(global-set-key [?\C-z] 'undo)
(global-set-key [?\C-w] 'backward-delete-char)
(global-set-key [?\M-w] 'backward-kill-word)

(global-set-key (kbd "s-x") 'ecfg-kill-line-or-selection)
(global-set-key (kbd "s-c") 'ecfg-copy-line-or-selection)
(global-set-key "\C-v" 'clipboard-yank)
(global-set-key "\M-v" 'yank-pop)

(global-set-key (kbd "s-k") 'ecfg-kill-current-line)
(global-set-key (kbd "<S-return>") 'ecfg-end-of-line-and-indent)
(global-set-key [C-S-up] 'drag-stuff-up)
(global-set-key [C-S-down] 'drag-stuff-down)

(global-set-key (kbd "\C-c i") 'ecfg-reformat-text)
(global-set-key (kbd "C-/") 'ecfg-toggle-comment-on-lines)
(global-set-key (kbd "C-?") 'ecfg-toggle-comment)
(global-set-key (kbd "C-M-/") 'comment-indent)
(global-set-key (kbd "s-h") 'ecfg-highlight-current-tag-occurences)
(global-set-key (kbd "C-c s-h") 'ecfg-highlight-occurences)
(global-set-key (kbd "C-s-h") 'ecfg-unhighlight-all)
(global-set-key (kbd "<C-s-268632072>") 'ecfg-unhighlight-all) ;tweak for os-x
(global-unset-key (kbd "s-t")) ;os-x assigns ns-popup-font-panel for it
(global-unset-key (kbd "s-d")) ;isearch-repeat-backward gets on my nerves

(global-set-key [f5] 'bookmark-set)
(global-set-key [f6] 'bookmark-jump)
(global-set-key [f9] 'nav-toggle)

(global-unset-key (kbd "C-x C-z")) ;we don't need to minimize it through the shortcut
(global-set-key (kbd "C-c a") 'org-agenda) ;making agenda access global

  ;;; providing a universal keybinding for ending the work with buffer
  ;;; for both standard and client modes
(add-hook 'server-switch-hook
          (lambda ()
            (local-set-key (kbd "C-x k")
                           '(lambda ()
                              (interactive)
                              (if server-buffer-clients
                                  (server-edit)
                                (ido-kill-buffer))))))
