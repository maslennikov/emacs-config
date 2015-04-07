;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-org-module-init ()
  ;;WARNING: if el-get fails to clone the org-mode git repo, try to place the
  ;;following mirror url into 'el-get/el-get/recipes/org-mode.rcp':
  ;;http://repo.or.cz/org-mode.git
  (ecfg-install org-mode
   ;; todo: move to keybindings file
   (global-set-key "\C-cl" 'org-store-link)
   (global-set-key "\C-ca" 'org-agenda)

   (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
   (add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))
   (add-hook 'org-mode-hook 'ecfg--org-hook)))

;; autoloading org-mode immediately to override standard org-loaddefs
;;;###autoload (ecfg-org-module-init)
;; this we don't need
;; ;;;###autoload (ecfg-auto-module "\\.org$" org)
;;; ;;###autoload (ecfg-auto-module "\\.org.txt$" org)


(defun ecfg--org-hook ()
  ;; (org-remove-from-invisibility-spec '(org-link))
  (local-unset-key (kbd "<M-left>"))
  (local-unset-key (kbd "<M-right>"))
  (local-unset-key (kbd "<M-up>"))
  (local-unset-key (kbd "<M-down>"))

  (setq org-src-fontify-natively t)
  (setq org-show-entry-below '((tags-tree . t))))
