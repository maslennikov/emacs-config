;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-org-module-init ()
  ;;WARNING: if el-get fails to clone the org-mode git repo, try to place the
  ;;following mirror url into 'el-get/el-get/recipes/org-mode.rcp':
  ;;http://repo.or.cz/org-mode.git
  (ecfg-install org-mode

   ;; see doc to `org-agenda-files' variable for more info
   (global-set-key "\C-cl" 'org-store-link)
   (global-set-key "\C-ca" 'org-agenda)

   ;; for capturing, you'll also want to put something similar in your ~/.emacs:
   ;; (setq org-directory "~/Documents/notes")
   ;; (setq org-default-notes-file (expand-file-name "inbox.org.txt" org-directory))
   (global-set-key "\C-cc" 'org-capture)

   ;; clocking time
   (global-set-key (kbd "<f10>")
      '(lambda ()
         (interactive)
         (let ((current-prefix-arg '(4))) ;; emulate C-u
           (call-interactively 'org-clock-in-last))))

   (global-set-key (kbd "<M-f10>") 'org-clock-in-last)
   (global-set-key (kbd "<f11>") 'org-clock-out)

   (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
   (add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

   (eval-after-load "org"
     '(progn
        ;; releasing the autocomplete shortcut
        (define-key org-mode-map (kbd "<C-tab>") nil)
        (setq org-clock-persist 'history)
        (org-clock-persistence-insinuate)
        (setq org-clock-idle-time 10)
        ;; measuring time always in hours and minutes
        (setq org-time-clocksum-format
              '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))))

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

  (setq
   org-src-fontify-natively t
   org-show-entry-below '((tags-tree . t))
   org-footnote-auto-label 'confirm))
