;; configuring the look and feel
;;
;; -*- lexical-binding: t -*-

(defun ecfg-gui-module-init ()
  "Entry function for ecfg init system."

  (ecfg--setup-basic-gui)
  (ecfg--setup-sml-modeline)
  (ecfg--setup-tabbar)
  (ecfg--setup-emacs-nav)
  (ecfg--setup-color-theme))


(defun ecfg--setup-basic-gui ()
  (setq-default inhibit-startup-screen t)
  (show-paren-mode t)
  (global-hl-line-mode t)
  (column-number-mode t))

(defun ecfg--setup-sml-modeline ()
  (ecfg-install sml-modeline
   (require 'sml-modeline)
   (setq sml-modeline-borders '("[" . "]"))
   (setq sml-modeline-len 14)
   (sml-modeline-mode t)))

(defun ecfg--setup-tabbar ()
  (ecfg-install tabbar
   (require 'tabbar)
   (tabbar-mode t)
   (tabbar-mwheel-mode 0)

   (eval-after-load "tabbar"
     '(setq
       tabbar-separator (quote (0.7))
       tabbar-buffer-list-function 'ecfg--tabbar-buffer-list-function
       tabbar-buffer-groups-function 'ecfg--tabbar-buffer-groups))

   (add-hook 'nav-mode-hook (lambda () (tabbar-local-mode nil))))

  (defvar ecfg--tabbar-ignore-buffers '("^\s*\\*"))

  (defun ecfg--tabbar-buffer-list-function ()
    (remove-if
     (lambda (buffer)
       ;; Always include the current buffer.
       (and (not (eq (current-buffer) buffer))
            (loop for regex in ecfg--tabbar-ignore-buffers
                  thereis (string-match regex (buffer-name buffer)))))
     (buffer-list)))

  ;; combine .c and .cpp files together, .h - separately
  (defun ecfg--tabbar-buffer-groups ()
    (let ((current-ext (file-name-extension (or (buffer-file-name) ""))))
      (cond
       ((member current-ext '("c" "cpp" "cc")) '("C/C++"))
       ((member current-ext '("h" "hpp" "hh")) '("C/C++ headers"))
       (t (tabbar-buffer-groups))))))


(defun ecfg--setup-emacs-nav ()
  ;; todo find something better
  (ecfg-install nav
   (autoload 'nav-toggle "nav" "toggling nav window" t)
   (eval-after-load "nav"
     '(nav-disable-overeager-window-splitting))))


(defun ecfg--setup-color-theme ()
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" ecfg-dir))
  (setq x-underline-at-descent-line t)
  (load-theme 'ecfg-light t))
