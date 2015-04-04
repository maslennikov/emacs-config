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
  (ecfg-install color-theme
   (ecfg--light-color-theme))
)


(defun ecfg--light-color-theme ()
  "Providing color theme scheme."

  (interactive)
  (ecfg--color-theme-base
   :foreground   "#0a0a0a"
   :background   "#ffffff"
   :border       "grey60"
   :toolbar      "grey90"
   :cursor       "#A6A6A3"
   :current-line "#ebf2fa"
   :selection    "#bfdbfa"
   :comment      "#8e908c"
   :red          "#c82829"
   :orange       "#f5871f"
   :brown        "#8C620E"
   :green        "#089909"
   :aqua         "#FFE5CF"
   :blue         "#255da5"
   :purple       "#8959a8"))


(defun ecfg--color-theme-base (&rest scheme)
  "Styling of basic emacs elements with the provided color scheme.
  `color-theme-initialize' must be already called by this moment
  "
  (ecfg-with-named-params
   scheme
   (:foreground :background :border :toolbar :cursor :current-line :selection
    :comment :red :orange :brown :green :aqua :blue :purple)
   nil

   (let
     ((frame-settings
       `((background . ,background)
         (background-mode . light)
         (border-color . ,cursor)
         (cursor-color . ,cursor)
         (foreground-color . ,foreground)))

      (face-settings
       `(
         ;; Built-in Emacs stuff
         (default (:background ,background :foreground ,foreground))
         (vertical-border (:foreground ,cursor))
         (fringe (:background ,background))
         (region (:background ,selection :foreground))
         (minibuffer-prompt (:foreground ,blue))
         (hl-line (:background ,current-line))

         ;; show-paren-mode
         (show-paren-match-face (:background ,blue :foreground ,background))
         (show-paren-mismatch-face (:background ,orange :foreground ,background))

         ;; mode-line related stuff
         (mode-line
          (:background ,toolbar :foreground ,foreground :weight light
                       :box (:line-width -1 :color ,cursor :style nil)))
         (mode-line-inactive
          (:background ,background :foreground ,foreground :weight light
                       :box (:line-width -1 :color ,cursor :style nil)))

         (sml-modeline-end-face (:foreground ,foreground :background ,toolbar))
         (sml-modeline-vis-face (:foreground ,toolbar :background ,border))

         ;; tabbar-mode
         (tabbar-default (:background ,toolbar :foreground ,foreground :box nil))
         (tabbar-unselected (:inherit tabbar-default :box nil))
         (tabbar-selected
          (:inherit tabbar-unselected :background ,background :foreground ,foreground :box nil))
         (tabbar-highlight (:background ,background :underline nil :box nil))
         (tabbar-button (:inherit tabbar-unselected :box nil))
         (tabbar-button-highlight (:inherit tabbar-button :inherit tabbar-highlight))
         (tabbar-separator (:inherit tabbar-default))

         ;; linum-mode
         (linum (:background ,toolbar :foreground ,foreground))

         ;; font-lock stuff
         (font-lock-comment-face (:foreground ,comment :slant italic))
         (font-lock-doc-face (:foreground ,comment))
         (font-lock-doc-string-face (:foreground ,comment))
         (font-lock-constant-face (:foreground ,green))
         (font-lock-string-face (:foreground ,green))
         (font-lock-function-name-face (:foreground ,foreground))
         (font-lock-keyword-face (:foreground ,blue))
         (font-lock-builtin-face (:foreground ,blue))
         (font-lock-type-face (:foreground ,purple))
         (font-lock-variable-name-face (:foreground ,foreground))
         (font-lock-warning-face (:foreground ,red))
         (isearch (:foreground ,background :background ,orange))
         (lazy-highlight (:foreground ,foreground :background ,aqua))
         (match (:foreground ,foreground :background ,selection))

         ;; generic outline mode (+ levels for org-mode)
         (outline-1 (:foreground ,foreground :weight bold))
         (outline-2 (:foreground ,blue))
         (outline-3 (:foreground ,green))
         (outline-4 (:foreground ,brown))
         (outline-5 (:foreground ,purple))
         (outline-6 (:foreground ,red))
         (outline-7 (:foreground ,orange))

         ;;hi-lock faces
         (hi-black-b (:weight bold :underline ,foreground))
         (hi-black-hb (:slant italic :weight bold :underline ,foreground))
         (hi-green (:background ,green))
         (hi-green-b (:foreground ,green :weight bold :underline ,green))
         (hi-blue (:background ,selection))
         (hi-blue-b (:foreground ,blue :weight bold :underline ,blue))
         (hi-red-b (:foreground ,red :weight bold :underline ,red))
         (hi-yellow (:background ,aqua))

         ;; compilation-mode
         (compilation-info (:foreground ,blue))
         (compilation-warning (:foreground ,orange))
         (compilation-error (:foreground ,red))

         ;; js2-mode
         (js2-function-param (:foreground ,foreground))
         (js2-external-variable (:foreground ,foreground :weight bold))
         (js2-warning (:underline ,red))
         (js2-error (:foreground ,red :weight bold))

         ;; org-mode
         (org-date (:foreground ,purple))
         (org-done (:foreground ,green))
         (org-hide (:foreground ,current-line))
         (org-link (:foreground ,blue))
         (org-todo (:foreground ,red)))))


   (defun format-face-descr (face)
     "Formatting face definition according to `defface' specification"
     `(,(car face) ((t ,(cadr face)))))

   ;; there seems to be a bug in `color-theme-canonic' handling FACES
   ;; differently in cases of presence and absence of VARIABLES
   ;; that's why we need to append the FACES list, not put it as an argument
   (color-theme-install
    (cons frame-settings
          (mapcar 'format-face-descr face-settings))))))
