;; based on github.com/bbatsov/solarized-emacs
;; see also github.com/chriskempson/tomorrow-theme
;;
;; -*- lexical-binding: t -*-


;;; Utilities, taken from bbatsov/solarized-emacs

(defun ecfg-color-name-to-rgb (color)
  "Convert COLOR string to a list of normalized RGB components.
COLOR should be a color name (e.g. \"white\") or an RGB triplet
string (e.g. \"#ff12ec\").  Normally the return value is a list
of three floating-point numbers, (RED GREEN BLUE), each between
0.0 and 1.0 inclusive.  If current frame cannot display COLOR,
return nil.
"
  ;; `colors-values' maximum value is either 65535 or 65280 depending on the
  ;; display system. So we use a white conversion to get the max value.
  (let ((valmax (float (car (color-values "#ffffff")))))
    (mapcar (lambda (x) (/ x valmax)) (color-values color))))

(defun ecfg-color-rgb-to-hex (red green blue)
  "Return hexadecimal notation for the color RED GREEN BLUE.
RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive.
"
  (format "#%02x%02x%02x" (* red 255) (* green 255) (* blue 255)))

(defun ecfg-color-blend (fg bg alpha)
  "Blends FG onto BG with ALPHA. FG and BG should
be color names (e.g. \"white\") or RGB triplet strings
(e.g. \"#ff12ec\").  Alpha should be a float between 0 and 1.
Returns string with hex notation of the resulting color.
"
  (apply 'ecfg-color-rgb-to-hex
         (mapcar* '(lambda (fg-part bg-part)
                       (+ (* alpha fg-part) (* bg-part (- 1 alpha))))
                    (ecfg-color-name-to-rgb fg)
                    (ecfg-color-name-to-rgb bg))))


;;; Color theme generation

(defun ecfg-create-theme (theme-name palette &optional is-dark)
  "Create a theme named THEME-NAME given the PALETTE alist,
providing a clue if it IS-DARK theme."

  (defun ecfg--palette-color (color &optional default)
    (let ((col (or (cdr (assq color palette)) default)))
      (unless col
        (error "Color `%s' not found in the palette: %s" color palette))
      col))

  ;; NOTE Only use in exceptional cirmumstances!
  (defun ecfg--hc (color &optional alpha)
    "High-color variant of the given COLOR"
    (ecfg-color-blend color (ecfg--palette-color 'fg-hc) (or alpha 0.7)))

  ;; NOTE Only use in exceptional cirmumstances!
  (defun ecfg--lc (color &optional alpha)
    "Low-color variant of the given COLOR"
    (ecfg-color-blend color (ecfg--palette-color 'bg-base) (or alpha 0.25)))

  (defun ecfg--theme-set-faces (faces)
    "Formatting face definitions according to `defface' specification"
    (let ((class '((class color) (min-colors 89))))
      (apply 'custom-theme-set-faces
             theme-name
             (mapcar (lambda (face) `(,(car face) ((,class ,(cadr face)))))
                     faces))))

;;; Color palette
  (let* ((fg-base (ecfg--palette-color 'fg-base))
         (fg-hc (ecfg--palette-color 'fg-hc))
         (fg-lc (ecfg--palette-color 'fg-lc))
         (bg-base (ecfg--palette-color 'bg-base))
         (bg-hc (ecfg--palette-color 'bg-hc))

         (yellow  (ecfg--palette-color 'yellow))
         (orange  (ecfg--palette-color 'orange))
         (red     (ecfg--palette-color 'red))
         (violet  (ecfg--palette-color 'violet))
         (blue    (ecfg--palette-color 'blue))
         (cyan    (ecfg--palette-color 'cyan))
         (green   (ecfg--palette-color 'green))

         ;; logical parts
         (cursor (ecfg--lc fg-lc 0.8))
         (modeline-bg (ecfg--lc cursor))

         (sel-primary-bg (ecfg--palette-color 'selection (ecfg--hc bg-hc 0.9)))
         (sel-secondary-bg (ecfg--lc cyan))
         (match-hc-bg orange)
         (match-lc-bg (ecfg--lc orange))

         (tooltip-bg bg-hc)
         (tooltip-fg cyan)
         (tooltip-sel-bg sel-secondary-bg))

;;; Theme Faces
    (ecfg--theme-set-faces
     `(
;;;; basic coloring
       (default (:foreground ,fg-base :background ,bg-base))
       (vertical-border (:foreground ,cursor))

       (cursor (:foreground ,bg-base :background ,cursor :inverse-video t))
       ;; (mouse (:foreground ,bg-base :background ,cursor :inverse-video t))
       (hl-line (:background ,bg-hc))
       (hl-line-face (:background ,bg-hc))

       (region (:background ,sel-primary-bg :foreground unspecified))
       (secondary-selection (:background ,sel-secondary-bg))

       (fringe (:foreground ,fg-lc :background ,bg-base))
       (linum (:foreground ,fg-lc :background ,bg-base))

       (shadow (:foreground ,fg-lc))

;;;; mode-line, minibuffer
       (minibuffer-prompt (:foreground ,blue))
       (ido-subdir (:foreground ,blue))

       (mode-line
        (:foreground ,fg-base
         :background ,modeline-bg
         :box (:line-width -1 :color ,cursor :style unspecified)))

       (mode-line-inactive
        (:foreground ,fg-base
         :background ,bg-base
         :box (:line-width -1 :color ,cursor :style unspecified)))

       (mode-line-buffer-id (:weight bold))

       (sml-modeline-end-face (:foreground ,fg-base :background ,modeline-bg))
       (sml-modeline-vis-face (:foreground ,modeline-bg :background ,cursor))

;;;; header, tabbar
       (header-line (:inherit mode-line :box nil))
       (tabbar-default (:background ,modeline-bg :foreground ,fg-base :box nil :height 0.8 :inherit variable-pitch))
       (tabbar-unselected (:inherit tabbar-default :box nil))
       (tabbar-selected (:inherit tabbar-unselected :background ,bg-base :foreground ,fg-base :box nil))
       (tabbar-highlight (:background ,bg-base :underline nil :box nil))
       (tabbar-button (:inherit tabbar-unselected :box nil))
       (tabbar-button-highlight (:inherit tabbar-button :inherit tabbar-highlight))
       (tabbar-separator (:inherit tabbar-default))

;;;; highlighting
       (show-paren-match-face (:background ,blue :foreground ,bg-base))
       (show-paren-mismatch-face (:background ,orange :foreground ,bg-base))

       (highlight (:background ,(ecfg--lc yellow)))
       (match (:foreground ,fg-hc :background ,match-lc-bg))
       (isearch (:foreground ,bg-base :background ,match-hc-bg))
       (isearch-fail (:foreground ,fg-base :background ,(ecfg--lc red)))
       (lazy-highlight (:inherit match :background unspecified :foreground unspecified))

;;;; hi-lock-mode
       (hi-black-b (:foreground ,fg-hc :background ,bg-base :weight bold))
       (hi-black-hb (:foreground ,fg-hc :background ,bg-hc :weight bold))
       (hi-green (:background ,(ecfg--lc green)))
       (hi-green-b (:foreground ,green :weight bold))
       (hi-blue (:background ,(ecfg--lc blue)))
       (hi-blue-b (:foreground ,blue :weight bold))
       (hi-red-b (:foreground ,red :weight bold))
       (hi-yellow (:background ,(ecfg--lc yellow)))
       (hi-pink (:background ,(ecfg--lc violet)))

;;;; font-lock stuff
       (font-lock-comment-face (:foreground ,fg-lc :slant italic))
       (font-lock-doc-face (:foreground ,fg-lc))
       (font-lock-doc-string-face (:foreground ,fg-lc))
       (font-lock-constant-face (:foreground ,green))
       (font-lock-string-face (:foreground ,green))
       (font-lock-function-name-face (:foreground ,fg-base))
       (font-lock-keyword-face (:foreground ,blue))
       (font-lock-builtin-face (:foreground ,blue))
       (font-lock-type-face (:foreground ,violet))
       (font-lock-variable-name-face (:foreground ,fg-base))
       ;; (font-lock-preprocessor-face (:foreground ,blue))
       (font-lock-warning-face (:foreground ,red :weight normal))

       (trailing-whitespace (:background ,(ecfg--lc red)))
       (success (:foreground ,green))
       (warning (:foreground ,orange))
       (error (:foreground ,red))

;;;; completions
       (completions-annotations (:foreground ,fg-lc))

       (ac-candidate-face (:background ,bg-hc :foreground ,cyan))
       (ac-selection-face (:background ,(ecfg--lc cyan) :foreground ,(ecfg--hc cyan)))
       (ac-candidate-mouse-face (:background ,(ecfg--hc cyan) :foreground ,(ecfg--lc cyan)))
       (ac-completion-face (:foreground ,fg-hc :underline t))
       (ac-gtags-candidate-face (:background ,bg-hc :foreground ,blue))
       (ac-gtags-selection-face (:background ,(ecfg--lc blue) :foreground ,(ecfg--hc blue)))
       (ac-yasnippet-candidate-face (:background ,bg-hc :foreground ,yellow))
       (ac-yasnippet-selection-face (:background ,(ecfg--lc yellow) :foreground ,(ecfg--hc yellow)))

       (company-template-field (:background ,yellow :foreground ,bg-hc))
       (company-tooltip (:background ,tooltip-bg :foreground ,tooltip-fg))
       (company-tooltip-common (:inherit company-tooltip :foreground ,fg-hc))
       (company-tooltip-selection (:background ,tooltip-sel-bg :foreground ,fg-hc))
       (company-tooltip-common-selection (:inherit company-tooltip-selection :foreground ,fg-hc))
       (company-tooltip-mouse (:inherit company-tooltip-selection :inverse-video t))
       (company-tooltip-annotation (:foreground ,fg-hc :background ,bg-hc))
       (company-scrollbar-fg (:background ,tooltip-fg :foreground ,bg-hc))
       (company-scrollbar-bg (:background ,tooltip-sel-bg))
       (company-preview (:inherit company-tooltip))
       (company-preview-common (:inherit company-tooltip-common-selection :underline t))

;;;; popup
       (popup-face (:background ,bg-hc :foreground ,fg-base))
       (popup-isearch-match (:background ,yellow :foreground ,bg-base))
       (popup-menu-face (:background ,bg-hc :foreground ,fg-base))
       (popup-menu-mouse-face (:background ,blue :foreground ,fg-base))
       (popup-menu-selection-face (:background ,violet :foreground ,bg-base))
       (popup-scroll-bar-background-face (:background ,fg-lc))
       (popup-scroll-bar-foreground-face (:background ,fg-hc))
       (popup-tip-face (:background ,bg-hc :foreground ,fg-base))

;;;; generic outline mode (+ levels for org-mode)
       (outline-1 (:inherit default :foreground ,fg-hc :weight bold))
       (outline-2 (:inherit default :foreground ,blue :weight bold))
       (outline-3 (:inherit default :foreground ,green :weight bold))
       (outline-4 (:inherit default :foreground ,violet :weight bold))
       (outline-5 (:inherit default :foreground ,fg-lc :weight bold))
       (outline-6 (:inherit default :foreground ,fg-lc :weight bold))
       (outline-7 (:inherit default :foreground ,fg-lc :weight bold))
       (outline-8 (:inherit default :foreground ,fg-lc :weight bold))

;;;; org-mode
       (org-document-title (:foreground ,fg-hc  :weight bold))
       (org-document-info (:foreground ,fg-base))

       (org-ellipsis (:foreground ,fg-lc))
       (org-link (:foreground ,blue))
       (org-footnote (:inherit org-link :foreground ,fg-lc))
       (org-date (:foreground ,violet))
       (org-code (:foreground ,fg-lc))

       (org-todo (:foreground ,red :weight normal))
       (org-done (:foreground ,fg-lc :weight normal))

       (org-agenda-dim-todo-face (:foreground ,fg-lc))
       (org-agenda-restriction-lock (:background ,yellow))
       (org-clock-overlay (:background ,yellow))
       (org-mode-line-clock-overrun (:inherit mode-line :background ,red))


;;;; js2-mode colors
       (js2-function-param (:foreground ,fg-base))
       (js2-error (:foreground ,red :weight bold))
       (js2-warning (:underline ,red))
       (js2-external-variable (:foreground ,fg-base :weight bold))

       (js2-jsdoc-tag (:foreground ,(ecfg-color-blend cyan fg-lc 0.25)))
       (js2-jsdoc-value (:foreground ,(ecfg-color-blend yellow fg-lc 0.25)))


;;;; compilation
       (compilation-info (:foreground ,blue :underline nil :bold nil))
       (compilation-info-face (:foreground ,blue :underline nil))
       (compilation-line-number (:foreground ,fg-lc))
       (compilation-error (:inherit error :underline nil))
       (compilation-error-face (:foreground ,red : :underline nil))
       (compilation-warning (:inherit warning :underline nil))
       (compilation-warning-face (:foreground ,orange))

;;;; helm
       (helm-source-header
        (:inherit default
         :background ,bg-base
         :foreground ,fg-hc
         :weight bold
         :underline nil
         :font-family nil
         :height 1.0))
       (helm-match-item (:inherit match))
       (helm-match (:foreground ,(ecfg--hc red)))
       (helm-grep-match (:inherit helm-match))
       (helm-buffer-not-saved (:foreground ,orange))
       (helm-buffer-saved-out (:foreground ,red :background ,bg-base :inverse-video t))
       (helm-buffer-size (:foreground ,fg-lc))
       (helm-candidate-number (:background ,bg-hc :foreground ,fg-hc :bold t))
       (helm-ff-directory (:background ,bg-base  :foreground ,blue))
       (helm-ff-dotted-directory (:inherit helm-ff-directory :background nil  :foreground nil))
       (helm-ff-file (:background ,bg-base :foreground ,fg-base))
       (helm-ff-executable (:foreground ,green))
       (helm-ff-symlink (:foreground ,cyan))
       (helm-ff-prefix (:inherit match))
       (helm-grep-file (:inherit helm-ff-symlink :underline nil))
       (helm-moccur-buffer (:inherit helm-grep-file))
       (helm-grep-finish (:foreground ,green))
       (helm-grep-lineno (:foreground ,fg-lc))
       (helm-grep-running (:foreground ,red))
       (helm-header (:inherit header-line))
       (helm-lisp-completion-info (:foreground ,fg-base))
       (helm-lisp-show-completion (:foreground ,yellow  :background ,bg-hc :bold t))
       (helm-M-x-key (:foreground ,orange :underline t))
       (helm-selection (:background ,bg-hc :underline t))
       (helm-selection-line (:background ,bg-hc :foreground ,fg-hc :underline nil))
       (helm-separator (:foreground ,red))
       (helm-visible-mark (:background ,bg-base :foreground ,violet :bold t))

;;; helm-css-scss
       (helm-css-scss-target-line-face (:inherit match))
       (helm-css-scss-selector-depth-face-1 (:inherit helm-grep-file))
       (helm-css-scss-selector-depth-face-2 (:inherit helm-grep-file))
       (helm-css-scss-selector-depth-face-3 (:inherit helm-grep-file))
       (helm-css-scss-selector-depth-face-4 (:inherit helm-grep-file))
       (helm-css-scss-selector-depth-face-5 (:inherit helm-grep-file))
       (helm-css-scss-selector-depth-face-6 (:inherit helm-grep-file))

;;;; markdown-mode
       (markdown-header-delimiter-face (:foreground ,fg-lc))
       (markdown-header-rule-face (:foreground ,fg-lc))
       (markdown-header-face (:foreground ,fg-hc :weight bold))
       (markdown-header-face-1 (:inherit outline-1))
       (markdown-header-face-2 (:inherit outline-2))
       (markdown-header-face-3 (:inherit outline-3))
       (markdown-header-face-4 (:inherit outline-4))
       (markdown-header-face-5 (:inherit outline-5))
       (markdown-header-face-6 (:inherit outline-6))
       (markdown-url-face (:foreground ,fg-lc))

;;;; yasnippet
       (yas-field-highlight-face (:inherit secondary-selection))

;;;; undo-tree
       (undo-tree-visualizer-default-face (:foreground ,fg-lc))
       (undo-tree-visualizer-unmodified-face (:foreground ,blue))
       (undo-tree-visualizer-current-face (:foreground ,red :weight bold))

;;;; xml, html, web-mode, zencoding
       (zencoding-preview-input (:background ,bg-hc :box ,fg-hc))
       (nxml-element-local-name (:foreground ,blue))
       (web-mode-current-column-highlight-face (:background ,bg-hc))
       (web-mode-current-element-highlight-face (:background ,bg-hc))
       (web-mode-param-name-face (:inherit web-mode-html-attr-name-face))

;;;; END custom-theme-set-faces
       ))


;;; Theme Variables
    (custom-theme-set-variables
     theme-name

;;;; ansi-colors
     `(ansi-color-names-vector
       [,bg-hc ,red ,green ,yellow ,blue ,violet ,cyan ,fg-base])

;;;; compilation
     `(compilation-message-face 'default)

;;;; fill-column-indicator
     `(fci-rule-color ,bg-hc)

;;;; END custom-theme-set-variables
     )

;;; END ecfg-create-theme
    ))


;;; Providing the theme

(deftheme ecfg-light "The light variant of the Solarized colour theme")
(ecfg-create-theme
 'ecfg-light
 ;;;; Solarized theme
 ;; '(;; emphasized content
 ;;   (fg-hc .   "#586e75")
 ;;   ;; primary content
 ;;   (fg-base . "#657b83")
 ;;   ;; comments, secondary content
 ;;   (fg-lc .   "#93a1a1")
 ;;   ;; background highlight
 ;;   (bg-hc .   "#eee8d5")
 ;;   ;; background light
 ;;   (bg-base . "#fdf6e3")

 ;;   ;; Solarized accented colors
 ;;   (yellow  . "#b58900")
 ;;   (orange  . "#cb4b16")
 ;;   (red     . "#dc322f")
 ;;   (magenta . "#d33682")
 ;;   (violet  . "#6c71c4")
 ;;   (blue    . "#268bd2")
 ;;   (cyan    . "#2aa198")
 ;;   (green   . "#859900"))

 ;;;; Tomorrow theme
 '(;; emphasized content
   (fg-hc .   "#050505")
   ;; primary content
   (fg-base . "#0a0a0a")
   ;; comments, secondary content
   (fg-lc .   "#8e908c")
   ;; background highlight
   (bg-hc .   "#ebf2fa")
   ;; background light
   (bg-base . "#ffffff")

   ;; accented colors
   (yellow  . "#eab700")
   (orange  . "#f5871f")
   (red     . "#dc322f")
   (violet  . "#8959a8")
   (blue    . "#255da5")
   (cyan    . "#3e999f")
   (green   . "#089909"))

 )

(provide-theme 'ecfg-light)
(enable-theme 'ecfg-light)
