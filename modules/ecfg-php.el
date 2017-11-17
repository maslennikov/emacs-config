;; -*- lexical-binding: t -*-

;;;###autoload
(defun ecfg-php-module-init ()

  (ecfg-install php-mode
    (autoload 'php-mode "php-mode" "Editing PHP" t)
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
    (add-hook 'php-mode-hook 'ecfg--php-hook)
    )
)
;;;###autoload (ecfg-auto-module "\\.php$" php)

(defun ecfg--php-hook ()
  (c-add-style
  "maslennikov-php"
  '("php"
    (c-basic-offset . 4)
    (tab-width . 4)
    (c-indent-comments-syntactically-p t)
    (fill-column . 80)
    (c-offsets-alist . (
      (arglist-close . php-lineup-arglist-close)
      (arglist-cont . (first php-lineup-cascaded-calls 0))
      (arglist-cont-nonempty . (first php-lineup-cascaded-calls c-lineup-arglist))
      (arglist-intro . php-lineup-arglist-intro)
      (case-label . 0)
      (class-open . 0)
      (comment-intro . 0)
      (inlambda . 0)
      (inline-open . 0)
      (namespace-open . 0)
      (lambda-intro-cont . +)
      (label . +)
      (statement-cont . php-lineup-hanging-semicolon)
      (substatement-open . 0)
      (topmost-intro-cont . (first php-lineup-cascaded-calls +))))
    ))

  (c-set-style "maslennikov-php")
  (add-hook 'post-self-insert-hook 'ecfg-two-dots nil t))
