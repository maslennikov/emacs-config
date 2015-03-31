;; Setting up el-get. This should be done during emacs start-up as early as
;; possible.

(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

;; (let ((package-load-list '((el-get t))))
  ;; (package-initialize))

;; forbid package.el initializing anything automatically since we use el-get
(setq package-enable-at-startup nil)

;; lazy download el-get during the first start-up
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)

  (require 'el-get)
  ;; this will cause el-get to download the freshest version of itself
  (el-get 'sync)

  ;; build melpa packages for el-get
  ;; (el-get-elpa-build-local-recipes)
)

;; enable git shallow clone to save time and bandwidth
(setq el-get-git-shallow-clone t)

;; Sometimes, we need to experiment with our own recipe, or override the
;; default el-get recipe to get around bugs.
(add-to-list 'el-get-recipe-path (expand-file-name "el-get-recipes" ecfg-dir))

;; tell el-get to look into local customizations for every package into
;; `~/.emacs.d/init-<package>.el'
(setq el-get-user-package-directory user-emacs-directory)
