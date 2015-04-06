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

(setq
 ;; enable git shallow clone to save time and bandwidth
 el-get-git-shallow-clone t

 ;; tell el-get to look into local customizations for every package into
 ;; `~/.emacs.d/init-<package>.el'
 el-get-user-package-directory user-emacs-directory

 ;; making the el-get logging verbose
 ;; WARNING: makes init slower
 ;; el-get-verbose t

 ;; turning them for core moduels off
 el-get-use-autoloads nil
 )

;; Sometimes, we need to experiment with our own recipe, or override the
;; default el-get recipe to get around bugs.
(add-to-list 'el-get-recipe-path (expand-file-name "el-get-recipes" ecfg-dir))


;; Defining the helper macro to be used instead of `el-get-bundle'. The reason
;; behind it is to get rid of `el-get-bundle' caching of the init code in the
;; `el-get-bundle-init-directory'
(defmacro ecfg-install (package &rest hook)
  "Install the given PACKAGE via el-get running an optional
after-install HOOK forms"
  `(let ((el-get-sources '((:name ,package :after (progn ,@hook)))))
     (el-get 'sync ',package)))

(defmacro ecfg-with-local-autoloads (&rest body)
  "Ecexutes BODY after building (if not present) and loading the
autoloads in the `default-directory'. The reason behind this is
not using the standard el-get autoloads. Sometimes, for large
packages, it's too awkward to configure autoloads manually in the
hooks of `ecfg-install'."
  `(let ((loaddefs (expand-file-name "ecfg-local-loaddefs.el" default-directory)))
    (unless (file-exists-p loaddefs)
      (let ((generated-autoload-file loaddefs))
        (message "ECFG: Generating local autoloads: %s" loaddefs)
        (update-directory-autoloads default-directory)))
    (load-file loaddefs)
    ,@body))
