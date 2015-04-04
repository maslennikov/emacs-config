;; -*- lexical-binding: t -*-
;;
;; Entry init script for Emacs configuration set-up `ecfg'
;;

(defvar ecfg-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "ecfg root directory")
(defvar ecfg-plugin-dir (expand-file-name "plugins" ecfg-dir)
  "ecfg directory for static plugins, i.e. not installed with el-get")
(defvar ecfg-user-dir (expand-file-name "ecfg" user-emacs-directory)
  "ecfg user directory, mainly for generated stuff")

(unless (file-directory-p ecfg-plugin-dir)
  (make-directory ecfg-plugin-dir 'recursive))

(unless (file-directory-p ecfg-user-dir)
  (make-directory ecfg-user-dir 'recursive))

(add-to-list 'load-path ecfg-plugin-dir)
(let ((default-directory ecfg-plugin-dir))
  (normal-top-level-add-subdirs-to-load-path))


(defun ecfg-module-init-hook (modulename)
  (intern (format "ecfg-%s-module-init" modulename)))

(defun ecfg-load-module (filename modulename)
  "Loads init file, where FILENAME is a full path to the loaded file.
After the file is loaded, `ecfg' tries to run its init function
named `ecfg-MODULENAME-module-init' if it's defined.
"
  (let ((init-hook (ecfg-module-init-hook modulename)))
    (load-file filename)
    (if (fboundp init-hook)
        (funcall init-hook)
      (message "No module init-hook found: %s" init-hook))))


(defun ecfg-run-init-scripts (dir)
  "Loads every script in the given DIR thats name matches the
following regexp: \"^[0-9][0-9]-ecfg-\\(.+\\)\.el$\".
"
  (let ((regexp "^\\([0-9][0-9]-\\)?ecfg-\\(.+\\)\\.el$"))
    (dolist (filename (directory-files dir))
      (when (string-match regexp filename)
        (ecfg-load-module (expand-file-name filename dir)
                          (match-string 2 filename))))))


;;; Loading core initialization scripts

(ecfg-run-init-scripts (expand-file-name "init.d" ecfg-dir))

;;; Loading additional modules

(defmacro ecfg-auto-module (pattern module)
  "Autoload helper for the ecfg-modules. Utilizes the
auto-mode-alist to trigger the autoload of the module."
  (let ((init-hook (ecfg-module-init-hook module))
        (auto-hook-name (intern (format "ecfg--auto-module-hook-%s" module))))

    `(progn
       ;; check missing init hook early: on loading the loaddefs
       (unless (fboundp ',init-hook)
         (error "No auto-module init-hook found: %s" ',init-hook))

       ;; defining the auto-module init hook
       (defun ,auto-hook-name ()
         ;; removing itself from the auto-mode-alist upon the first call
         (setq auto-mode-alist
               (rassq-delete-all ',auto-hook-name auto-mode-alist))
         ;; running the module init-hook triggering the module autoload
         (,init-hook)
         ;; hoping that in hook the proper auto-mode alist entry was inserted
         (set-auto-mode))

       ;; registering the auto-module hook to the aut-mode alist
       (add-to-list 'auto-mode-alist '(,pattern . ,auto-hook-name)))))

;;; load all modules for the first time triggering el-get to install all stuff
;;; we need; for the subsequent runs the generated autoloads will be picked-up
(require 'autoload)

(let* ((ecfg-module-dir (expand-file-name "modules" ecfg-dir))
       (generated-autoload-file (expand-file-name "loaddefs.el" ecfg-user-dir)))

  ;; directory containing autoloads should be included because loaddefs has
  ;; relative paths
  (add-to-list 'load-path ecfg-user-dir)
  ;; (add-to-list 'load-path ecfg-module-dir)

  (if (file-exists-p generated-autoload-file)
      ;; already been there, just load loaddefs
      (load-file generated-autoload-file)
    ;; making the first run, churn up all modules to trigger el-get installs
    (ecfg-run-init-scripts ecfg-module-dir)
    (update-directory-autoloads ecfg-module-dir)))

;; todo: set title to file-name-nodirectory
;; todo: autoloads for different modules like ecfg-org.el
