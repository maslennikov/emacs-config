;; -*- lexical-binding: t -*-
;;
;; Entry init script for Emacs configuration set-up `ecfg'
;;


(defvar ecfg-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "ecfg root directory")
(defvar ecfg-plugin-root (expand-file-name "plugins" ecfg-dir)
  "ecfg directory for static plugins, i.e. not installed with el-get")

(unless (file-directory-p ecfg-plugin-root) (make-directory ecfg-plugin-root 'recursive))

(add-to-list 'load-path ecfg-plugin-root)
(let ((default-directory ecfg-plugin-root))
  (normal-top-level-add-subdirs-to-load-path))


(defun ecfg-load-module (filename modulename)
  "Loads init file, where FILENAME is a full path to the loaded file.
After the file is loaded, `ecfg' tries to run its init function
named `ecfg-MODULENAME-module-init' if it's defined.
"
  (let ((init-hook (intern (format "ecfg-%s-module-init" modulename))))

    ;; ensure el-get-sources is empty before loading "ome-.+\.org" files
    (setq el-get-sources nil)
    ;; enable git shallow clone to save time and bandwidth
    (setq el-get-git-shallow-clone t)

    (load-file filename)
    (if (fboundp init-hook)
        (funcall init-hook)
      (message "No module init-hook found: %s" init-hook))))


(defun ecfg-run-init-scripts (dir)
  "Loads every script in the given DIR thats name matches the
following regexp: \"^[0-9][0-9]-ecfg-\\(.+\\)\.el$\".
"
  (let ((regexp "^[0-9][0-9]-ecfg-\\(.+\\)\\.el$"))
    (dolist (filename (directory-files dir))
      (when (string-match regexp filename)
        (ecfg-load-module (expand-file-name filename dir)
                          (match-string 1 filename))))))

;; load all modules
(ecfg-run-init-scripts (expand-file-name "init.d" ecfg-dir))

;; todo: set title to file-name-nodirectory
;; todo: autoloads for different modules like ecfg-org.el
