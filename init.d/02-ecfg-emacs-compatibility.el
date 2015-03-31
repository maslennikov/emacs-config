;; -*- lexical-binding: t -*-

;; Some workaround for emacs version < 24.0, thanks Silthanis@github.
(if (< emacs-major-version 24)
    (defun file-name-base (&optional filename)
      "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
      (file-name-sans-extension
       (file-name-nondirectory (or filename (buffer-file-name))))))

(when (version< emacs-version "24.3")
  (el-get 'sync '(cl-lib))
  (add-to-list 'load-path
               (expand-file-name "el-get/cl-lib" user-emacs-directory)))
