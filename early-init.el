;; -*- lexical-binding: t; -*-

;; fix: GCC 15 on macOS ARM hides libemutls_w.a in a non-standard subdirectory
;; causing native compilation trampolines to fail with "library 'emutls_w' not found"
(when (eq system-type 'darwin)
  (let ((gcc-lib "/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin25/15"))
    (when (file-directory-p gcc-lib)
      (setenv "LIBRARY_PATH"
              (concat gcc-lib
                      (let ((existing (getenv "LIBRARY_PATH")))
                        (if existing (concat ":" existing)())))))))
