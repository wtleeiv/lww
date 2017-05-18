;;;; lww.lisp

(in-package #:lww)

(defvar *file-write-regex* "(.+)_(\\w{2,4})\\.lisp")
(defvar *directory-end-slash-regex* "(.*)(\\w+)$")

(defun unsaved-warning (dotfile)
  (when (cl-ppcre:scan *file-write-regex* (namestring dotfile))
    (format t "** ~a is unsaved~%" dotfile)
    (lex:echo-line "** consider saving it and writing web files again")))

(defun write-page (lisp-file page-name)
  (with-open-file (web-stream page-name :direction :output :if-exists :supersede)
    (let ((soc:*soc* web-stream))
       (load lisp-file))))

(defun write-web (lisp-file web-file force-write)
  (flet ((write-it (lisp web)
           (format t "lisp: ~a~%" lisp)
           (format t "web: ~a~%~%" web)
           (write-page lisp web)))
    (cond
      ((lex:dotfile-p lisp-file)
       (unsaved-warning lisp-file))
      (force-write
       (write-it lisp-file web-file))
      ((not (uiop/filesystem:file-exists-p web-file))
       (write-it lisp-file web-file))
      ((> (file-write-date lisp-file) (file-write-date web-file))
       (write-it lisp-file web-file)))))

(defun write-file (lisp-path rewrite)
  (let ((lisp-file (namestring lisp-path)))
    (multiple-value-bind (web-file matched) (cl-ppcre:regex-replace *file-write-regex* lisp-file "\\1\.\\2")
      (if matched
          (write-web lisp-file web-file rewrite)))))

(defun write-files (dir force)
  ;; TODO load 'common.lisp' here if exists (using dir)
  (mapcar (lambda (x) (write-file x force)) (uiop/filesystem:directory-files dir "*.lisp"))
  (mapcar (lambda (x) (write-files x force)) (uiop/filesystem:subdirectories dir)))

;; run this
(defun write-app (dir &optional (force nil))
  (flet ((vet-dir ()
           (cl-ppcre:regex-replace *directory-end-slash-regex* dir "\\1\\2\/")))
    (write-files (vet-dir) force)))
