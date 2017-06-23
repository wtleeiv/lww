;;;; lww.lisp

(in-package #:lww)

(defvar *file-write-regex* "(.+)_(\\w{2,4})\\.lisp")
(defvar *directory-end-slash-regex* "(.*)(\\w+)$")

(defun dotdir-p (dir)
  (flet ((check-dotdir (dir-path)
           (let* ((parent-path (uiop/pathname:pathname-parent-directory-pathname dir-path))
                 (dir-name (namestring (uiop/pathname:enough-pathname dir-path parent-path)))
                 (first-char (char dir-name 0)))
             (char= #\. first-char))))
    (if (pathnamep dir)
        (check-dotdir dir)
        (check-dotdir (pathname dir)))))

(defun dotfile-p (file)
  (let* ((filename (file-namestring file))
         (first-char (char filename 0)))
    (princ (char= #\. first-char))
    (char= #\. first-char)))

(defun unsaved-warning (dotfile)
  (when (cl-ppcre:scan *file-write-regex* (namestring dotfile))
    (format t "** ~a is unsaved~%" dotfile)
    (format t "** consider saving it and writing web files again~%")))

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
      ((dotfile-p lisp-file)
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
  (unless (dotdir-p dir)
    (break)
    (let ((load-file (uiop/pathname:merge-pathnames* (pathname dir) (make-pathname :name "common" :type "lisp"))))
      (when (uiop/filesystem:file-exists-p load-file)
        (load load-file)))
    (mapcar (lambda (x) (write-file x force)) (uiop/filesystem:directory-files dir "*.lisp"))
    (mapcar (lambda (x) (write-files x force)) (uiop/filesystem:subdirectories dir))))

;; run this
(defun write-app (dir &optional (force nil))
  (flet ((vet-dir ()
           (cl-ppcre:regex-replace *directory-end-slash-regex* dir "\\1\\2\/")))
    (write-files (vet-dir) force)))
