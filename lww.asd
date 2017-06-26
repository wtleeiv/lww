;;;; lww.asd

(asdf:defsystem #:lww
  :description  "Lisp Web Writer: html, css, javascript file writing utility for common lisp"
  :author "Tyler Lee wtleeiv@gmail.com"
  :license "MIT License"
  :serial t
  :depends-on (#:uiop
               #:cl-ppcre
               #:spinneret)
  :components ((:file "package")
               (:file "lww")))

