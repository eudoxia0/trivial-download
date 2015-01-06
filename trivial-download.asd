(in-package :cl-user)
(defpackage trivial-download-asd
  (:use :cl :asdf))
(in-package :trivial-download-asd)

(defsystem trivial-download
  :version "0.2"
  :author "Fernando Borretti"
  :license "MIT"
  :homepage "https://github.com/eudoxia0/trivial-download"
  :depends-on (:drakma)
  :components ((:module "src"
                :components
                ((:file "trivial-download"))))
  :description "Download files from Common Lisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")))
