(in-package :cl-user)
(defpackage trivial-download-asd
  (:use :cl :asdf))
(in-package :trivial-download-asd)

(defsystem trivial-download
  :version "0.2"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:drakma)
  :components ((:module "src"
                :components
                ((:file "trivial-download"))))
  :description "Download files from Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op trivial-download-test))))
