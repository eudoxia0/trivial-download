(in-package :cl-user)
(defpackage trivial-download-test
  (:use :cl :fiveam))
(in-package :trivial-download-test)

(def-suite downloads)
(in-suite downloads)

(defparameter +readme-pathname+
  (asdf:system-relative-pathname :trivial-download #p"README.md"))

(defparameter +readme-content+
  (uiop:read-file-string +readme-pathname+))

(defparameter +download-pathname+
  (asdf:system-relative-pathname :trivial-download #p"down.md"))

;;; The server will serve files from +tmp-directory+

(defparameter +server+
  (make-instance 'clack.middleware.static:<clack-middleware-static>
                 :path "/"
                 :root (asdf:component-pathname (asdf:find-system :trivial-download))))

(defparameter *server-handler* nil)

(defparameter +server-port+ 41111)

(test set-up
  (finishes
   (setf *server-handler*
         (clack:clackup (lack:builder +server+) :port +server-port+))))

(test (download-file :depends-on set-up)
  (finishes
   (trivial-download:download "http://localhost:41111/README.md"
                              +download-pathname+))
  (is-true
   (probe-file +download-pathname+))
  (is
   (equal +readme-content+
          (uiop:read-file-string +download-pathname+))))

(test (missing-file :depends-on set-up)
  (finishes
    (delete-file +download-pathname+))
  (signals trivial-download:http-error (trivial-download:download "http://localhost:41111/notafile"
                                                                  +download-pathname+))
  (is-false (probe-file +download-pathname+)))

(test (tear-down :depends-on set-up)
  (finishes
    (when (probe-file +download-pathname+)
      (delete-file +download-pathname+)))
  (finishes
   (clack:stop *server-handler*)))

(run! 'downloads)
