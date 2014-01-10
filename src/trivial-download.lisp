(in-package :cl-user)
(defpackage trivial-download
  (:use :cl)
  (:export :file-size
           :download))
(in-package :trivial-download)

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defun file-size (url)
  "Take a URL to a file, return the size (in bytes)."
  (handler-case
      (parse-integer
       (cdr
        (assoc :content-length
               (third (multiple-value-list
                       (drakma:http-request url :want-stream t :method :head))))))
    (t () nil)))

(defparameter +size-symbol-map+
  (list (cons 1000000000000 "TB")
        (cons 1000000000 "GB")
        (cons 1000000 "MB")
        (cons 1000 "kB")
        (cons 1 "B")))

(defun human-file-size (size)
  "Take a file size (in bytes), return it as a human-readable string."
  (let ((pair (loop for pair in +size-symbol-map+
                    if (>= size (car pair)) return pair)))
    (format nil "~f ~A" (/ size (car pair)) (cdr pair))))

(defun percentage (total-bytes current-bytes)
  (floor (/ (* current-bytes 100) total-bytes)))

(defun download (url output)
  "Download a file and save it to a pathname."
  (with-open-file (file output
                      :direction :output
                      :if-does-not-exist :create
                      :if-exists :supersede
                      :element-type '(unsigned-byte 8))
    (let* ((file-size (file-size url))
           (input (drakma:http-request url
                                       :want-stream t))
           (byte-count 0)
           (last-percentage 0))
      (format t "Downloading ~S (~A)~&" url (if file-size
                                                (human-file-size file-size)
                                                "Unknown size"))
      (awhile (read-byte input nil nil)
        (write-byte it file)
        (incf byte-count)
        (if file-size
            (let ((progress (percentage file-size byte-count)))
              (if (> progress last-percentage)
                  (if (eql 0 (mod progress 10))
                      (format t "~D%" progress)
                      (format t ".")))
              (setf last-percentage progress))))
    (close input))))
