(in-package :cl-user)
(defpackage trivial-download
  (:use :cl)
  (:export :*chunk-size*
           :file-size
           :with-download
           :with-download-progress
           :download
           :http-error
           :it))
(in-package :trivial-download)

(defparameter *chunk-size* 256
  "Files are downloaded in chunks of this many bytes.")

(define-condition http-error (error)
  ((code :initarg :code :accessor response-code))
  (:report (lambda (c s)
             (format s "HTTP error, response code ~S." (response-code c)))))

(defun http-request (url &rest args)
  (let* ((vals (multiple-value-list (apply #'drakma:http-request url args)))
         (code (second vals)))
    (unless (= 200 code)
      (error 'http-error :code code))
    (values-list vals)))

(defun file-size (url)
  "Take a URL to a file, return the size (in bytes)."
  (handler-case
      (parse-integer
       (cdr
        (assoc :content-length
               (third (multiple-value-list
                       (http-request url :method :head))))))
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
                    if (or (>= size (car pair))
                           (= (car pair) 1))
                    return pair)))
    (format nil "~f ~A" (/ size (car pair)) (cdr pair))))

(defun percentage (total-bytes current-bytes)
  (if (= current-bytes 0)
      100
      (floor (/ (* current-bytes 100) total-bytes))))

(defmacro with-download (url (file-size total-bytes-read array stream &key quiet)
                         &body body)
  "Execute body at every chunk that is downloaded."
  `(let* ((,file-size (file-size ,url))
          (,total-bytes-read 0)
          (,array (make-array *chunk-size* :element-type '(unsigned-byte 8)))
          (,stream (http-request ,url
                                 :want-stream t)))
     (unless quiet
       (format t "Downloading ~S (~A)~&" ,url (if ,file-size
                                                  (human-file-size ,file-size)
                                                  "Unknown size")))
     (finish-output nil)
     ;; We read the file in `*chunk-size*`-byte chunks by using `read-sequence`
     ;; to fill `array`. The return value of `read-sequence`, in this context,
     ;; is the number of bytes read. we know we've reached the end of file when
     ;; the number of bytes read is less than `*chunk-size*`
     (loop do
       (let ((bytes-read-this-chunk (read-sequence ,array ,stream)))
         (incf ,total-bytes-read bytes-read-this-chunk)
         ,@body
         (if (< bytes-read-this-chunk *chunk-size*)
             (return))))
     (close ,stream)))

(defmacro with-download-progress (url (file-size total-bytes-read array stream &key quiet)
                                  &body body)
  "Like with-download but with a progress bar."
  (alexandria:with-gensyms (last-percentage progress)
    `(let ((,last-percentage 0))
       (with-download ,url (,file-size ,total-bytes-read ,array ,stream :quiet ,quiet)
         (progn
           (if ,file-size
               (let ((,progress (percentage ,file-size ,total-bytes-read)))
                 (if (> ,progress ,last-percentage)
                     (progn
                       (if (eql 0 (mod ,progress 10))
                           (format t "~D%" ,progress)
                           (format t "."))
                       (finish-output nil)))
                 (setf ,last-percentage ,progress)))
           ,@body)))))

(defun download (url output &key quiet)
  "Download a file and save it to a pathname. Directories containing `output`
are created if they don't exist."
  (ensure-directories-exist (uiop:pathname-directory-pathname output))
  (with-open-file (file output
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede
                        :element-type '(unsigned-byte 8))
    (if quiet
        (with-download url (file-size total-bytes-read array stream :quiet quiet)
          (write-sequence array file :end bytes-read-this-chunk))
        (with-download-progress url (file-size total-bytes-read array stream
                                               :quiet quiet)
          (write-sequence array file :end bytes-read-this-chunk)))))
