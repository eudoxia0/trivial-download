(defsystem trivial-download
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.3"
  :homepage "https://github.com/eudoxia0/trivial-download"
  :bug-tracker "https://github.com/eudoxia0/trivial-download/issues"
  :source-control (:git "git@github.com:eudoxia0/trivial-download.git")
  :depends-on (:drakma)
  :components ((:module "src"
                :components
                ((:file "trivial-download"))))
  :description "Download files from Common Lisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")))
