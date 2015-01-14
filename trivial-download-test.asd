(defsystem trivial-download-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:trivial-download
               :fiveam
               :clack)
  :components ((:module "t"
                :components
                ((:file "trivial-download")))))
