(defsystem trivial-download-test
  :author "Fernando Borretti"
  :license "MIT"
  :description "Tests for trivial-download"
  :depends-on (:trivial-download
               :fiveam
               :clack
               :clack-v1-compat)
  :components ((:module "t"
                :components
                ((:file "trivial-download")))))
