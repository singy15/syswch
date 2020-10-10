(defsystem "syswch-cl"
  :version "0.1.0"
  :author "kedama"
  :license "MIT"
  :depends-on (
    "alexandria"
    "cl-ppcre"
    "hunchentoot"
    "easy-routes"
  )
  :components ((:module "src"
                :components (
                  (:file "syswch"))))
  :description ""
  :in-order-to ((test-op (test-op "syswch-cl/tests"))))

(defsystem "syswch-cl/tests"
  :author ""
  :license ""
  :depends-on ("syswch-cl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for syswch-cl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
