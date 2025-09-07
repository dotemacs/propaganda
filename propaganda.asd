(defsystem "propaganda"
  :version "0.1.0"
  :author "Aleksandar Simic"
  :license "BSD"
  :description "A Lisp propaganda feed aggregator and Mastodon poster"
  :homepage "https://github.com/dotemacs/propaganda"
  :depends-on (#:dexador
               #:feeder
               #:uiop
               #:local-time
               #:cl-ppcre
               #:arrows
               #:puri
               #:plump
               #:cl-redis)
  :components ((:file "propaganda"))
  :build-operation "program-op"
  :build-pathname "propaganda"
  :entry-point "propaganda:main")
