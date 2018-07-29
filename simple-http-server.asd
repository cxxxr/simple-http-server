(require "comm")

(defsystem "simple-http-server"
  :depends-on ("babel")
  :serial t
  :components ((:file "package")
               (:file "variables")
               (:file "util")
               (:file "simple-http-server")))
