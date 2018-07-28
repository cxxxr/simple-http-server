(require "comm")

(defsystem "simple-http-server"
  :serial t
  :components ((:file "package")
               (:file "variables")
               (:file "util")
               (:file "simple-http-server")))
