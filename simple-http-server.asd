(require "comm")

(defsystem "simple-http-server"
  :serial t
  :components ((:file "package")
               (:file "simple-http-server")))
