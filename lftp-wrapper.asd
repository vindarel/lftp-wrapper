
(in-package :asdf-user)

(defsystem "lftp-wrapper"
  :version "0.1"
  :depends-on (:secret-values
               :str
               :termp
               :trivial-types
               :log4cl
               )
  :components ((:module "src"
                :components
                ((:file "lftp"))))
  )
