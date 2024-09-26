
(in-package :asdf-user)

(defsystem "lftp-wrapper"
  :version "0.1"
  :depends-on (:secret-values
               :termp
               )
  :components ((:module "src"
                :components
                ((:file "lftp"))))

  :build-operation "program-op"
  :build-pathname "observatoire"
  :entry-point "observatoire::main"
  )
