
(in-package :asdf-user)

(defsystem "lftp-wrapper"
  :version "0.1"
  :description "Wrapper around the lftp program, used for a SFTP client."
  :author "vindarel"
  :license "WTFPL"
  :homepage "https://github.com/vindarel/lftp-wrapper"
  :source-control (:git "https://github.com/vindarel/lftp-wrapper/")
  :bug-tracker "https://github.com/vindarel/lftp-wrapper/issues/"
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
