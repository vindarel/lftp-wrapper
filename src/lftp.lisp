(uiop:define-package :lftp-wrapper
    (:use :cl)
  (:documentation "
")
  (:export
   #:*devel*
   #:*show-passwords*
   #:*dry-run*
   #:*ftp-hostname*
   #:*ftp-login*
   #:*version*
   #:profile
   #:sftp-profile
   #:hostname
   #:login
   #:port
   #:server
   #:password
   #:print-password
   #:make-profile
   #:make-profile-from-plist
   #:run
   #:build-command
   #:put
   #:command
   ))

(in-package :lftp-wrapper)

(defparameter *devel* t
  "If t, get FTP interactive errors")

(defparameter *show-passwords* nil
  "If non nil, print passwords in clear text on stdout when debugging commands.")

(defparameter *dry-run* nil
  "If T, don't send the SFTP command.")

(defparameter *ftp-hostname* ""
  "(s)FTP hostname to connect to (string).")

(defparameter *ftp-login* ""
  "(s)FTP login (string).")

(defvar *version* #.(asdf:system-version (asdf:find-system :lftp-wrapper)))

(defclass profile ()
  ((protocol :initarg :protocol
             :initform nil
             :accessor protocol)
   (hostname :initarg :hostname
             :initform *ftp-hostname*
             :accessor hostname)
   (login :initarg :login
          :initform *ftp-login*
          :accessor login)
   (port :initarg :port
         :initform nil
         :accessor port)
   (server :initarg :server
           :initform ""
           :accessor server)
   (password :initarg :password
             :initform ""
             :accessor password
             :documentation "The password is concealed. To see it, call `show-password' on a profile."))
  (:documentation "LFTP profile."))

(defclass sftp-profile (profile)
  ()
  (:default-initargs
   :protocol "sftp"))

(defmethod print-password ((p profile) &key (stream t))
  "Print the password, revealed. "
  ;; nil is printed "NIL".
  (format stream "~a" (secret-values:ensure-value-revealed (password p))))

(defun make-profile (&key login server port password)
  "Create a PROFILE instance by searching for credentials in environment variables or files.

  The password is also read, but concealed."
  (make-instance 'profile
                 :login (or login (find-ftp-login))
                 :server (or server (find-ftp-server))
                 :password (secret-values:conceal-value (or password (find-ftp-password))
                                                        :name "SFTP password")
                 :port (or port (find-ftp-port))))


(defmethod print-object ((p profile) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (protocol hostname login port server password) p
      (format stream "protocol: ~s, hostname: ~s, login: ~s, port: ~s, server: ~s, password? ~s"
              protocol hostname login port server
              (if password t nil)))))

(defun find-ftp-login ()
  (or (uiop:getenv "LFTP_LOGIN")
      (when (uiop:file-exists-p "LFTP_LOGIN.txt")
        (str:trim (uiop:read-file-string "LFTP_LOGIN.txt")))))

(defun find-ftp-password ()
  (or (uiop:getenv "LFTP_PASSWORD")
      (when (uiop:file-exists-p "LFTP_PASSWORD.txt")
        (str:trim (uiop:read-file-string "LFTP_PASSWORD.txt")))))

(defun find-ftp-port ()
  (or (uiop:getenv "LFTP_PORT")))

(defun find-ftp-server ()
  (or (uiop:getenv "LFTP_SERVER")))

(defun error-or-quit (msg)
  (if (termp:termp :force t)
      (progn
        (when msg
          (format *error-output* "~a" msg))
        (uiop:quit 1))
      (error (or msg "Error"))))

(defgeneric local-filename (filename)
  (:documentation "Return a path where FILENAME is located on the filesystem. By default, use FILENAME.")
  (:method (filename)
    filename))

(defgeneric make-profile-from-plist (plist &key protocol)
  (:documentation "Create a PROFILE instance with slots taken from PLIST. PLIST has the keys :server, :port, :login, :password etc.")
  (:method (plist &key (protocol "sftp"))
    (when (and (consp plist)
               (trivial-types:property-list-p plist))
      (make-instance 'profile
                     :protocol (or (getf plist :protocol) protocol)
                     :login (getf plist :login)
                     :server (getf plist :server)
                     :password (getf plist :password)
                     :port (getf plist :port)))))

(defclass command ()
  ())

(defclass put (command)
  ((cd :initarg :cd
       :initform nil
       :accessor cd
       :documentation "If set, CD into a directory on the target before PUT-ing or getting files.")
   (local-filename :initarg :local-filename
                   :initform ""
                   :accessor local-filename
                   :documentation "The filename on the host filesystem to send.")))

(defmethod print-object ((o command) stream)
  (print-unreadable-object (o stream :type t :identity t)))

(defmethod print-object ((o put) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (with-slots (cd local-filename) o
      (format stream "cd: ~a, filename: ~s" cd local-filename))))

(defgeneric build-command (profile command &key dry-run)
  (:documentation "run lftp for the sftp protocol.

  If DRY-RUN is non nil, include the password in clear text and really run the command.

  Currently done for PUT commands.")

  (:method (profile command &key dry-run)
    (declare (ignore dry-run))
    (error "build-command is not implement for a command of class ~a, but only for PUT. We are certainly not far though..." (type-of command)))

  (:method (profile (command put) &key (dry-run *dry-run*))
    ;; Example PUT command:
    ;; lftp -p 400 sftp://user:*****@test.org -e \" put file.txt ; bye\"
    (format nil "lftp -p ~a ~a://~a:~a@~a  -e  \"~a put ~a ; bye\""
                                  (port profile)
                                  (protocol profile)
                                  (login profile)
                                  ;; (secret-values:reveal-value password)
                                  (if (or *show-passwords* (not dry-run))
                                      (print-password profile :stream nil)
                                      "*****")
                                  (server profile)
                                  (if (cd command)
                                      (format nil "cd ~a ;" (cd command))
                                      "")
                                  (local-filename command))))

(defgeneric run (profile command &key dry-run)
  (:documentation "Run COMMAND, using the login credentials in PROFILE.

  If the output is:

   NIL
   NIL
   0

  then the command went fine.

  Implemented to PUT files with sftp.

  If *dry-run* is t, don't actually run the LFTP command.

  If *devel* is t, handle any error and re-signal it, so the developper gets the interactive debugger. When *devel* is NIL (the default), the error is logged on stderr."))

(defmethod run ((profile profile) command &key (dry-run *dry-run*))
  (let* ((local-filename (local-filename command))
         (cmd (build-command profile command :dry-run dry-run)))
    (log:info "Our lftp command is: ~a" cmd)
    (handler-case
        ;; strangely cmd:$cmd didn't work, used the server address in lowercase?
        (if dry-run
            (format t "~&dry-run: not sending the SFTP command.~&")

            ;; TODO: if a bad sftp command, it could stay up a long
            ;; time on the terminal. Need timeout.
            ;; XXX: we could catch stdout and the 0 exit code for success.
            (uiop:run-program cmd :error-output t))

      (error (c)
        (log:error "LFTP error for file ~a" local-filename)
        (format *error-output* "~a" c)
        (when *devel*
          (error c))
        ))))
