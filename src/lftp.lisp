(uiop:define-package :lftp-wrapper
    (:use :cl)
  (:documentation "Send files to FTP and SFTP servers, using the LFTP program.

Prerequesites:

    apt install lftp

the lftp program handles many protocols, and allows to easily use SFTP on the command line (including giving the password on the command, which otherwise is a bit tedious).

Nice to have:

create a ~/.lftprc file with:

debug
set sftp:auto-confirm 1

this answers \"yes\" to accept connections.

Usage:

(make-profile :login xxx :server xxx :port xxx :password xxx)

the password is not revealed on stdout by default.


We also use a few environment variables to find LFT credentials:

LFTP_LOGIN
LFTP_PORT
etc for server and password.

We also use files to read the credentials, if we don't find env vars:

LFTP_LOGIN.txt
etc

so you can create a profile with only

    (make-profile)

or with

    (make-profile-from-plist plist)

to create it from a plist.


To transfer a file with SFTP, use

    (sftp-transfer profile \"filename.txt\")

with optional arguments:

- :cd in order to 'cd' to a directory on the server
- :dry-run


You can debug the sftp command with

    (put-command profile \"filename.txt\")

"))

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
  ((hostname :initarg :hostname
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
    (with-slots (hostname login port server password) p
      (format stream "hostname: ~s, login: ~a, port: ~a, server: ~a, password? ~a" hostname login port server
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

(defgeneric make-profile-from-plist (plist)
  (:documentation "Create a PROFILE instance with slots taken from PLIST. PLIST has the keys :server, :port, :login, :password etc.")
  (:method (plist)
    (when (and (consp plist)
               (trivial-types:property-list-p plist))
      (make-instance 'profile
                     :login (getf plist :login)
                     :server (getf plist :server)
                     :password (getf plist :password)
                     :port (getf plist :port)))))

(defgeneric put-command (profile filename &key cd)
  (:documentation "run lftp for the sftp protocol and PUT a file on the server. If CD is given (a string), first CD into the remote directory.")
  (:method (profile filename &key cd)
    (format nil "lftp -p ~a sftp://~a:~a@~a  -e  \"~a put ~a ; bye\""
                                  (port profile)
                                  (login profile)
                                  ;; (secret-values:reveal-value password)
                                  (server profile)
                                  (if *show-passwords*
                                      (print-password profile :stream nil)
                                      "*****")
                                  (if cd
                                      (format nil "cd ~a ;" cd)
                                      "")
                                  filename)))

(defgeneric sftp-transfer (profile filename &key dry-run)
  (:documentation "Send FILENAME via STFP, using the login credentials in PROFILE."))

(defmethod sftp-transfer ((profile profile) filename &key (dry-run *dry-run*))
  "If *dry-run* is t, don't actually run the LFTP command.

  If *devel* is t, handle any error and re-signal it, so the developper gets the interactive debugger. When *devel* is NIL (the default), the error is logged on stderr."
  (let* ((local-filename (local-filename filename))
         (cmd (put-command profile local-filename)))

    (log:info cmd)

    (handler-case
        ;; strangely cmd:$cmd didn't work, used the server address in lowercase?
        (if dry-run
            (format t "~&dry-run: not sending the SFTP command.~&")

            ;; TODO: if a bad sftp command, it could stay up a long
            ;; time on the terminal. Need timeout.
            (uiop:run-program cmd :error-output t))

      (error (c)
        (log:error "LFTP error for file ~a" filename)
        (format *error-output* "~a" c)
        (when *devel*
          (error c))
        ))))
