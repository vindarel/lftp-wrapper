
A wrapper around `lftp` to easily send files to a SFTP server.

While you can very well format a command line yourself, we ship
utilities to create profiles and assemble commands.

For FTP, see also [cl-ftp](https://github.com/pinterface/cl-ftp) which works very well.

**status**: the original copy is running in production©, we are moving things around in this one and things might break. It is also limited, but extensible.

~~~lisp
CL-USER> (use-package :lftp-wrapper)  ;; optional, or:
CL-USER> (uiop:add-package-local-nickname :lftp :lftp-wrapper)

CL-USER> (defvar profile (make-profile-from-plist (uiop:read-file-form "CREDS.lisp-expr"))
#<PROFILE protocol: "sftp", login: "user", port: 10022, server: "prod.com", password? T>

CL-USER> (defvar command (put :cd "I/" :local-filename "data.csv"))
#<PUT cd: "I/", filename: "data.csv" {1007153883}>

CL-USER> (run profile command)
…
success!
~~~


## Prerequisites

    apt install lftp

the lftp program handles many protocols, and allows to easily use SFTP on the command line (including giving the password on the command, which otherwise is a bit tedious).

Nice to have:

create a ~/.lftprc file with:

```
debug
set sftp:auto-confirm 1
```

this answers \"yes\" to accept new servers connections.

## Usage

Create a profile:

~~~lisp
(make-profile :login xxx :server xxx :port xxx :password xxx)
~~~

- it defaults to the "sftp" :protocol
- the password is not revealed on stdout by default.

We have global variables if you use only one profile during development in your Lisp image:

~~~lisp
*ftp-server*
*ftp-login*
~~~

We also use a few environment variables to find the credentials:

    LFTP_LOGIN
    LFTP_PORT
    etc for server and password

We also use files to read the credentials, if we don't find env vars:

    LFTP_LOGIN.txt
    etc

so you can create a profile with only

~~~lisp
(make-profile)
~~~

and it looks up the environment variables and then the files.

But you can also put all your credentials into a lispy file like this:

```lisp
;; creds.lisp-expr
(:server "abc.prod.com"
 :port 10022
 :login "user"
 :password "****"
 :protocol "sftp"
 )
```

and use:

~~~lisp
(make-profile-from-plist (uiop:read-file-form "creds.lisp-expr"))
~~~

## Commands

Our OO ceremony is to easily create commands like

    lftp -p 400 sftp://user:*****@test.org -e " put file.txt ; bye"

Obviously you can format the string yourself and run it with uiop:run-program, but you would loose out on publishing some code on GitHub.

Create commands:

~~~lisp
(defvar put (make-instance 'put :cd "I/" :local-filename "test.txt"))
~~~

Currently we have a generic `command` class and only the `put` child.


## Run commands

Run commands for a profile:

~~~lisp
(run profile put)
~~~

>Note: it is currently only implemented for `put` commands.  Why? Because of needs-driven development. You can extend the behaviour though, see below.

with optional arguments:

- `:dry-run` to *not* run the lftp command. See also `*dry-run*`.

## Extend for your own commands

So if you feel the present use case is pretty limited, you can create
a class of yours and create a method for `build-command`:

~~~lisp
(defclass my-command (command)
  ())

(defmethod build-command (profile (command my-command) &key (dry-run *dry-run*))
  (format nil "echo building sftp command for user ~a" (login profile)))
~~~

now you can call `run` with a profile object and your command.


## Debug and inspect commands

You can see the sftp command with

~~~lisp
(build-command profile put)
~~~


---

licence: WTFPL.

@vindarel 2024-2099, running Common Lisp in production©
