
A wrapper around `lftp` to easily PUT files to a SFTP server (*S*FTP).

For FTP, see also [cl-ftp](https://github.com/pinterface/cl-ftp).
Send files to FTP and SFTP servers, using the LFTP program.

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

~~~lisp
(make-profile :login xxx :server xxx :port xxx :password xxx)
~~~

the password is not revealed on stdout by default.

We have global variables if you use only one server on your Lisp image:

~~~lisp
*FTP-HOSTNAME*
*FTP-LOGIN*
~~~

But you can create profiles, that will hold connexion data.

~~~lisp
(make-profile :login xxx :server :port … :password …)
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

But you can also put all your credentials into a lispy file like this:

```lisp
;; creds.lisp-expr
(:server "psftp-dlc.centprod.com"
 :port 10022
 :login "pdTOBABELUJO"
 :password "sai9tieP"
 )
```

and use:

~~~lisp
(make-profile-from-plist plist)
~~~

so use `(uiop:read-file-form "creds.lisp-expr")` to read the file as a plist.

## Commands

To transfer a file with SFTP, use

~~~lisp
(sftp-transfer profile \"filename.txt\")
~~~

with optional arguments:

- `:cd` in order to 'cd' to a directory on the server
- `:dry-run` to *not* run the lftp command.

## Debug

You can see the sftp command with

~~~lisp
(put-command profile \"filename.txt\")
~~~


## Dev
### generate the doc

staple:

    (staple:generate :LFTP-WRAPPER :packages (list "LFTP-WRAPPER") :if-exists :overwrite :documents (list "README.markdown"))

---

licence: WTFPL.
