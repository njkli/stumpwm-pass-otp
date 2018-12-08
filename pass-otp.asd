(asdf:defsystem #:pass-otp
  :serial t
  :description "passwordstore.org integration for StumpWM"
  :author "Voob of Doom <vod@njk.li>"
  :license "GPLv3"
  :depends-on (:stumpwm
               :quri
               :listopia
               :cl-ppcre
               :cl-string-match
               :cl-hash-util
               :cl-one-time-passwords
               :assoc-utils
               :cl-base32)
  :components ((:file "package")
               (:file "pass-otp")
               (:file "password-class")))