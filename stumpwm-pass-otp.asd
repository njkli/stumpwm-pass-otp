(asdf:defsystem #:stumpwm-pass-otp
  :serial t
  :description "Integrate 'pass' with StumpWM"
  :author "Voob of Doom <vod@njk.li>"
  :license "GPLv3"
  :depends-on (:stumpwm
               :quri
               :listopia
               :cl-ppcre
               :cl-string-match
               :cl-hash-util
               :cl-yaml
               :cl-one-time-passwords
               :assoc-utils
               :cl-base32)
  :components ((:file "stumpwm-pass-otp")
               (:file "password")))
