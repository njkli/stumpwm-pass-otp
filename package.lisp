(defpackage #:pass-otp
  (:use #:cl #:listopia #:cl-string-match #:stumpwm #:cl-hash-util)
  (:export *password-store-dir*
           *known-window-class-regex*
           *uri-regex*
           *pass-otp-map*
           *xdotool-delay*
           *autotype-delay*))
