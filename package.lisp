(defpackage #:pass-otp
  (:use #:cl #:listopia #:cl-string-match #:stumpwm #:cl-hash-util)
  (:export *password-store-dir*
           *known-window-class-regex*
           *uri-regex*
           *xdotool-delay*
           *autotype-delay*
           *autotype-default*
           *autotype-regex-password*
           *autotype-regex-username*
           *field-regex-username*
           *field-regex-autotype*
           *field-regex-url*
           *pass-otp-menu-map*
           *pass-otp-entry-menu-map*
           *pinentry-listen-address*
           *pinentry-listen-port*
           entry-menu-action))
