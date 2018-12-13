(in-package #:pass-otp)

(defvar *pinentry-listen-address* nil)
(when (null *pinentry-listen-address*) (setf *pinentry-listen-address* "127.0.0.1"))

(defvar *pinentry-listen-port* nil)
(when (null *pinentry-listen-port*) (setf *pinentry-listen-port* 65530))

(defun write-address-for-shell-counterpart ()
  (let ((address-file (merge-pathnames #p"pinentry-stumpwm.env" (concat (getenv "XDG_RUNTIME_DIR") "/"))))
    (with-open-file (fh address-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (format fh "PINENTRY_HOST=~A~%PINENTRY_PORT=~A~%" *pinentry-listen-address* *pinentry-listen-port*))))

(defun pinentry-handler (stream)
  (let ((description (percent:decode (read-line stream)))
        (prompt (read-line stream)))
    (format stream (or (stumpwm:read-one-line (stumpwm:current-screen)
                                              (format nil "~a~%~a " description prompt)
                                              :password t)
                       ""))))

(defun pinentry-init ()
  (write-address-for-shell-counterpart)
  (usocket:socket-server *pinentry-listen-address*
                         *pinentry-listen-port*
                         #'pinentry-handler nil
                         :in-new-thread t
                         :multi-threading t))

(if *INITIALIZING* (pinentry-init))
