;; Handling pass entries here
(in-package #:stumpwm-pass-otp)

(defun otpauth-to-hex (secret)
  (format nil "~{~2,'0x~}" (coerce (cl-base32:base32-to-bytes secret) 'list)))

(defclass password ()
  ((entry :accessor entry :type string :initarg :entry :initform (message "Must supply password entry"))
   raw
   lines))

(defmethod precache ((obj password))
  (setf (slot-value obj 'raw)
        (run-shell-command (format nil "pass show ~A" (entry obj)) t)
        (slot-value obj 'lines)
        (cl-ppcre:split "\\n" (slot-value obj 'raw))))

(defmethod display ((obj password))
  (message (slot-value obj 'raw)))

(defmethod display-menu ((obj password))
  (let  ((sel (select-from-menu
               (current-screen)
               (format-menu (slot-value obj 'lines))
               "set-x-selection:"
               0
               )))
    (when sel
      ;; TODO: check if clipboard-history module loaded before blatantly doing stuff to it
      ;; clipboard-history uses polling, so what I do here is useless
      ;; (clipboard-history:stop-clipboard-manager)
      ;; (clipboard-history:start-clipboard-manager)
      (set-x-selection (cond ((ppcre:scan ": " (car sel)) (ppcre:register-groups-bind (field)
                                                              (": (.*)" (car sel) :sharedp t)
                                                            field))
                              ((ppcre:scan "otpauth:" (car sel)) (otp obj))
                              (t (car sel))) :clipboard))))

(defmethod passwd ((obj password))
 (car (slot-value obj 'lines)))

(defmethod uname ((obj password))
  (let ((defined (field-for obj "username: (.*)")))
    (if defined defined (car (last (cl-ppcre:split "/" (entry obj)))))))

(defmethod otp ((obj password))
  (let ((defined (field-for obj "(otpauth:.*)")))
    (if defined
        (let ((secret (assoc-utils:aget (quri:uri-query-params (quri:uri defined)) "secret")))
          (write-to-string (cl-totp:totp (otpauth-to-hex secret))))
        (message (format nil "^B^1OTP undefined:~%^n~A" (entry obj))))))

(defmethod field-for ((obj password) &rest regex)
  (ppcre:register-groups-bind (field)
      ((car regex) (slot-value obj 'raw) :sharedp t)
    field))

(defmethod initialize-instance :after ((obj password) &key)
  (precache obj))

#||
(message (format nil "^B^1*Error In Command '^b~a^B': ^nOther shit" "some" ))

;; working with the class
(setf pass (make-instance 'password :entry "github.com/voipscout"))
(setf pass (make-instance 'password :entry "icq.com/27708472"))

(otp pass)
(uname pass)
(describe pass)
(entry pass)
(display pass)
(precache pass)
(passwd pass)

(princ-to-string (raw pass))

||#
