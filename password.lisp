;; Handling pass entries here
(in-package #:stumpwm-pass-otp)

;; (defun defined (regex obj)
;;   (ppcre:register-groups-bind (field)
;;       (regex (slot-value obj 'raw) :sharedp t)
;;     field)
;;   )

(defclass password ()
  ((entry :accessor entry :type string :initarg :entry :initform (error "Must supply password entry"))
   raw
   lines))

(defmethod precache ((obj password))
  (setf (slot-value obj 'raw)
        (stumpwm:run-shell-command (format nil "pass show ~A" (entry obj)) t)

        (slot-value obj 'lines)
        (cl-ppcre:split "\\n" (slot-value obj 'raw))))

(defmethod display ((obj password))
  (stumpwm:message (slot-value obj 'raw)))

(defmethod passwd ((obj password))
 (car (slot-value obj 'lines)))

(defmethod uname ((obj password))
  (let ((defined (ppcre:register-groups-bind (uname)
                     ("username: (.*)" (slot-value pass 'raw) :sharedp t)
                   uname)))
    (if defined defined (car (last (cl-ppcre:split "/" (entry obj)))))))

(defmethod otp ((obj password))

  (stumpwm:run-shell-command (format nil "pass otp ~A" (entry obj)) t))

(defmethod initialize-instance :after ((obj password) &key)
  (precache obj))

(defmethod defined-field ((obj password) &rest regex)
  (ppcre:register-groups-bind (field)
      (regex (slot-value obj 'raw) :sharedp t)
    field)
  )

#||
;; working with the class
(setf pass (make-instance 'password :entry "icq.com/27708472"))

((defined-field pass) "username: (.*)")

(describe (defined-field pass "username: (.*)") )

(describe (uname pass))

(describe pass)
(entry pass)
(display pass)
(precache pass)
(passwd pass)
(princ-to-string (raw pass))


(describe (cl-ppcre:scan-to-strings "username: (.*)" (slot-value pass 'raw) :sharedp t ) )


(ppcre:register-groups-bind (uname)
    ("username: (.*)" (slot-value pass 'raw) :sharedp t)
  uname)

  ;; (print first))

(print (car (slot-value pass 'lines)) )
;; (defmethod initialize-instance (obj password)
;;   (call-method (pass obj) :pass "strpass") )
;; (((pass "s") pass) "bullocks")
||#
