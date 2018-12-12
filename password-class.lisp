;; Handling pass entries here
(in-package #:pass-otp)

(defvar *autotype* (hash (":enter" "xdotool key Return")
                         (":tab" "xdotool key Tab")
                         (":space" "xdotool key space")))

(defvar *default-browser* nil)
(when (null *default-browser*) (setf *default-browser* "firefox"))

(defvar *xdotool-delay* nil)
(when (null *xdotool-delay*) (setf *xdotool-delay* 3))

(defvar *autotype-delay* nil)
(when (null *autotype-delay*) (setf *autotype-delay* 5))

(defun otpauth-to-hex (secret)
  (format nil "~{~2,'0x~}" (coerce (cl-base32:base32-to-bytes secret) 'list)))

(defclass password ()
  ((entry :accessor entry :type string :initarg :entry :initform (message "Must supply password entry"))
   raw
   lines
   autotype))

(defmethod precache ((obj password))
  (setf (slot-value obj 'raw)
        (run-shell-command (format nil "pass show ~A" (entry obj)) t)

        (slot-value obj 'autotype)
        (field-for obj "autotype: (.*)")

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
      (set-x-selection (cond ((cl-ppcre:scan ": " (car sel)) (cl-ppcre:register-groups-bind (field)
                                                              (": (.*)" (car sel) :sharedp t)
                                                            field))
                              ((cl-ppcre:scan "otpauth:" (car sel)) (otp obj))
                              (t (car sel))) :clipboard))))

(defmethod passwd ((obj password))
 (car (slot-value obj 'lines)))

(defmethod uname ((obj password))
  (let ((defined (field-for obj "username: (.*)")))
    (if defined defined (car (last (cl-ppcre:split "/" (entry obj)))))))

(defmethod url ((obj password))
  (let ((defined (field-for obj "url: (.*)")))
    (if defined
        defined
        (let ((obj-path (cl-ppcre:split "/" (entry obj)))
              (from-path nil))
          (setf from-path (nth (- (length obj-path) 2) obj-path))
          (quri:render-uri (quri.uri.http:make-uri-https :host from-path))))))

(defmethod otp ((obj password))
  (let ((defined (field-for obj "(otpauth:.*)")))
    (if defined
        (let ((secret (assoc-utils:aget (quri:uri-query-params (quri:uri defined)) "secret")))
          (write-to-string (cl-totp:totp (otpauth-to-hex secret))))
        (message (format nil "^B^1OTP undefined:~%^n~A" (entry obj))))))

(defmethod field-for ((obj password) &rest regex)
  (cl-ppcre:register-groups-bind (field)
      ((car regex) (slot-value obj 'raw) :sharedp t)
    field))

(defmethod autotype ((obj password))
  (let ((at-seq (cl-ppcre:split " " (slot-value obj 'autotype)))
        (xdt (format nil "xdotool type --delay ~A --clearmodifiers" *xdotool-delay*))
        (cmds nil))
    (dolist (at at-seq)
      (cond ((hash-get *autotype* (list at))
             (setf cmds (append cmds (list (hash-get *autotype* (list at))))))

            ((field-for obj (format nil "~A: (.*)" at))
             (setf cmds (append cmds (list (concat xdt " " (field-for obj (format nil "~A: (.*)" at))) ))))

            ((cl-ppcre:scan ":otp" at)
             (setf cmds (append cmds (list (concat xdt " " (otp obj))))))

            ((cl-ppcre:scan ":delay" at)
             (setf cmds (append cmds (list (format nil "sleep ~A" *autotype-delay*)))))

            ((cl-ppcre:scan "pass" at)
             (setf cmds (append cmds (list (concat xdt " " (passwd obj))))))

            ((cl-ppcre:scan "user" at)
             (setf cmds (append cmds (list (concat xdt " " (uname obj))))))

            (t
             (setf cmds (append cmds (list (concat xdt " " at)))))
            ))
    (run-shell-command (format nil "~{~a~^ && ~}" cmds))))

(defmethod initialize-instance :after ((obj password) &key)
  (precache obj))
