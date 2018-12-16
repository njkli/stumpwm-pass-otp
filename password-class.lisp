(in-package #:pass-otp)

(defvar *autotype* (hash (":enter" "xdotool key Return")
                         (":tab" "xdotool key Tab")
                         (":space" "xdotool key space")))

(defvar *autotype-regex-username* nil)
(when (null *autotype-regex-username*) (setf *autotype-regex-username* "user"))

(defvar *autotype-regex-password* nil)
(when (null *autotype-regex-password*) (setf *autotype-regex-password* "pass"))

(defvar *field-regex-username* nil)
(when (null *field-regex-username*) (setf *field-regex-username* "username: (.*)"))

(defvar *field-regex-url* nil)
(when (null *field-regex-url*) (setf *field-regex-url* "url: (.*)"))

(defvar *field-regex-autotype* nil)
(when (null *field-regex-autotype*) (setf *field-regex-autotype* "autotype: (.*)"))

(defvar *xdotool-delay* nil)
(when (null *xdotool-delay*) (setf *xdotool-delay* 3))

(defvar *autotype-delay* nil)
(when (null *autotype-delay*) (setf *autotype-delay* 5))

(defvar *autotype-default* nil)
(when (null *autotype-default*) (setf *autotype-default* "user :tab pass :enter"))

(defvar *pass-otp-entry-menu-map* nil)
(when (null *pass-otp-entry-menu-map*)
  (setf *pass-otp-entry-menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "M-RET") (entry-menu-action :field-autotype))
          (define-key m (kbd "RET") (entry-menu-action :field-copy))
          m)))

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
        (or (field-for obj *field-regex-autotype*) *autotype-default*)

        (slot-value obj 'lines)
        (cl-ppcre:split "\\n" (slot-value obj 'raw))))

(defmethod display ((obj password))
  (message (slot-value obj 'raw)))

;; TODO: scan-to-string instead of group bind
(defmethod field-filter ((obj password) &rest str)
  (cond ((cl-ppcre:scan ": " (car str)) (cl-ppcre:register-groups-bind (field)
                                            (": (.*)" (car str) :sharedp t)
                                          field))
        ((cl-ppcre:scan "otpauth:" (car str)) (otp obj))
        (t (car str))))

(defmethod field-copy ((obj password) &rest str)
  (set-x-selection (field-filter obj (car str)) :clipboard))

(defmethod field-autotype ((obj password) &rest str)
  (run-shell-command
   (format
    nil
    "xdotool type --delay ~A --clearmodifiers '~A'"
    *xdotool-delay*
    (field-filter obj (car str)))))

(defmethod display-menu ((obj password))
  (multiple-value-bind (action choice)
      (select-from-menu
       (current-screen)
       (format-menu (slot-value obj 'lines))
       (format nil " [ ~A ]" (slot-value obj 'entry))
       0
       *pass-otp-entry-menu-map*)
    (case action
      (:field-autotype
       (field-autotype obj (car choice)))
      (:field-copy
       (field-copy obj (car choice))))))

(defmethod passwd ((obj password))
  (car (slot-value obj 'lines)))

(defmethod uname ((obj password))
  (let ((defined (field-for obj *field-regex-username*)))
    (if defined defined (car (last (cl-ppcre:split "/" (entry obj)))))))

(defmethod url ((obj password))
  (let ((defined (field-for obj *field-regex-url*)))
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
        nil)))

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
             (setf cmds (append cmds
                                (list (hash-get *autotype* (list at))))))
            ((field-for obj (format nil "~A: (.*)" at))
             (setf cmds (append cmds
                                (list (concat xdt
                                              " "
                                              (format nil "'~A'"
                                                      (field-for obj (format nil "~A: (.*)" at))))))))
            ((cl-ppcre:scan ":otp" at)
             (setf cmds (append cmds
                                (list (concat xdt " " (otp obj))))))
            ((cl-ppcre:scan ":delay" at)
             (setf cmds (append cmds
                                (list (format nil "sleep ~A" *autotype-delay*)))))
            ((cl-ppcre:scan *autotype-regex-password* at)
             (setf cmds (append cmds
                                (list (concat xdt " " (format nil "'~A'" (passwd obj)))))))
            ((cl-ppcre:scan *autotype-regex-username* at)
             (setf cmds (append cmds
                                (list (concat xdt " " (format nil "'~A'" (uname obj)))))))
            (t
             (setf cmds (append cmds
                                (list (concat xdt " " (format nil "'~A'" at))))))))
    (run-shell-command (format nil "~{~a~^ && ~}" cmds))))

(defmethod qr-code-show ((obj password))
  (run-shell-command (format nil "pass otp uri -q ~A" (entry obj))))

(defmethod qr-code-generate ((obj password))
  (let ((fn (concat (getenv "XDG_RUNTIME_DIR") "/qr.png")))
    (run-commands (format nil "screenshot-window ~A" fn))
    (run-shell-command (format nil "zbarimg -q --raw ~A | pass otp append ~A && rm -rf ~A" fn (entry obj) fn))))

(defmethod qr-code ((obj password))
  (let ((otp (otp obj)))
    (if otp
        (qr-code-show obj)
        (qr-code-generate obj))))

(defmethod initialize-instance :after ((obj password) &key)
  (precache obj))
