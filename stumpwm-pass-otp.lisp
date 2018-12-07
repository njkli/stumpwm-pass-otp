(defpackage #:stumpwm-pass-otp
  (:use #:cl #:listopia #:cl-string-match #:stumpwm)
  (:export *password-store-dir* *pass-otp-map* pass-otp))

(in-package #:stumpwm-pass-otp)

(defvar *uri-regex* "(?:(?:https?|ftp|file):\/\/|www\.|ftp\.)(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[-A-Z0-9+&@#\/%=~_|$?!:,.])*(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[A-Z0-9+&@#\/%=~_|$])")

(defvar *uri-regex-scanner*
  (cl-ppcre:create-scanner *uri-regex* :multi-line-mode t :case-insensitive-mode t))

(defvar *password-store-dir* (merge-pathnames #p".password-store/" (user-homedir-pathname)))

(defvar *pass-otp-map* nil )
;; (when (null *stumpwm-pass-otp-map*)
;;   (setf *stumpwm-pass-otp-map*
;;         (let ((m (stumpwm:make-sparse-keymap)))
;;           (stumpwm:define-key m (stumpwm:kbd "M-o") (lambda (x) (print "Hello!")))
;;           m)
;;         ))

(defun pass-entries ()
  (let ((home-ns-len (length (namestring *password-store-dir*))))
    (mapcar
     (lambda (entry)
       (let ((entry-ns (namestring entry)))
         (subseq entry-ns home-ns-len (- (length entry-ns) 4))))
     (directory (make-pathname :directory `(,@(pathname-directory *password-store-dir*)
                                              :wild-inferiors)
                               :name :wild
                               :type "gpg")))))

(defun matches (entry)
  (.filter (lambda (x) (string-contains-brute entry x)) (pass-entries)))

(defun known-window-class-p ()
  (let ((class (window-class (current-window))))
    (if (ppcre:scan "Firefox|Chromium" class) t nil)))

(defun domain (window-title-str)
  (let ((uri (cl-ppcre:scan-to-strings *uri-regex-scanner* window-title-str)))
    (quri:uri-domain (quri:uri uri))))

(defun format-menu (items)
  (mapcar (lambda (i) (list i i)) items))

(defun find-match ()
  (let ((title (window-title (current-window))))
    (if (and (known-window-class-p) (matches (domain title)))
        (matches (domain title))
        nil)))

(defun entry-display (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
    (display pass-obj)))

(defun entry-display-menu (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
    (display-menu pass-obj)))

;; FIXME: keymap in menu doesn't work for me, so these are here
(defvar *entry-display* nil)
(defvar *entry-autofill* nil)

(defcommand pass-otp () ()
  "show pass for current window"
  ;; (if (find-match)
  ;;     (setf msg (format nil "~{~A~^, ~}" (find-match)))
  ;;     (setf msg "Not found!"))
  ;; (message msg)
  (if (find-match)
      (let ((sel (select-from-menu
                  (current-screen)
                  (format-menu (find-match))
                  nil
                  0
                  ;; FIXME: can't pass keymap directly here
                  (let ((m (make-sparse-keymap)))
                    (define-key m (kbd "M-RET") (lambda (x)
                                                  (setf *entry-autofill* t)
                                                  (:menu-finish x)))
                    (define-key m (kbd "M-o") (lambda (x)
                                                (setf *entry-display* t)
                                                (:menu-finish x)))
                    m)
                  )))
        (when sel
          (cond (*entry-autofill* (progn
                                    (setf *entry-autofill* nil)
                                    (message "Autofill requested! ~A" (car sel))))
                (*entry-display* (progn
                                   (setf *entry-display* nil)
                                   (entry-display (car sel))))
                (t (entry-display-menu (car sel))))
          ))
      (message "No match found!")
      )
  )

(define-key *root-map* (kbd "s-p") "pass-otp")

#||

(password-store-insert "mytes.com/bullshit@user.cc" "somep")

(defcommand pass-copy () ()
  "Put a password into the clipboard."
  (let ((entry (completing-read (current-screen)
                                        "entry: "
                                        (pass-entries))))
    (run-shell-command (format nil "pass -c ~a" entry))))

(defcommand pass-generate () ()
  "Generate a password and put it into the clipboard"
  (let ((entry-name (read-one-line (stumpwm:current-screen)
                                           "entry name: ")))
(stumpwm:run-shell-command (format nil "pass generate -c ~a" entry-name))))
||#
