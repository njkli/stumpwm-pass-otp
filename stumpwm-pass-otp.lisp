(defpackage #:stumpwm-pass-otp
  (:use #:cl #:listopia #:cl-string-match #:stumpwm #:cl-hash-util)
  (:export *password-store-dir*
           *known-window-class-regex*
           *uri-regex*
           *pass-otp-map*
           *autotype-delay*
           *xdotool-delay*
           ;; pass-otp
           ;; pass-otp-show-all
           ))

(in-package #:stumpwm-pass-otp)

(defvar *password-store-dir* (merge-pathnames #p".password-store/" (user-homedir-pathname)))
(defvar *known-window-class-regex* "Firefox|Chromium")
(defvar *uri-regex* "(?:(?:https?|ftp|file):\/\/|www\.|ftp\.)(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[-A-Z0-9+&@#\/%=~_|$?!:,.])*(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[A-Z0-9+&@#\/%=~_|$])")

(defvar *uri-regex-scanner*
  (cl-ppcre:create-scanner *uri-regex* :multi-line-mode t :case-insensitive-mode t))

(defvar *pass-otp-map* nil)
;; (when (null *stumpwm-pass-otp-map*)
;;   (setf *stumpwm-pass-otp-map*
;;         (let ((m (stumpwm:make-sparse-keymap)))
;;           (stumpwm:define-key m (stumpwm:kbd "M-o") (lambda (x) (print "Hello!")))
;;           m)
;;         ))

(defun known-window-class-p ()
  (let ((class (window-class (current-window))))
    (if (cl-ppcre:scan *known-window-class-regex* class) t nil)))

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

(defun entry-autotype (entry)
  (autotype (make-instance 'password :entry entry)))

(defun entry-display-menu (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
    (display-menu pass-obj)))

;; FIXME: keymap in menu doesn't work for me, so these are here
(defvar *entry-display* nil)
(defvar *entry-edit* nil)
(defvar *entry-autotype* nil)

(defun entries-menu (menu-list)
  (let ((sel (select-from-menu
              (current-screen)
              (format-menu menu-list)
              nil
              0
              ;; FIXME: can't pass keymap directly here
              (let ((m (make-sparse-keymap)))
                (define-key m (kbd "M-RET") (lambda (x)
                                              (setf *entry-autotype* t)
                                              (menu-finish x)))
                (define-key m (kbd "M-o") (lambda (x)
                                            (setf *entry-display* t)
                                            (menu-finish x)))
                (define-key m (kbd "M-e") (lambda (x)
                                            (setf *entry-edit* t)
                                            (menu-finish x)))
                m)
              )))
    (when sel
      (cond (*entry-autotype* (progn
                                (setf *entry-autotype* nil)
                                (entry-autotype (car sel))))
            (*entry-display* (progn
                               (setf *entry-display* nil)
                               (entry-display (car sel))))
            (*entry-edit* (progn
                            (setf *entry-edit* nil)
                            (entry-edit (car sel))))
            (t (entry-display-menu (car sel))))
      )))

(defun password-store-insert (entry)
  (run-shell-command (format nil "pass generate -f ~A" entry) t))

(defun entry-new-with-url ()
  (let ((entry-url (domain (window-title (current-window)))  ))
    (let ((entry-name (read-one-line (current-screen) (format nil "New entry for ~A/" entry-url))))
      (when entry-name
        (password-store-insert (concat entry-url "/" entry-name))
        (run-commands "pass-otp")))))

(defun entry-edit (entry)
  (run-shell-command (format nil "pass edit ~A" entry)))

(defcommand pass-otp () ()
  "Show entries for current window"
  (cond ((find-match) (entries-menu (find-match)))
        ((known-window-class-p) (entry-new-with-url))
        (t (message "No match!"))))

(defcommand pass-otp-show-all () ()
  "Show all entries"
  (entries-menu (pass-entries)))

(define-key *root-map* (kbd "s-p") "pass-otp")
(define-key *root-map* (kbd "C-s-p") "pass-otp-show-all")
