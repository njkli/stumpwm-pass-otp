(defpackage #:stumpwm-pass-otp
  (:use #:cl #:listopia #:cl-string-match #:stumpwm)
  (:export *password-store-dir* *stumpwm-pass-otp-map* show-uri))

(in-package #:stumpwm-pass-otp)

(defvar *password-store-dir* (merge-pathnames #p".password-store/" (user-homedir-pathname)))

(defvar *stumpwm-pass-otp-map* nil )
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
  (listopia:.filter (lambda (x) (sm:string-contains-brute entry x)) (pass-entries)))

(defun known-window-class ()
  (let ((class (stumpwm:window-class (stumpwm:current-window))))
    (if (ppcre:scan "Firefox|Chromium" class) t nil)))

(defun domain (window-title-str)
  (quri:uri-domain (quri:uri (sequence:subseq (cadr (ppcre:split " - " window-title-str)) 0))))

(defun format-menu (items)
  (mapcar (lambda (i) (list i i)) items))

(defun find-match ()
  (let ((title (stumpwm:window-title (stumpwm:current-window))))
    (if (known-window-class)
         (matches (domain title))
         (pass-entries))))

;; (defun otp (entry)
;;   (stumpwm:run-shell-command (format nil "pass otp -c ~A" entry)))

(defun entry-show (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
    (display pass-obj)))

;; (defun entry-show (entry)
;;   (let ((pass-str (stumpwm:run-shell-command (format nil "pass show ~A" entry) t)))
;;     (stumpwm:message pass-str)))

;; (defun entry-show (menu_obj)
;;   (let ((entry (car (nth (stumpwm::menu-selected menu_obj) (stumpwm::menu-table menu_obj)))))
;;     (print entry)
;;     (stumpwm:message entry)))

(defvar *entry-show* nil)
(defvar *entry-autofill* nil)

(stumpwm:defcommand show-uri () ()
  "show win uri"
  (if (find-match)
      (setf msg (format nil "~{~A~^, ~}" (find-match)))
      (setf msg "Not found!"))
  ;; (stumpwm:message msg)
  (let ((sel (stumpwm:select-from-menu
              (stumpwm:current-screen)
              (format-menu (find-match))
              nil
              0
              (let ((m (make-sparse-keymap)))
                (define-key m (kbd "M-RET") (lambda (x)
                                              (setf *entry-autofill* t)
                                              (stumpwm::menu-finish x)))
                (define-key m (kbd "M-o") (lambda (x)
                                            (setf *entry-show* t)
                                            (stumpwm::menu-finish x)))
                m)
              )))
    (when sel
      (when *entry-autofill*
        (setf *entry-autofill* nil)
        (stumpwm:message "Autofill requested! ~A" (car sel)) )
      (when *entry-show*
        (setf *entry-show* nil)
        (entry-show (car sel)) )
      ;; (entry-action (car sel))

      ;; (show-entry (car sel))
      ;; (otp (car sel))
      ;; Action stuff
      ;; (stumpwm:message (format nil "~A : ~A" sel (stumpwm:get-x-selection 5 :clipboard)))
      ;; (stumpwm:message (stumpwm:run-shell-command (format nil "pass show ~a" sel) t))
      )))

#||
(stumpwm:defcommand pass-copy () ()
  "Put a password into the clipboard."
  (let ((entry (stumpwm:completing-read (stumpwm:current-screen)
                                        "entry: "
                                        (pass-entries))))
    (stumpwm:run-shell-command (format nil "pass -c ~a" entry))))

(stumpwm:defcommand pass-generate () ()
  "Generate a password and put it into the clipboard"
  (let ((entry-name (stumpwm:read-one-line (stumpwm:current-screen)
                                           "entry name: ")))
(stumpwm:run-shell-command (format nil "pass generate -c ~a" entry-name))))
||#
