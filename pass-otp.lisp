(in-package #:pass-otp)
(access:enable-dot-syntax)


(define-condition url-not-found-in-window-title (simple-error)
  ()
  (:report (lambda (c str)
             (format str "Known window class, but URL could not be parsed!"))))

(defvar *password-store-locked-p* t)

(defvar *password-store-dir* nil)
(when (null *password-store-dir*) (setf *password-store-dir* (merge-pathnames #p".password-store/" (user-homedir-pathname))))

(defvar *known-window-class-regex* nil)
(when (null *known-window-class-regex*) (setf *known-window-class-regex* "Firefox|Chromium"))

(defvar *uri-regex* nil)
(when (null *uri-regex*) (setf *uri-regex* "(?:(?:https?|ftp|file):\/\/|www\.|ftp\.)(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[-A-Z0-9+&@#\/%=~_|$?!:,.])*(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[A-Z0-9+&@#\/%=~_|$])"))

(defvar *uri-regex-scanner*
  (cl-ppcre:create-scanner *uri-regex* :multi-line-mode t :case-insensitive-mode t))

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
    ;; TODO: url-not-found-in-window-title
    (quri:uri-domain (quri:uri uri))
    ))

(defun format-menu (items)
  (mapcar (lambda (i) (list i i)) items))

(defun find-match ()
  (let ((title (window-title (current-window))))
    (if (and (known-window-class-p) (matches (domain title)))
        (matches (domain title))
        nil)))

(defun password-store-insert (entry)
  (run-shell-command (format nil "pass generate -f ~A" entry) t))

(defun entry-create-with-url ()
  (let ((entry-url (domain (window-title (current-window)))  ))
    (let ((entry-name (read-one-line (current-screen) (format nil "New entry for ~A/" entry-url))))
      (when entry-name
        (password-store-insert (concat entry-url "/" entry-name))
        (run-commands "pass-otp")))))

(defun entry-create ()
  (cond ((known-window-class-p) (entry-create-with-url))
        (t
         (let ((entry-name (read-one-line (current-screen) (format nil "New entry for: "))))
           (when entry-name
             (password-store-insert entry-name)
             (run-commands "pass-otp-show-all"))))))

(defun entry-display (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
    (display pass-obj)))

(defun entry-open-url (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
   (run-shell-command (format nil "xdg-open ~A" (url pass-obj)))))

(defun entry-edit (entry)
  (run-shell-command (format nil "pass edit ~A" entry)))

(defun entry-autotype (entry)
  (autotype (make-instance 'password :entry entry)))

(defun entry-qr-code (entry)
  (qr-code (make-instance 'password :entry entry)))

(defun entry-display-menu (entry)
  (let ((pass-obj (make-instance 'password :entry entry)))
    (display-menu pass-obj)))

(defun entry-menu-action (action-type)
  (lambda (menu)
    (throw :menu-quit
      (values action-type
              (nth #Dmenu.selected #Dmenu.table)))))

(defvar *pass-otp-menu-map* nil)
(when (null *pass-otp-menu-map*)
  (setf *pass-otp-menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "M-RET") (entry-menu-action :entry-autotype))
          (define-key m (kbd "M-o") (entry-menu-action :entry-display))
          (define-key m (kbd "M-e") (entry-menu-action :entry-edit))
          (define-key m (kbd "M-n") (entry-menu-action :entry-create))
          (define-key m (kbd "C-RET") (entry-menu-action :entry-open-url))
          (define-key m (kbd "M-q") (entry-menu-action :entry-qr-code))
          (define-key m (kbd "RET") (entry-menu-action :entry-menu))
          m)))

(defun entries-menu (menu-list)
  (multiple-value-bind (action choice)
      (select-from-menu
       (current-screen)
       (format-menu menu-list)
       nil
       0
       *pass-otp-menu-map*)
    (case action
      (:entry-autotype
       (entry-autotype (car choice)))
      (:entry-display
       (entry-display (car choice)))
      (:entry-edit
       (entry-edit (car choice)))
      (:entry-create
       (entry-create))
      (:entry-open-url
       (entry-open-url (car choice)))
      (:entry-qr-code
       (entry-qr-code (car choice)))
      (:entry-menu
       (entry-display-menu (car choice))))))

;; TODO: force pin entry dialog, if keys are locked
;; That doesn't work though, it hangs without asking for pin
;; (defun handle-pin-entry ()
;;     (run-shell-command (format nil "pass show ~A" (car (pass-entries))))
;;     (setf *password-store-locked-p* nil))

;; (*password-store-locked-p* (handle-pin-entry))

(defcommand pass-otp () ()
  "Show entries for current window"
  (cond ((find-match) (entries-menu (find-match)))
        ((known-window-class-p) (entry-create-with-url))
        (t (entries-menu (pass-entries)))))

(defcommand pass-otp-show-all () ()
  "Show all entries"
  (entries-menu (pass-entries)))
