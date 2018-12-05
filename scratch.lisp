(push "/home/vod/.quicklisp/local-projects/stumpwm-pass-otp/" asdf:*central-registry*)
(ql:quickload :stumpwm-pass-otp)

(push "/home/vod/.quicklisp/local-projects/next/" asdf:*central-registry*)
(ql:quickload :next)

(stumpwm::menu-selected stumpwm-pass-otp::*st*)

(nth 1 (stumpwm::menu-table stumpwm-pass-otp::*st*))

(describe *stumpwm-pass-otp-map*)

;; (single-menu-current-input menu))
;; menu-selected

(stumpwm-pass-otp:*stumpwm-pass-otp-map*)

;; This captures output:
(run-shell-command "ls /" t)

(ql:quickload :cl-yaml)
(stumpwm:window-class (stumpwm:current-window))

(ql:quickload :cl-string-match)
(ql:quickload :listopia)

(let ((entries (stumpwm-pass-otp::pass-entries)))
  (sequence:find-if (lambda (x) (sm:string-contains-brute "github" x)) entries))

(let ((entries (stumpwm-pass-otp::pass-entries)))
  (listopia:.filter (lambda (x) (sm:string-contains-brute "github" x)) entries))

(listopia:.filter (lambda (x) (sm:string-contains-brute "github" x)) (stumpwm-pass-otp::pass-entries))

(describe (car (stumpwm-pass-otp::pass-entries)))

(stumpwm-pass-otp::find-match)
(describe (window-title (stumpwm:current-window)))
(ql:quickload :quri)

(quri:uri-domain (quri:uri "https://www.google.com"))

(load-module "stumpwm-pass-otp")
