;;;; cl-net-test.lisp

(in-package #:cl-net-test)

(cl-interpol:enable-interpol-syntax)

(defvar *gate-ips* nil "List of IPs to ping")
(defvar *admin-email* "root@example.com")
(defvar *email-host* "mail.example.com")
(defvar *net-name* "MY")
(defvar *tests* '((front-ping-test :step-min 1)))

(defun ip-addr-p (ip)
  (or (register-groups-bind (first second third fourth)
	  ("^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d{1,3})$" ip)
	(and (< (parse-integer first) 256)
	     (< (parse-integer second) 256)
	     (< (parse-integer third) 256)
	     (< (parse-integer fourth) 256)))
      nil))

(defun pings-p (ip)
  (assert (ip-addr-p ip))
  (multiple-value-bind (out err err-code)
      (script #?"ping -c 3 $(ip)")
    (declare (ignore out err))
    (equal 0 err-code)))

(define-condition net-test-fail ()
  ((name :initform (error "Name must be provided!") :initarg :name :reader net-test-name)
   (details :initform nil :initarg :details :reader net-test-details)))

(defun front-ping-test ()
  "Succeeds, if all IPs listed in *GATE-IPS* are pinging, NET-TEST-FAILs otherwise"
  (iter (for ip in *gate-ips*)
	(if (not (pings-p ip))
	    (collect ip into failed-ips))
	(finally (if failed-ips
		     (error 'net-test-fail
			    :name "FRONT-PING-TEST"
			    :details failed-ips)
		     (return t)))))

(defun net-test-mail (fail)
  (cl-smtp:send-email *email-host* *admin-email* *admin-email*
		      #?"[$(*net-name*)-NET-FAIL] $((net-test-name fail))"
		      (format nil "~a" (net-test-details fail))))

(defun make-mailer-on-new-fail (sym-func)
  (let (prev-fail)
    (lambda ()
      (handler-case (funcall (symbol-function sym-func))
	(net-test-fail (new-fail)
	  (when (or (not prev-fail)
		    (not (equal (net-test-name prev-fail)
				(net-test-name new-fail)))
		    (not (equal (net-test-details prev-fail)
				(net-test-details new-fail))))
	    (setf prev-fail new-fail)
	    (net-test-mail new-fail)
	    nil))
	(:no-error (val)
	  (when prev-fail
	    (setf prev-fail nil)
	    (net-test-mail (make-instance 'net-test-fail
					  :name (string sym-func)
					  :details "Everything is OK now")))
	  val)))))

(defun clear-net-test-jobs ()
  (iter (for (func-sym . rest) in *tests*)
	(cl-cron:delete-cron-job (intern (string func-sym) "KEYWORD"))))

(defun make-net-test-jobs ()
  (iter (for (func-sym . rest) in *tests*)
	(apply #'cl-cron:make-cron-job
	       `(,(make-mailer-on-new-fail func-sym) ,@rest
		  :hash-key ,(intern (string func-sym) "KEYWORD")))))


(defun start-net-test ()
  (make-net-test-jobs)
  (cl-cron:start-cron))

(defun stop-net-test ()
  (cl-cron:stop-cron)
  (clear-net-test-jobs))

