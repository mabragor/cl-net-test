;;;; cl-net-test.lisp

(in-package #:cl-net-test)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

(define-yaml-config read-net-test-config ("~/.cl-net-test.d/config.yml" *net-test-config*)
  (*admin-email* "root@example.com")
  (*email-host* "mail.example.com")
  (*net-name* "MY")
  (*tests* ""))

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

(defun ping-test (name ips-field)
  "Succeeds, if all IPs listed in IPS-FIELD of config are pinging, NET-TEST-FAILs otherwise"
  (iter (for ip in (gethash ips-field *net-test-config*))
	(destructuring-bind (host ip) (if (consp ip)
					  ip
					  (list ip ip))
	  (if (not (pings-p ip))
	      (collect host into failed-hosts))
	  (finally (if failed-hosts
		       (error 'net-test-fail
			      :name name
			      :details failed-hosts)
		       (return t))))))

(defun net-test-mail (fail)
  (apply #'cl-smtp:send-email `(,*email-host* ,*admin-email* ,*admin-email*
					      ,#?"[$(*net-name*)-NET-FAIL] $((net-test-name fail))"
					      ,(format nil "~a" (net-test-details fail))
					      ,@(let ((login (gethash "login" *net-test-config*)))
						     (if login
							 `(:authentication (,login
									    ,(gethash "pwd" *net-test-config*)))))
					      ,@(let ((ssl (gethash "ssl" *net-test-config*)))
						     (cond ((string= ssl "tls") `(:ssl :tls))
							   ((eq ssl nil) nil)
							   (t `(:ssl t)))))))

(defun make-mailer-on-new-fail (sym-func args)
  (let (prev-fail)
    (lambda ()
      (handler-case (apply (symbol-function sym-func) args)
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


(defrule integer ()
  (parse-integer (concatenate 'string
			      (postimes (character-ranges (#\0 #\9))))))

(defrule cron-interval (&optional from to step-from step-to)
  (let ((value (|| (progn "*" :every)
		   integer)))
    (if (and (integerp value)
	     (or (and from (> from value))
		 (and to (< to value)))
	     (fail-parse (literal-string "Interval value out of bounds"))))
    (let ((step (? (progn "/" integer))))
      (if (and step
	       (or (and step-from (> step-from step))
		   (and step-to (< step-to step)))
	       (fail-parse (literal-string "Interval step out of bounds"))))
      (list value step))))

(defrule cron-minutes ()
  (descend-with-rule 'cron-interval 0 60 1 60))
(defrule cron-hours ()
  (descend-with-rule 'cron-interval 0 24 1 24))
(defrule cron-day-of-month ()
  (descend-with-rule 'cron-interval 1 31 1 31))
(defrule cron-day-of-week ()
  (descend-with-rule 'cron-interval 0 6 1 6))
(defrule cron-months ()
  (descend-with-rule 'cron-interval 1 12 1 12))

(defrule whitespace? ()
  (times (|| #\space #\tab)))

(defrule cron-entry ()
  whitespace? c!-1-cron-minutes whitespace? c!-2-cron-hours
  whitespace? c!-3-cron-day-of-month whitespace? c!-4-cron-day-of-week
  whitespace? c!-5-cron-months whitespace?
  (let* ((pre-rest (times character))
	 (rest (split (literal-string "\\s+") (concatenate 'string pre-rest))))
    (list (intern (string-upcase (car rest)) (find-package (literal-string "CL-NET-TEST")))
	  (cdr rest)
	  (list c!-1 c!-2 c!-3 c!-4 c!-5))))
	       
(defun parse-test-spec (string)
  (iter (for spec in (split "\\n" string))
	(collect (parse 'cron-entry spec))))

(defun expand-cron-params (mins hours doms dows months)
  (macrolet ((frob (var key step-key)
	       ``(,@(if (not (eq :every (car ,var)))
			`(,',key ,(car ,var)))
		    ,@(if (cadr ,var)
			  `(,',step-key ,(cadr ,var))))))
    (append (frob mins :minute :step-min)
	    (frob hours :hour :step-hour)
	    (frob doms :day-of-month :step-dom)
	    (frob dows :day-of-week :step-dow)
	    (frob months :month :step-month))))
      
(let (parsed-tests)
  (defun clear-net-test-jobs ()
    (iter (for (func-sym . rest) in parsed-tests)
	  (cl-cron:delete-cron-job (intern (string func-sym) "KEYWORD"))))
  (defun make-net-test-jobs ()
    (setf parsed-tests (parse-test-spec *tests*))
    (iter (for (func-sym params cron-params) in parsed-tests)
	  (apply #'cl-cron:make-cron-job
		 `(,(make-mailer-on-new-fail func-sym params) ,@(apply #'expand-cron-params cron-params)
		    :hash-key ,(intern (string func-sym) "KEYWORD"))))))


(defun start-net-test ()
  (read-net-test-config)
  (make-net-test-jobs)
  (cl-cron:start-cron))

(defun stop-net-test ()
  (cl-cron:stop-cron)
  (clear-net-test-jobs))

