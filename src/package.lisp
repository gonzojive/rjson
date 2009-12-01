(defpackage :org.iodb.rjson
  (:nicknames :rjson)
  (:use :common-lisp :closer-mop :parenscript)
  (:export #:encode-rjson
	   #:encode-rjson-to-string
	   #:represent
	   #:represent-rjson
	   #:represent-rjson-slot
	   #:represented-slots
	   #:rjson-type
	   #:rjson-type-of-class
	   #:create
	   #:xref-p
	   
	   ;; parenscript side
	   #:decode-rjson
	   #:rjtype))
