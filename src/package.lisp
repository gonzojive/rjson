(defpackage :org.iodb.rjson
  (:nicknames :rjson)
  (:use :common-lisp :closer-mop :parenscript)
  (:export #:encode-rjson
	   #:encode-rjson-to-string
	   #:represent
	   #:represent-rjson
	   #:rjson-type
	   
	   ;; parenscript side
	   #:decode-rjson
	   #:rjtype))
