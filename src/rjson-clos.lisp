(in-package :rjson)
;;; This File Includes Utilities for sending CLOS instances across the net as
(defmethod represent-rjson ((object structure-object))
  "Represents an object's slots and such in an RJSON-compatible way."
  (error "Cannot represent structures with RJSON."))

;;; RJSON CLOS protocol
(defgeneric represent-rjson-slot (object slot)
  (:documentation "Should return two parenscript forms, one for
the key to the slot and the other for the value of the slot.
Returns nil if the slot should not be included."))

(defgeneric represented-slots (object)
  (:documentation "Returns a list of slots that should be represented by RJSON.  Default 
is all slots of the class."))

(defgeneric rjson-type-of-class (class)
  (:documentation "Like RJSON-TYPE but for use on the class of the object being serialized,
not the object itself."))

(defmethod represent-rjson-slot ((object standard-object) slot)
  (let ((value
	 (if (slot-boundp-using-class (class-of object) object slot)
	     (slot-value-using-class (class-of object) object slot)
	     'undefined))
	(key (or (first (slot-definition-initargs slot))
		 (slot-definition-name slot))))
    (if (or ;(null value)
	    (eql 'undefined value))
	nil
	(values (parenscript:symbol-to-js-string key)
		(represent value)))))

(defmethod represented-slots ((object standard-object))
  "Default is all the slots of the object."
  (class-slots (class-of object)))

(defmethod rjson-type ((object standard-object))
  (rjson-type-of-class (class-of object)))

(defmethod rjson-type-of-class ((class standard-class))
  (let ((type-symbol (class-name class)))
    (string-downcase (format nil
			     "~A:~A"
			     (package-name (symbol-package type-symbol))
			     (symbol-name type-symbol)))))

(defmethod represent-rjson ((object standard-object))
  "Represents an object's slots and such in an RJSON-compatible way."
  `(create ,@(mapcan #'(lambda (slot)
			 (multiple-value-bind (key value)
			     (represent-rjson-slot object slot)
			   (if (null key)
			       nil
			       (list key value))))
		     (represented-slots object))))