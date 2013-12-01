;; gzochi/private/data.scm: Private infrastructure for gzochi data API
;; Copyright (C) 2013 Julian Graham
;;
;; gzochi is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs

(library (gzochi private data)
  (export gzochi:make-managed-record-type-descriptor
	  gzochi:make-managed-record-constructor-descriptor
	  gzochi:managed-record-accessor
	  gzochi:managed-record-mutator
	  gzochi:managed-record-constructor
	  gzochi:managed-record-predicate

          gzochi:define-managed-record-type
	  gzochi:managed-record-type-descriptor
	  gzochi:managed-record-constructor-descriptor

	  gzochi:make-managed-record

	  gzochi:managed-record?
	  gzochi:managed-record-rtd 
	  gzochi:managed-record-type-name
	  gzochi:managed-record-type-parent 
	  gzochi:managed-record-type-uid

	  gzochi:create-reference
	  gzochi:dereference

	  gzochi:get-binding
	  gzochi:set-binding!
	  gzochi:remove-binding!
	  gzochi:remove-object!

	  gzochi:make-managed-reference
	  gzochi:managed-reference?
	  gzochi:managed-reference-oid

	  gzochi:managed-reference-serialization
	  gzochi:serialize-managed-reference
	  gzochi:deserialize-managed-reference

	  gzochi:serialize-managed-record
	  gzochi:deserialize-managed-record

	  gzochi:mark-for-write!
	  gzochi:mark-for-read!)

  (import (gzochi io)
	  (only (guile) and=> gensym macroexpand) 
	  (rnrs)
	  (only (srfi :1) last split-at take)
	  (srfi :8))

  (define (gzochi:serialize-managed-reference port reference)
    (or (not reference) 
	(gzochi:managed-reference? reference)
	(raise (condition (make-assertion-violation)
			  (make-message-condition "Not a managed reference.")
			  (make-irritants-condition reference))))

    (gzochi:write-boolean port reference)
    (if reference (gzochi:write-integer 
		   port (gzochi:managed-reference-oid reference))))

  (define (gzochi:deserialize-managed-reference port)
    (and (gzochi:read-boolean port)
	 (gzochi:make-managed-reference (gzochi:read-integer port) #f)))

  (define gzochi:managed-reference-serialization 
    (gzochi:make-serialization gzochi:serialize-managed-reference 
			       gzochi:deserialize-managed-reference))

  (define (gzochi:managed-record-accessor rtd k)
    (define accessor (record-accessor rtd k))
    (lambda (record)
      (let ((val (accessor record)))
	(if (gzochi:managed-reference? val)
	    (gzochi:dereference val)
	    val))))

  (define gzochi:managed-record-reference-accessor record-accessor)

  (define (gzochi:managed-record-mutator rtd k)
    (define accessor (record-accessor rtd k))
    (define mutator (record-mutator rtd k))
    (lambda (record val)
      (gzochi:mark-for-write! record)
      (mutator record (if (gzochi:managed-record? val) 
			  (gzochi:create-reference val)
			  val))
      val))

  (define gzochi:managed-record-reference-mutator record-mutator)
  (define gzochi:managed-record-constructor record-constructor)
  (define gzochi:managed-record-predicate record-predicate)

  (define gzochi:managed-record-rtd record-rtd)
  (define gzochi:managed-record-type-name record-type-name)
  (define gzochi:managed-record-type-parent record-type-parent)
  (define gzochi:managed-record-type-uid record-type-uid)

  (define (managed-record-subtype? rtd)
    (or (eq? rtd gzochi:managed-record)
	(and=> (record-type-parent rtd) managed-record-subtype?)))

  (define default-serialization
    (gzochi:make-serialization
     gzochi:serialize-managed-reference 
     gzochi:deserialize-managed-reference))
  
  (define (gzochi:make-managed-record-type-descriptor 
	   name parent uid sealed? opaque? fields)
    
    (or uid 
	(raise 
	 (condition
	  (make-assertion-violation)
	  (make-message-condition "Managed records must be nongenerative"))))

    (let* ((fields (vector->list fields))
	   (field-names (map (lambda (f) (take f 2)) fields))
	   (field-serializations (map last fields))
	   (managed-parent
	    (cond ((not parent) gzochi:managed-record)
		  ((managed-record-subtype? parent) parent)
		  (else (raise (condition 
				(make-assertion-violation)
				(make-message-condition
				 "Parent types must be managed"))))))
	   (rtd (make-record-type-descriptor
		 name managed-parent uid sealed? opaque? 
		 (list->vector field-names))))
      
      (gzochi:register-record-type! uid rtd)
      (gzochi:register-serialization! 
       uid (list->vector field-serializations))

      rtd))

  (define (gzochi:make-managed-record-constructor-descriptor 
	   rtd parent-constructor-descriptor protocol)
    (define prtd (record-type-parent rtd))
    (define (maybe-create-reference arg)
      (if (gzochi:managed-record? arg) (gzochi:create-reference arg) arg))
    (define (default-inherited-protocol n)
      (lambda args
	(receive 
	 (n-args p-args) 
	 (split-at args (- (length args) 
			   (vector-length (record-type-field-names rtd))))
	 (let ((field-binder (apply n n-args)))
	   (apply field-binder p-args)))))
    (define (managed-type? rtd)
      (and rtd (or (eq? rtd gzochi:managed-record) 
		   (managed-type? (record-type-parent rtd)))))
    (define (wrap-field-binder field-binder)
      (lambda args (apply field-binder (map maybe-create-reference args))))

    (or (managed-type? rtd) (raise (make-assertion-violation)))

    (if (eq? prtd gzochi:managed-record)
	(make-record-constructor-descriptor 
	 rtd
	 (record-constructor-descriptor gzochi:managed-record)
	 (if protocol
	     (lambda (mr-constructor)
	       (protocol (lambda args
			   (apply (mr-constructor) 
				  (map maybe-create-reference args)))))
	     (lambda (mr-constructor) 
	       (lambda args (apply (mr-constructor) 
				   (map maybe-create-reference args))))))

	(make-record-constructor-descriptor 
	 rtd
	 (or parent-constructor-descriptor
	     (gzochi:make-managed-record-constructor-descriptor prtd #f #f))
	 (lambda (parent-constructor)
	   ((or protocol default-inherited-protocol)
	    (lambda n-args
	      (let ((field-binder (apply parent-constructor n-args)))
		(lambda p-args
		  (apply field-binder
			 (map maybe-create-reference p-args))))))))))

  (define managed-record-type-registry (make-eq-hashtable))

  (define (register-managed-record-type name rtd rcd)
    (hashtable-set! managed-record-type-registry name (cons rtd rcd)))
  (define (lookup-managed-record-type-descriptor name)
    (and=> (hashtable-ref managed-record-type-registry name #f) car))
  (define (lookup-managed-record-constructor-descriptor name)
    (and=> (hashtable-ref managed-record-type-registry name #f) cdr))

  (define (guess-constructor-name record-name)
    (string->symbol (string-append "make-" (symbol->string record-name))))
  (define (guess-predicate-name record-name)
    (string->symbol (string-append (symbol->string record-name) "?")))

  (define (sequence n)
    (define (seq-inner n) (if (= n 0) '(0) (cons n (seq-inner (- n 1)))))
    (reverse (seq-inner n)))

  (define (process-field-descriptors fields)
    (let loop ((processed-fields '())
	       (fields fields))
      (syntax-case fields (immutable mutable serialization)
	[() #`(list #,@processed-fields)]
        [((immutable name) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote immutable) (quote name) #f)) #'rest)]
	[((immutable name (serialization s)) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote immutable) (quote name) s)) #'rest)]
	[((immutable name accessor) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote immutable) (quote name) #f)) #'rest)]
	[((immutable name accessor (serialization s)) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote immutable) (quote name) s)) #'rest)]
	[((mutable name) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote mutable) (quote name) #f)) #'rest)]
        [((mutable name (serialization s)) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote mutable) (quote name) s)) #'rest)]
        [((mutable name accessor mutator) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote mutable) (quote name) #f)) #'rest)]
        [((mutable name accessor mutator (serialization s)) . rest)
	 (loop #`(#,@processed-fields 
		  (list (quote mutable) (quote name) s)) #'rest)]
        [(name . rest) (identifier? #'name)
	 (loop #`(#,@processed-fields 
		  (list (quote mutable) (quote name) #f)) #'rest)])))

  (define (process-fields record-name fields)
    (define (wrap x) (datum->syntax record-name x))
    (define (id->string x) (symbol->string (syntax->datum x)))
    (define record-name-str (id->string record-name))
    (define (guess-accessor-name field-name)
      (wrap (string->symbol (string-append
			     record-name-str "-" (id->string field-name)))))
    (define (guess-mutator-name field-name)
      (wrap (string->symbol
	     (string-append
	      record-name-str "-" (id->string field-name) "-set!"))))

    (define (f x)
      (syntax-case x (immutable mutable serialization)
        [(immutable name)
         (list (wrap `(immutable ,(syntax->datum #'name) #f))
               (guess-accessor-name #'name)
               #f)]
        [(immutable name (serialization s))
         (list (wrap `(immutable ,(syntax->datum #'name) #f))
	       (guess-accessor-name #'name)
	       #f)]
        [(immutable name accessor)
         (list (wrap `(immutable ,(syntax->datum #'name) #f)) #'accessor #f)]
	[(immutable name accessor (serialization s))
	 (list (wrap `(immutable ,(syntax->datum #'name) #f)) #'accessor #f)]
        [(mutable name)
         (list (wrap `(mutable ,(syntax->datum #'name) #f))
               (guess-accessor-name #'name)
               (guess-mutator-name #'name))]
        [(mutable name (serialization s))
         (list (wrap `(mutable ,(syntax->datum #'name) #f))
               (guess-accessor-name #'name)
               (guess-mutator-name #'name))]
        [(mutable name accessor mutator)
         (list (wrap `(mutable ,(syntax->datum #'name) #f)) 
	       #'accessor 
	       #'mutator)]
        [(mutable name accessor mutator (serialization s))
         (list (wrap `(mutable ,(syntax->datum #'name) #f))
	       #'accessor 
	       #'mutator)]
        [name (identifier? #'name)
	      (list (wrap `(immutable ,(syntax->datum #'name) #f))
		    (guess-accessor-name #'name)
		    #f)]
        [else
         (syntax-violation 
	  'gzochi:define-managed-record-type "Invalid field specifier" x)]))
    (map f fields))

  (define-syntax gzochi:define-managed-record-type
    (lambda (stx)
      (syntax-case stx ()
	((_ (record-name constructor-name predicate-name) record-clause ...)
	 #'(define-managed-record-type0 
	     (record-name constructor-name predicate-name)
	     record-clause ...))
	((_ record-name record-clause ...)
	 (let* ((record-name-sym (syntax->datum #'record-name))
		(constructor-name 
		 (datum->syntax 
		  #'record-name (guess-constructor-name record-name-sym)))
		(predicate-name 
		 (datum->syntax 
		  #'record-name (guess-predicate-name record-name-sym))))
	   #`(define-managed-record-type0 
	       (record-name #,constructor-name #,predicate-name) 
	       record-clause ...))))))

  (define-syntax define-managed-record-type0
    (lambda (stx)	  
      (define *unspecified* (cons #f #f))
      (define (unspecified? obj)
        (eq? *unspecified* obj))
      (syntax-case stx ()
        ((_ (record-name constructor-name predicate-name) record-clause ...)
         (let loop ((_fields *unspecified*)
                    (_parent *unspecified*)
                    (_protocol *unspecified*)
                    (_sealed *unspecified*)
                    (_opaque *unspecified*)
                    (_nongenerative *unspecified*)
                    (_constructor *unspecified*)
                    (_parent-rtd *unspecified*)
                    (record-clauses #'(record-clause ...)))

           (syntax-case record-clauses
               (fields parent protocol sealed opaque nongenerative
                       constructor parent-rtd serialization)
             [()
              (let* ((fields (if (unspecified? _fields) '() _fields))
                     (processed-fields (process-fields #'record-name fields))
		     (field-names (process-field-descriptors fields))
                     (field-accessors
                      (fold-left 
		       (lambda (lst x c)
			 (cons #`(define #,(cadr x)
				   (gzochi:managed-record-accessor 
				    record-name #,c))
			       lst))
		       '() processed-fields 
		       (sequence (length processed-fields))))
                     (field-mutators
                      (fold-left
		       (lambda (lst x c)
			 (if (caddr x)
			     (cons #`(define #,(caddr x)
				       (gzochi:managed-record-mutator 
					record-name #,c))
				   lst)
			     lst))
		       '() processed-fields 
		       (sequence (length processed-fields))))
                     (parent-cd
		      (cond ((not (unspecified? _parent))
			     #`(gzochi:managed-record-constructor-descriptor
				#,_parent))
			    ((not (unspecified? _parent-rtd))
			     (cadr _parent-rtd))
			    (else #f)))
                     (parent-rtd
		      (cond ((not (unspecified? _parent))
			     #`(gzochi:managed-record-type-descriptor 
				#,_parent))
			    ((not (unspecified? _parent-rtd))
			     (car _parent-rtd))
			    (else #f)))
                     (protocol (if (unspecified? _protocol) #f _protocol))
                     (uid (if (unspecified? _nongenerative) 
			      #''record-name 
			      _nongenerative))
                     (sealed? (if (unspecified? _sealed) #f _sealed))
                     (opaque? (if (unspecified? _opaque) #f _opaque)))

		#`(begin
		    (define record-name
                      (gzochi:make-managed-record-type-descriptor
                       (quote record-name)
                       #,parent-rtd #,uid #,sealed? #,opaque? 
		       (list->vector #,field-names)))
		    
		    (let ()
		      (register-managed-record-type 
		       (quote record-name)
		       record-name 
		       (gzochi:make-managed-record-constructor-descriptor 
			record-name #,parent-cd #,protocol)))

                    (define constructor-name
                      (record-constructor
                       (gzochi:make-managed-record-constructor-descriptor
                        record-name #,parent-cd #,protocol)))
                    (define predicate-name (record-predicate record-name))
                    #,@field-accessors
                    #,@field-mutators))]
             [((fields record-fields ...) . rest)
              (if (unspecified? _fields)
                  (loop #'(record-fields ...) _parent _protocol _sealed _opaque
			_nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((parent parent-name) . rest)
              (if (not (unspecified? _parent-rtd))
                  (raise (make-assertion-violation))
                  (if (unspecified? _parent)
                      (loop _fields #'parent-name _protocol _sealed _opaque
                            _nongenerative _constructor _parent-rtd #'rest)
                      (raise (make-assertion-violation))))]
             [((protocol expression) . rest)
              (if (unspecified? _protocol)
                  (loop _fields _parent #'expression _sealed _opaque
                        _nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((sealed sealed?) . rest)
              (if (unspecified? _sealed)
                  (loop _fields _parent _protocol #'sealed? _opaque
                        _nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((opaque opaque?) . rest)
              (if (unspecified? _opaque)
                  (loop _fields _parent _protocol _sealed #'opaque?
                        _nongenerative _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((nongenerative) . rest)
              (if (unspecified? _nongenerative)
                  (loop _fields _parent _protocol _sealed _opaque 
			#''record-name _constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((nongenerative uid) . rest)
              (if (unspecified? _nongenerative)
                  (loop _fields _parent _protocol _sealed _opaque #''uid 
			_constructor _parent-rtd #'rest)
                  (raise (make-assertion-violation)))]
             [((parent-rtd rtd cd) . rest)
              (if (not (unspecified? _parent))
                  (raise (make-assertion-violation))
                  (if (unspecified? _parent-rtd)
                      (loop _fields _parent _protocol _sealed _opaque
                            _nongenerative _constructor #'(rtd cd) #'rest)
                      (raise (make-assertion-violation))))]))))))

  (define-syntax gzochi:managed-record-type-descriptor
    (lambda (stx)
      (syntax-case stx ()
	((_ name) #`(lookup-managed-record-type-descriptor 
		     #,(datum->syntax 
			stx (list 'quote (syntax->datum #'name))))))))

  (define-syntax gzochi:managed-record-constructor-descriptor
    (lambda (stx)
      (syntax-case stx ()
	((_ name) #`(lookup-managed-record-constructor-descriptor 
		     #,(datum->syntax 
			stx (list 'quote (syntax->datum #'name))))))))

  (define (gzochi:serialize-managed-record port record)
    (define (serialize rtd)
      (let ((parent-rtd (record-type-parent rtd)))
	(if (and parent-rtd (not (eq? parent-rtd gzochi:managed-record)))
	    (serialize parent-rtd))
	(let ((serialization 
	       (gzochi:uid->serialization (record-type-uid rtd)))
	      (num-fields (vector-length (record-type-field-names rtd))))
	  (let loop ((i 0))
	    (and (< i num-fields)
		 (let ((accessor (record-accessor rtd i))
		       (serializer (or (and=> (vector-ref serialization i)
					      gzochi:serialization-serializer)
				       gzochi:serialize-managed-reference)))

		   (serializer port (accessor record))
		   (loop (+ i 1))))))))
    
    (let ((rtd (record-rtd record)))
      (gzochi:write-symbol port (record-type-uid rtd))
      (serialize rtd)))

  (define (gzochi:deserialize-managed-record port)
    (define (deserialize rtd)
      (let ((serialization 
	     (gzochi:uid->serialization (record-type-uid rtd)))
	    (num-fields (vector-length (record-type-field-names rtd))))

	(let loop ((i 0) (vals '()))
	  (if (< i num-fields)
	      (let ((deserializer (or (and=> (vector-ref serialization i)
					     gzochi:serialization-deserializer)
				      gzochi:deserialize-managed-reference)))
		(loop (+ i 1) (cons (deserializer port) vals)))
	      (reverse vals)))))
	
    (define (constructor-descriptor rtd)
      (let* ((parent-rtd (record-type-parent rtd))
	     (parent-rctd 
	      (and parent-rtd 
		   (not (eq? parent-rtd gzochi:managed-record))
		   (constructor-descriptor parent-rtd))))
	(make-record-constructor-descriptor 
	 rtd parent-rctd (lambda (n)
			   (lambda ()
			     (let ((p (n)))
			       (apply p (deserialize rtd))))))))
    
    (let* ((uid (gzochi:read-symbol port))
	   (rtd (gzochi:uid->record-type uid)))
      (or rtd (raise (condition 
		      (make-assertion-violation)
		      (make-message-condition 
		       (string-append 
			"Attempting to deserialize unknown type " 
			(symbol->string uid))))))

      (let* ((rctd (constructor-descriptor rtd))
	     (ctor (record-constructor rctd)))
	(ctor))))
  
  (define record-type-registry (make-eq-hashtable))

  (define (gzochi:register-record-type! uid rtd)
    (hashtable-set! record-type-registry uid rtd))
  (define (gzochi:uid->record-type uid)
    (hashtable-ref record-type-registry uid #f))

  (define serializer-registry (make-eq-hashtable))

  (define (gzochi:register-serialization! uid serialization)
    (hashtable-set! serializer-registry uid serialization))
  (define (gzochi:uid->serialization uid) 
    (hashtable-ref serializer-registry uid #f))

  (define (gzochi:mark-for-write! managed-record)
    (or (gzochi:managed-record? managed-record)
	(raise (condition (make-assertion-violation)
			  (make-message-condition 
			   "Only managed records may be marked."))))
    (primitive-mark-for-write! managed-record))
    
  (define (gzochi:mark-for-read! managed-record) (if #f #f))

  (define-record-type 
    (gzochi:managed-record gzochi:make-managed-record gzochi:managed-record?))
    
  (define-record-type (gzochi:managed-reference 
		       gzochi:make-managed-reference 
		       gzochi:managed-reference?)
    (fields oid))

  (define (gzochi:create-reference obj)
    (or (gzochi:managed-record? obj)
	(raise (condition (make-assertion-violation)
			  (make-message-condition 
			   "References can only be created to managed records.")
			  (make-irritants-condition obj))))

    (primitive-create-reference obj))

  (define (gzochi:dereference reference)
    (or (gzochi:managed-reference? reference)
	(raise (condition (make-assertion-violation)
			  (make-message-condition
			   "Only managed references can be dereferenced."))))
    (primitive-dereference reference))

  (define (gzochi:get-binding name)
    (or (string? name) (raise (make-assertion-violation)))
    (primitive-get-binding name))
  
  (define (gzochi:set-binding! name obj)
    (or (string? name) (raise (make-assertion-violation)))
    (or (gzochi:managed-record? obj)
	(raise (condition 
		(make-assertion-violation)
		(make-message-condition
		 "Bindings may only be created for managed records."))))

    (primitive-set-binding! name obj))

  (define (gzochi:remove-binding! name)
    (or (string? name) (raise (make-assertion-violation)))
    (primitive-remove-binding! name))

  (define (gzochi:remove-object! obj)
    (or (gzochi:managed-record? obj)
	(raise (condition (make-message-condition "Not a managed record.") 
			  (make-assertion-violation))))
    (primitive-remove-object! obj))

  (define primitive-mark-for-write! #f)

  (define primitive-create-reference #f)
  (define primitive-dereference #f)

  (define primitive-get-binding #f)
  (define primitive-set-binding! #f)
  (define primitive-remove-binding! #f)
  (define primitive-remove-object! #f)
)
