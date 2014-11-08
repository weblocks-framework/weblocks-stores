
(defpackage #:weblocks-perec
  (:use :cl :weblocks-stores)
  (:documentation
   "A driver for weblocks backend store API that connects to cl-perec."))

(in-package :weblocks-perec)

(weblocks-stores:register-store-type :perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :perec)) &rest args)
  (setf hu.dwim.perec:*database* (setf *default-store* (apply #'make-instance args))))

(defmethod close-store ((store hu.dwim.perec:database-mixin))
  ; TODO close database 
  (when (eq *default-store* store)
    (setf *default-store* nil)))

(defmethod clean-store ((store hu.dwim.perec:database-mixin))
  (maybe-begin-transaction store)
  (print (loop for i in (hu.dwim.rdbms:list-views)
               if (equal (char i 0) #\_)
               collect
               i))
  (print (loop for i in (hu.dwim.rdbms:list-tables)
               if (equal (char i 0) #\_)
               collect i))
  ;(print (hu.dwim.rdbms:list-views))
  #+l(let ((hu.dwim.rdbms:*database* store))
    (loop for i in (hu.dwim.rdbms:list-views)
          if (equal (char i 0) #\_)
          do
          (hu.dwim.rdbms:drop-view i))
    (loop for i in (hu.dwim.rdbms:list-tables)
          if (equal (char i 0) #\_)
          do
          (hu.dwim.rdbms:drop-table i))))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store hu.dwim.perec:database-mixin))
  (hu.dwim.perec:begin-transaction store (setf hu.dwim.rdbms:*transaction* (hu.dwim.rdbms:make-transaction store))))

(defmethod commit-transaction ((store hu.dwim.perec:database-mixin))
  (hu.dwim.perec:commit-transaction store hu.dwim.perec:*transaction*))

(defmethod rollback-transaction ((store hu.dwim.perec:database-mixin))
  (hu.dwim.perec:rollback-transaction store hu.dwim.perec:*transaction*))

(defun maybe-begin-transaction (store)
  (or 
    (hu.dwim.perec:in-transaction-p)
    #+l(if (boundp 'hu.dwim.rdbms::*transaction*) 
      hu.dwim.rdbms::*transaction*) 
    (begin-transaction store)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store hu.dwim.perec:database-mixin) object &key)
  (maybe-begin-transaction store)
  (hu.dwim.perec::export-to-rdbms (class-of object))

  (hu.dwim.perec:ensure-persistent object)
  (commit-transaction store)
  object)

(defmethod delete-persistent-object ((store hu.dwim.perec:database-mixin) object)
  (maybe-begin-transaction store)
  (hu.dwim.perec::export-to-rdbms (find-class (type-of object)))

  (hu.dwim.perec:purge-instance object)
  (commit-transaction store))

(defmethod delete-persistent-object-by-id ((store hu.dwim.perec:database-mixin) class-name object-id)
  (maybe-begin-transaction store)
  (hu.dwim.perec::export-to-rdbms (find-class class-name))

  (delete-persistent-object store (find-persistent-object-by-id store class-name object-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store hu.dwim.perec:database-mixin) class-name object-id)
  (maybe-begin-transaction store)
  (hu.dwim.perec::export-to-rdbms (find-class class-name))

  (hu.dwim.perec:with-database store 
    (let ((instance (hu.dwim.perec:load-instance object-id)))
      (assert (equal (type-of instance) class-name))
      (update-current-transaction instance))))

; Copied from weblocks-clsql
(defun range-to-offset (range)
  "Converts the 'range' argument to SQL OFFSET."
  (when range
    (car range)))

; Copied from weblocks-clsql
(defun range-to-limit (range)
  "Converts the 'range' argument to SQL LIMIT."
  (when range
    (- (cdr range) (car range))))

(defun update-current-transaction (object)
  (setf (hu.dwim.perec:transaction-of object) hu.dwim.perec:*transaction*)
  (hu.dwim.perec:load-instance object)
  object)

(defmethod find-persistent-objects ((store hu.dwim.perec:database-mixin) class-name 
                                                                         &key (filter nil) order-by range slot
                                                                         (value nil value-given)
                                                                         (test #'equal))
  "The slot and value keys must appear together.  If they appear, a
filter will be applied (before the filter passed, if any) that
requires all objects to have the given value in the given slot."
  (maybe-begin-transaction store)
  (hu.dwim.perec::export-to-rdbms (find-class class-name))

  (when (and filter (not range))
    (return-from find-persistent-objects 
                 (remove-if filter (find-persistent-objects store class-name :order-by order-by))))

  (when (or filter slot value)
    (error "Not implemented with args ~A ~A ~A ~A ~A" filter slot value range order-by))

  (let ((query (hu.dwim.perec:make-query 
                 `(hu.dwim.perec:select (i)
                    (hu.dwim.perec:from (i ,class-name))
                    ,@(when range 
                        `((hu.dwim.perec:limit ,(range-to-limit range))
                          (hu.dwim.perec:offset ,(range-to-offset range))))))))
    (when order-by 
      (hu.dwim.perec:add-order-by query
                                  `(slot-value i ', (car order-by))
                                  (if (equal (cdr order-by) :asc)
                                    :ascending
                                    :descending)))

    (mapcar 
      #'update-current-transaction
      (hu.dwim.perec:execute-query query))))

(defmethod count-persistent-objects ((store hu.dwim.perec:database-mixin) class-name &rest args &key filter)
  (maybe-begin-transaction store)
  (hu.dwim.perec::export-to-rdbms (find-class class-name))

  (when filter 
    (setf filter filter))

  (cond 
    ((and filter (= (length args) 2))
     (length (find-persistent-objects store class-name :filter filter)))
    ((not args)
     (hu.dwim.perec:count-instances class-name))
    (t 
     (error "Not implemented with args ~A" args))))

(defmethod list-model-classes ((store hu.dwim.perec:database-mixin))
  (loop for i in (hu.dwim.perec:collect-all-persistent-class-tables) 
        unless (equal (class-name (hu.dwim.perec::persistent-class-of i)) 'hu.dwim.perec:persistent-set)
        collect (class-name (hu.dwim.perec::persistent-class-of i))))

(defmethod delete-model-class ((store hu.dwim.perec:database-mixin) class-name)
  (maybe-begin-transaction store)

  (hu.dwim.perec:purge-instances class-name)
  (commit-transaction store))

#+l(defmethod weblocks-stores:object-id :around (obj)
  (if (subtypep (type-of obj) 'hu.dwim.perec:persistent-object)
    (hu.dwim.perec:oid-of obj) 
    (call-next-method)))

(defmethod weblocks-stores:object-id ((obj hu.dwim.perec:persistent-object))
  (hu.dwim.perec:oid-of obj))

(defmethod (setf weblocks-stores:object-id) (value (obj hu.dwim.perec:persistent-object))
  (setf (hu.dwim.perec:oid-of obj) value))

(defmethod store-type ((obj hu.dwim.perec:database-mixin))
  :perec)

(defmethod hu.dwim.perec::slot-value-using-class :around ((cls hu.dwim.perec:persistent-class) obj obj3)
  (maybe-begin-transaction (hu.dwim.rdbms::database-of hu.dwim.perec:*transaction*))
  (handler-bind ((error (lambda (c)
                          (when (find-restart 'hu.dwim.perec::load-instance c)
                            (invoke-restart (find-restart 'hu.dwim.perec::load-instance c))))))
    (call-next-method)))

(defmethod (setf hu.dwim.perec::slot-value-using-class) :around (value (cls hu.dwim.perec:persistent-class) obj obj3)
  (handler-bind ((error (lambda (c)
                          (when (find-restart 'hu.dwim.perec::load-instance c)
                            (invoke-restart (find-restart 'hu.dwim.perec::load-instance c))))))
    (call-next-method)))

(defmethod weblocks-stores:class-visible-slots-impl ((cls hu.dwim.perec:persistent-class) &key readablep writablep)
  (remove-if 
    (lambda (item)
      (find (c2mop:slot-definition-name item)
            (list 'hu.dwim.perec::oid 'hu.dwim.perec::persistent 'hu.dwim.perec::transaction 'hu.dwim.perec::transaction-event)))
    (call-next-method)))

(defmethod get-store-type ((store hu.dwim.perec:database-mixin))
  :perec)
