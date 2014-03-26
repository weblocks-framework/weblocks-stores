
(defpackage #:weblocks-montezuma
  (:use :cl :weblocks :weblocks-stores)
  (:documentation
   "A driver for weblocks backend store API that connects to montezuma index.")
  (:export #:*sorting-enabled-p*))

(in-package :weblocks-montezuma)

(weblocks-stores:register-store-type :montezuma)

(defvar *sorting-enabled-p* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :montezuma)) &rest args)
  (setf *default-caching* nil)
  (setf *default-store* (apply #'make-instance (list* 'montezuma:index args))))

(defmethod close-store ((store montezuma:index))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (montezuma:close store))

#+l(defmethod clean-store ((store database)))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store montezuma:index))
  (declare (ignore store))
  ; TODO
  )

(defmethod commit-transaction ((store montezuma:index))
  (declare (ignore store))
  ; TODO
  )

(defmethod rollback-transaction ((store montezuma:index))
  (declare (ignore store))
  ; TODO
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+l(defmethod persist-object ((store database) object &key))

#+l(defmethod delete-persistent-object ((store database) object))

#+l(defmethod delete-persistent-object-by-id ((store database) class-name object-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+l(defmethod find-persistent-object-by-id ((store database) class-name object-id))

(defmethod find-persistent-objects ((store montezuma:index)
                                    class-name &key
                                    order-by range where &allow-other-keys)
  (when where
    (error "Unimplemented"))
  (flet ((montezuma-document->object (doc)
           (let ((obj (make-instance class-name))
                 (field))
             (loop for slot in (mapcar #'c2mop:slot-definition-name (class-visible-slots-impl (find-class class-name))) do 
                   (setf field (montezuma:document-field doc (string-downcase slot)))
                   (setf (slot-value obj slot) 
                         (when field 
                           (montezuma:field-data field))))
             obj)))

    (let ((result))
      (montezuma:each 
        (montezuma:search 
          *default-store* (make-instance 'montezuma::match-all-query)
          :first-doc (car range)
          :num-docs (when range 
                      (- (cdr range) (car range)))
          :sort 
          (when (and order-by (or 
                                *sorting-enabled-p* 
                                (warn "Sorting is not enabled")))
            (make-instance 'montezuma::sort 
                           :fields (list 
                                     (make-instance 'montezuma::sort-field
                                                    :name (string-downcase (car order-by))
                                                    :reverse-p (equal (cdr order-by) :asc) 
                                                    ;:sort-type (montezuma::make-sort-type "title") 
                                                    )))))
        (lambda (score-doc)
          (push 
            (montezuma-document->object 
              (montezuma:get-document 
                *default-store* (montezuma:doc score-doc)))
            result)))

      result)))

(defmethod count-persistent-objects ((store montezuma:index) 
                                     class-name
                                     &key where
                                     &allow-other-keys)
  (when where 
    (error "Unimplemented"))
  ;(montezuma::document-count store)
  (montezuma::num-docs (montezuma:reader store)))
