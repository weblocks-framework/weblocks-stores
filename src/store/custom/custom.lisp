
(in-package :weblocks-custom)

(defclass custom-store ()
  ((schema :initarg :classes :initform nil)))

(defclass data-element ()
  ((data-class :initform nil :initarg :data-class)
   (data :initform nil :initarg :data)
   (store :initform nil :initarg :store)))

(defmethod initialize-instance :before ((obj custom-store) &rest args &key redefining-p)
  (let ((schema (getf args :classes)))
    (loop for (key value) on schema :by #'cddr do 
          (when (and (find-class key nil) (not (getf args :redefining-p)))
            (error "Class ~A already exists" key))
          (eval `(defclass ,key (data-element) 
                   ,(loop for (key reader) in (getf value :slots)
                          collect `(,key))))
          (eval `(defmethod initialize-instance :after ((obj ,key) &rest args)
                    ,@(loop for (key reader) in (getf value :slots) collect 
                            `(setf (slot-value obj ',key) (funcall ,reader obj (slot-value obj 'data)))))))))

(defmethod open-store ((store-type (eql :custom)) &rest args)
  (declare (ignore args))
  (setf *default-store* (apply #'make-instance (list* 'custom-store args))))

(defmethod close-store ((store custom-store))
  (with-slots (schema) store
    (loop for (key value) on schema :by #'cddr do
          (when (getf value :close-store)
            (funcall (getf value :close-store) store)))))

(defmacro with-class-property (store class-name property &body body)
  `(with-slots (schema) ,store 
     (let* ((class-data (getf schema ,class-name))
            (,property (getf class-data ,(intern (string-upcase property) :keyword))))
       ,@body)))

(defmethod count-persistent-objects ((store custom-store) class-name &key &allow-other-keys)
  (with-class-property 
    store class-name count-objects
    (if count-objects 
      (funcall count-objects class-name)
      (length (find-persistent-objects store class-name)))))

(defun objects->data-elements (list-of-objects class-name store)
  (loop for i in list-of-objects 
        collect (make-instance class-name 
                               :data i 
                               :data-class class-name 
                               :store store))) 

(defmethod find-persistent-objects ((store custom-store) 
                                    class-name
                                    &key (filter nil) order-by range)

  (let (return)
    (setf return 
          (with-class-property 
            store class-name find-objects
            (with-class-property 
              store class-name find-all-objects
              (cond 
                (find-objects
                  (objects->data-elements 
                    (funcall find-objects :filter filter :order-by order-by :range range)
                    class-name 
                    store))
                (find-all-objects
                  (weblocks-memory:range-objects-in-memory
                    (weblocks-memory:order-objects-in-memory
                      (let ((seq (objects->data-elements 
                                   (funcall find-all-objects)
                                   class-name 
                                   store)))
                        (if (and seq
                                 (functionp filter))
                          (remove-if-not filter seq)
                          seq))
                      order-by)
                    range))))))
    return))

(defmethod object-id ((obj data-element))
  (with-slots (store data) obj
    (with-class-property 
      store (type-of obj) object-id
      (let ((id (if object-id 
                  (funcall object-id obj data)
                  data)))
        (unless (or (stringp id) (integerp id) (symbolp id))
          (error "Object id (~A) for ~A is not integer, please provide correct :object-id lambda" id obj))
        id))))

(defmethod weblocks:class-visible-slots-impl :around (class &key readablep writablep)
  (declare (ignore readablep writablep))
  (if (subtypep class 'data-element)
    (mapstores 
      (lambda (store)
        (when (typep store 'custom-store)
          (with-class-property store (class-name class) slots 
            (when slots 
              (return-from 
                weblocks:class-visible-slots-impl 
                (loop for (slot accessor) in slots 
                      collect (make-instance 
                                'custom-slot 
                                :slot slot))))))))
    (call-next-method)))

(defclass custom-slot ()
  ((slot :initarg :slot)))

(defmethod c2mop:slot-definition-name ((obj custom-slot))
  (slot-value obj 'slot))

(defmethod c2mop:slot-definition-type ((obj custom-slot))
  t)

(defmethod begin-transaction ((store custom-store))
  ; No support for composable transactions
  nil)

(defmethod commit-transaction ((store custom-store))
  ; No support for composable transactions
  nil)

(defmethod rollback-transaction ((store custom-store))
  ; No support for composable transactions
  nil)

(defmethod replace-on-redefine-p ((store-type (eql :custom)))
  t)

(defmethod clean-store ((store custom-store))
  "No need in cleaning, just overriding"
  (declare (ignore store)))

