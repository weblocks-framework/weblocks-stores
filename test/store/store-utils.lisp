
(in-package :weblocks-store-test)

(deftestsuite store/store-utils-suite (weblocks-suite)
  ())

;;; test class-id-slot-name
(deftest class-id-slot-name-1
    (class-id-slot-name 'foobar)
  id)

;;; test object-id-slot-name
(deftest object-id-slot-name-1
    (object-id-slot-name weblocks-test::*joe*)
  id)

;;; test object-id
(deftest object-id-1
    (object-id weblocks-test::*joe*)
  1)

(deftest object-id-2
    (let ((employee (copy-template weblocks-test::*joe*)))
      (setf (object-id employee) 11)
      (object-id employee))
  11)

;;; test class-store
(deftest class-store-1
    (weblocks-test::with-request :get nil
      (string-downcase (symbol-name (class-name (class-of (class-store 'foobar))))))
  "memory-store")

;;; test object-store
(deftest object-store-1
    (weblocks-test::with-request :get nil
      (string-downcase (symbol-name (class-name (class-of (object-store weblocks-test::*joe*))))))
  "memory-store")

;;; Note, defstore, open-stores, and close-stores are tested
;;; implicitly as part of weblocks-test

(addtest reeval-defstore-doesnt-reset
  (ensure (typep weblocks-stores::*stores* 'hash-table))
  (ensure (typep weblocks-stores::*store-names* 'list))
  (let ((fakestore (gensym))
	(weblocks-stores::*stores* (make-hash-table))
	(weblocks-stores::*store-names* '()))
    (eval `(defstore ,fakestore :memory))
    (weblocks-stores::open-stores)
    (unwind-protect
	 (progn
	   (ensure (symbol-value fakestore))
	   (let ((oldval (symbol-value fakestore)))
	     (eval `(defstore ,fakestore :memory))
	     (ensure-same (symbol-value fakestore) oldval)))
      (weblocks-stores::close-stores))))

;;; test mapstores
(deftest mapstores-1
    (weblocks-test::with-request :get nil
      (let ((i 0))
	(mapstores (lambda (store)
		     (declare (ignore store))
		     (incf i)))
	(> i 0)))
  t)

;;; test webapp/store association
(addtest webapp-associates-first-defstore
  (with-test-webapp (:class-name 'app-with-not-searchable-store)
    (with-webapp (current-webapp)
      (ensure-same *default-store* *not-searchable-store*)))
  (weblocks-stores:open-stores)
  (ensure-same *default-store* *test-store*))

;;; test persist-objects
(deftest persist-objects-1
    (weblocks-test::with-request :get nil
      (persist-objects weblocks-stores:*default-store* (list weblocks-test::*joe* weblocks-test::*bob*))
      (count-persistent-objects weblocks-stores:*default-store* 'employee))
  2)

