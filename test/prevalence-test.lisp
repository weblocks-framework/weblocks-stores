(in-package :weblocks-store-test)

(weblocks-stores:defstore *prevalence-store* :prevalence 
                          (merge-pathnames (make-pathname :directory '(:relative "data"))
                                           (asdf-system-directory :weblocks-store-test)))

(deftestsuite prevalence-suite ()
              ()
              (:setup (weblocks-stores:open-stores))
              (:teardown (clean-store *prevalence-store*)
                         (weblocks-stores::close-stores))
              (:documentation "Tests for the prevalence store."))

(deftest-store prevalence-list-model-classes-1
               (progn
                 (clean-store *prevalence-store*)
                 (persist-object *prevalence-store* (make-instance 'persistent-1 :slot-1 1 :slot-2 2))
                 (list-model-classes *prevalence-store*))
               (persistent-1))
