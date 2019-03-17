(cl:in-package :claw)

;; Global
(defmacro with-anonymous-indexing (&body body)
  `(let ((*foreign-record-index* (make-hash-table)))
     ,@body))

(defun foreign-anonymous (id)
  (if *foreign-record-index*
      (gethash id *foreign-record-index*)
      (error "No current anonymous record index")))

(defun (setf foreign-anonymous) (v id)
  (if *foreign-record-index*
      (setf (gethash id *foreign-record-index*) v)
      (error "No current anonymous record index")))
