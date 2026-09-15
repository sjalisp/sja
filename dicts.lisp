(in-package #:sja)

(defun dict-keys (ht) (alexandria:hash-table-keys ht))

(defun dict-values (ht) (alexandria:hash-table-values ht))

(defun dict-map-keys (fn ht) (alexandria:maphash-keys fn ht))

(defun make-string-table () (make-hash-table :test #'string= :hash-function #'sxhash))

(defun dict-map-values (fn ht) (alexandria:maphash-values fn ht))

(defun dict-get (key ht &optional default)
  (multiple-value-bind (val present) (gethash key ht)
    (if present val default)))

(defun dict-has-key (k ht)
  (nth-value 1 (gethash k ht)))

(defun dict-remove-keys (test ht)
  (dolist (k (dict-keys ht) ht)
    (when (funcall test k)
      (remhash k ht))))

(defun dict-without-keys (ht &rest keys)
  (let ((to-remove (filter (lambda (k) (dict-has-key k ht)) keys)))
    (dolist (k to-remove ht)
      (remhash k ht))))

(defun update-hash (target mods)
  (maphash (lambda (k v)
	     (setf (gethash k target) v))
	   mods))
