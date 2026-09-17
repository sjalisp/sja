(in-package #:sja)

(defun str-trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun str-in (sub s)
  (search sub s))

(defun concat-strings (strings &optional (sep ""))
  (if strings
      (reduce (lambda (a b) (format nil "~a~a~a" a sep b)) strings)
      ""))

(defun last-subpart (s sep)
  (let* ((where (position sep s :from-end t)))
    (if (not (null where))
	(subseq s (1+ where)))))

(defun prefix-last (s sep)
  (let* ((where (position sep s :from-end t)))
    (if (not (null where))
	(values (subseq 0 where) (subseq s (1+ where))))))

(defun before-and-last (lst)
  (let ((rev (reverse lst)))
    (values (reverse (cdr rev)) (car rev))))

(defun unique-string-key (name table &optional (sep-char #\:))
  (let ((known (gethash name table)))
    (if known
	(let ((names (filter 
		      (lambda (item) (str-starts-with name item))
		      (dict-keys table))))
	  (ensure-unique name names sep-char))
	name)))

(defun ensure-unique (name names &optional (sep-char #\:))
  (if (member name names)
      (labels ((suffix-as-int (name)
		 (multiple-value-bind (prev suffix) (prefix-last name #\:)
		     (if (and
			  suffix
			  (int-stringp suffix)
			  (string= prev name))
			 (parse-integer suffix)
			 -1))))
	(let ((prev (reduce (lambda (v s) (max v (suffix s))) names :initial-value 0)))
	  (format nil "~a~a~a" name sep-char (1+ prev))))
      name))

(defun str-split-trim (split seq)
  (mapcar #'str-trim (str:split split seq :omit-nulls t)))

(defun str-starts-with (pred str)
  (alexandria::starts-with-subseq pred str))

(defun str-ends-with (pred str)
  (alexandria::ends-with-subseq pred str))

(defun int-stringp (s)
  (and (stringp s) (ignore-errors (parse-integer s))))

(defun str-find (substr str)
  (search substr str))

(defun max-string (strings)
  (reduce (lambda (a b) (if (string> a b) a b)) strings))

(defun str-replace (substr replacement str)
  (str:replace-all substr replacement str))



