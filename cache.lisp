(in-package :sja)

(defclass doubly-linked-node ()
  ((prev :accessor prev :initarg :prev :initform nil)
   (next :accessor next :initarg :next :initform nil)
   (object :accessor object :initarg :object)))

(defclass doubly-linked-list ()
  ((head :accessor head :initarg :head :initform nil)
   (tail :accessor tail :initarg :tail :initform nil)))

(defun same-node (node1 node2)
  (and node1 node2
       (eq (prev node1) (prev node2))
       (eq (next node1) (next node2))
       (eq (object node1) (object node2))))

(defmethod contains-node ((node doubly-linked-node) (llist doubly-linked-list))	    
  (do ((cur (head llist) (next cur)))
      ((or (null cur) (same-node cur node)) cur)))

(defmethod detach-node ((node doubly-linked-node) (llist doubly-linked-list))
  (when node
    (let ((prev (prev node)) (next (next node)))
      (if prev
	  (setf (next prev) next)
	  (setf (head llist) next))
      (if next
	  (setf (prev next) prev)
	  (setf (tail llist) prev)))))
    
(defun put-node-between (before node after)
  (setf (prev node) before)
  (if before
      (setf (next before) node))
  (setf (next node) after)
  (if after
      (setf (prev after) node)))

(defmethod make-head ((node doubly-linked-node) (llist doubly-linked-list))
    (unless (same-node node (head llist))
      (let ((head (head llist)))
	(put-node-between nil node head)
	(setf (head llist) node)
	(if (not (tail llist)) (setf (tail llist) node)))))

(defmethod promote-to-head ((node doubly-linked-node) (llist doubly-linked-list))
  (detach-node node llist)
  (make-head node llist))




(defclass lru-cache ()
  ((node-map :reader node-map :initarg :node-map :initform (make-string-table))
   (size :accessor size :initform 0)
   (max-size :reader max-size :initarg :max-size :initform 10000)
   (key-fn :reader key-fn :initarg :key-fn )
   (llist :reader llist :initform (make-instance 'doubly-linked-list))))

(defmethod cache-get (key (lru lru-cache))
  (let ((node (gethash key (node-map lru))))
    (when node
      (promote-to-head node (llist lru))
      (object node))))


(defmethod cache-drop-node (node (lru lru-cache))
  (let ((key (funcall (key-fn lru) (object node))))
    (detach-node node (llist lru))
    (remhash key (node-map lru))
    (decf (size lru))))

(defmethod cache-keys ((lru lru-cache))
  (dict-keys (node-map lru)))
  
(defmethod cache-insert (object (lru lru-cache))
  (let* ((key (and object (funcall (key-fn lru) object)))
	 (node (unless (or (not object) (gethash key (node-map lru)))
		 (make-instance 'doubly-linked-node :object object))))
    (when node
      (make-head node (llist lru))
      (setf (gethash key (node-map lru)) node)
      (incf (size lru))
      (if (> (size lru) (max-size lru))
	  (cache-drop-node (tail (llist lru)) lru))
      object)))


(defmethod cache-remove (key (lru lru-cache))
  (let ((node (gethash key (node-map lru))))
    (if node
      (cache-drop-node node lru))))

    
	
	
