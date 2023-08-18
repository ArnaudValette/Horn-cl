(in-package :horn-system)

(deftype node-type ()
  '(member article heading paragraph list-node list-element src tag verbatim bold italic link image))
(defun list-of-nodes-p (lst)
  (every (lambda (item) (typep (type-of-node item) 'node-type)) lst))
(defun list-of-strings-p (lst)
  (every (lambda (item) (stringp item)) lst))
(deftype list-of-nodes ()
  '(satisfies list-of-nodes-p))
(deftype list-of-strings ()
  '(satisfies list-of-strings-p))

(defclass horn-node ()
  ((children
    :initform (empty-arr)
    :type list-of-nodes
    :accessor children)
   (text
    :initarg :text
    :accessor text)
   (type-of-node
    :initarg :type-of-node
    :type node-type
    :accessor type-of-node)
   (ix
    :initarg :ix
    :accessor ix)
   (level
    :initarg :level
    :accessor level)
   (parent
    :initarg :parent
    :accessor parent
    )
   (glitter-nodes
    :initform (empty-arr)
    :accessor glitter-nodes)
   (tags
    :initform (empty-arr)
    :type list-of-strings
    :accessor tags)))

