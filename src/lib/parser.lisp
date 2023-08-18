(in-package :horn-system)

(defun node-switch (line)
  ;;; parses a line, and in function of the state, redirects it to the correct function
  (cond
    ((heading-node-p line) (handle-heading line))
    ((list-node-p line) (handle-list line))
    ((src-node-p line) (handle-src line))
    (T (handle-paragraph line))
    )
  )


(defun get-heading-level (line)
  (- (length (cl-ppcre:scan-to-strings "^\\*+\\s" line)) 1 ))

(defun get-text (line level)
  (subseq line level (length line)))

(defun handle-heading (line)
  (reset-mode)
  (let* (
         (level (get-heading-level line))
         (text (get-text line level))
         (node (make-instance 'horn-node
                              :text text
                              :type-of-node 'heading
                              :ix (c *state*)
                              :parent 'nil
                              :level level
                              )))
    (setf (c *state*) (+ 1 (c *state*)))
    (if (eq level 1)
        (add-root node)
        (branch-to-parent node level))))

;;; LIST NODE HANDLING

(defun push-list-node (level node)
  "SUBSCRIPTION:
  The last list node encountered is the current one.
  the children slot of the parent node contains our node.
  "
  (setf (gethash (level node) (lists *state*)) node)
  (setf (last-list-node *state*) node)
  (vector-push-extend node (children (parent node))))

(defun push-child-list-node (node)
  "2. Since we're on a child-list-node,
   the parent is the last list-node encountered.
   We subscribe our list node to the datastructure"
   (setf (parent node) (last-list-node *state*))
   (push-list-node (level node) node))

(defun push-root-list-node (node)
  "1. Since we're on a root list node,
   we toggle the list mode boolean.
   The parent is the last heading encountered.
   Then we subscribe our list node to the
   data structure."
  (list-mode)
  (setf (parent node) (last-heading *state*))
  (push-list-node (level node) node))


(defun handle-list-node (node)
  "Create a list-node (think html's <ul> tag)
   if we're not in list mode,
   that's a root list node,
   else, the list node is the children
   of another one
   We insert the node passed as parameter to that function
   as a child of this list-node (think html's <li> tag)."
  (let* ((parent-node (make-instance 'horn-node
                              :text ""
                              :ix (c *state*)
                              :parent 'nil
                              :level (level node)
                              :type-of-node 'list-node)))
  (setf (type-of-node node) 'list-element)
  (setf (c *state*) (+ 1 (c *state*)))
  (vector-push-extend node (children parent-node))
  (if (not (is-list-mode *state*))
      (push-root-list-node parent-node)
      (push-child-list-node parent-node))
  ))

;;; LIST ELEMENT
(defun handle-list-element (node)
  "Since we're on a list-element node
  The parent is the last list-node node
  of the same level."
  (setf (type-of-node node) 'list-element)
  (setf (parent node) (gethash (level node) (lists *state*)))
  (vector-push-extend node (children (parent node)))
  )

;;; HELP, list level and text content
(defun get-list-level (line)
  (+  (/ (length (cl-ppcre:scan-to-strings "^\\s*" line)) 2 ) 1 ))
(defun get-list-text (level line)
  (subseq line (* 2 level) (length line)))
(defun handle-list (line)
  (let* ((level (get-list-level line))
         (text (get-list-text level line)))
  (list-node-or-element level text)))

;;; CONDITION
(defun should-create-list-node-p (level)
  "we should create a list node
   when we're not in list mode,
   or when the level of our current list node is
   superior than the last one" 
  (or (not (is-list-mode *state*))
      (and
       (last-list-node *state*)
       ( > level
         (slot-value
            (last-list-node *state*)
            'level)
        ))))

;;; PROPER LIST HANDLER
(defun list-node-or-element (level text)
  "Create a node, and call the adequate function
   according to the condition"
  (let* ((node (make-instance 'horn-node
                              :text text
                              :ix (c *state*)
                              :parent 'nil
                              :level level
                              :type-of-node 'nil)))
  (setf (c *state*) (+ 1 (c *state*)))
  (if (should-create-list-node-p level)
    (handle-list-node node)
    (handle-list-element node))))



(defun handle-src (line)
  (cond
    ;;;
    ((cl-ppcre:scan "^#\\+end_src" line) (handle-end line))
    ((cl-ppcre:scan "^#\\+begin_src" line) (handle-begin line))
    ((cl-ppcre:scan "^#\\+name:" line) (handle-name line))))


(defun handle-name (line)
  (let* ((name (get-block-txt line "^#\\+name:"))
         (node (make-instance 'horn-node
                              :text name
                              :type-of-node 'src
                              :ix (c *state*)
                              :parent 'nil
                              :level 1)))
    (src-mode)
    (increase-c)
    ;;; set the parent of our node
    (setf (parent node) (last-heading *state*))
    ;;; set-it as the last-src 
    (setf (last-src *state*) node)
    (vector-push-extend node (children (parent node)))))

(defun handle-begin (line)
  nil)

(defun handle-end (line)
  (reset-mode))



(defun get-block-txt (line regex)
  (subseq line (length (cl-ppcre:scan-to-strings regex line)) (length line)))
;;;(setf (c *state*) (+ 1 (c *state*)))


   (defun handle-paragraph (line)
        (cond
          ((is-list-mode *state*) (subscribe-paragraph line))
          ((is-src-mode *state*) (fill-code-block line))
          (T (subscribe-paragraph line))
          )
        )

  (defun subscribe-paragraph (line)
    (if (last-heading *state*)
        (sub-paragraph line)
        (root-paragraph line)))

(defun root-paragraph (line)
  (let ((node (make-instance 'horn-node
                             :text line
                             :type-of-node 'paragraph
                             :ix (c *state*)
                             :parent 'nil
                             :level 1)))

    (increase-c)
    (vector-push-extend node *roots*)))

(defun sub-paragraph (line)
  (let ((node (make-instance 'horn-node
                             :text line
                             :type-of-node 'paragraph
                             :ix (c *state*)
                             :parent (last-heading *state*)
                             :level (slot-value (last-heading *state*) 'level))))
    (reset-mode)
    (increase-c)
    (vector-push-extend node (children (parent node)))
    ))

(defun fill-code-block (line)
  (let ((node (make-instance 'horn-node
                             :text line
                             :type-of-node 'paragraph
                             :ix (c *state*)
                             :parent (last-src *state*)
                             :level (level (last-src *state*)))))
    (increase-c)
          ;;; subscribe
    (vector-push-extend node (children (parent node)))
    ))
