(in-package :horn-system)
(defun add-root (node)
  (vector-push-extend node *roots*)
  (push-node 1 node))

(defun branch-to-parent (node lvl)
  (if (< (slot-value (last-heading *state*) 'level) lvl)
      (connect-parent-and-child (last-heading *state*) node)
      (connect-parent-and-child (find-parent node) node)))

(defun find-parent (node)
  (most-recent (headings *state*) (level node)))

(defun most-recent (ordered-table max-level)
  (let ((result (make-hash-table)) (max 0))
    (do ((counter 0 (+ 1 counter)))
        ((eq counter max-level) result)
       (if (and (gethash counter ordered-table) (> (slot-value (gethash counter ordered-table) 'ix) max))
           (progn
             (setf result (gethash counter ordered-table))
             (setf max (ix result))
           )
    )
    )))

(defun connect-parent-and-child (par child)
  (vector-push-extend child (children par))
  (setf (parent child) par)
  (push-node (level child) child))

(defun push-node (level node)
  (setf (gethash level (headings *state*)) node)
  (setf (last-heading *state*) node))
