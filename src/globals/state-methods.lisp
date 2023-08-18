(in-package :horn-system)

(defun empty-arr ()
    (make-array 0 :fill-pointer 0 :adjustable t))

(defun init-values ()
(setf (headings *state*) (make-hash-table))
(setf (last-heading *state*) nil)
(setf (is-list-mode *state*) nil)
(setf (lists *state*) (make-hash-table))
(setf (last-list-node *state*) nil)
(setf (is-src-mode *state*) nil)
(setf (last-src *state*) nil)
(setf (c *state*) 0)
(setf *roots* (empty-arr)))

(defun increase-c ()
(setf (c *state*) (+ 1 (c *state*))))

(defun reset-mode ()
(setf (is-list-mode *state*) nil)
(setf (is-src-mode *state*) nil))

(defun list-mode ()
(setf (is-list-mode *state*) T)
(setf (is-src-mode *state*) nil))

(defun src-mode ()
(setf (is-list-mode *state*) nil)
(setf (is-src-mode *state*) T))

