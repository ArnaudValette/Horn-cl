(in-package :horn-system)
(defstruct (a-state :conc-name)
(headings (make-hash-table))
(last-heading nil)
(is-list-mode nil)
(lists (make-hash-table))
(last-list-node nil)
(is-src-mode nil)
(last-src nil)
(c 0))

(defvar *state* (make-a-state))
(defvar *roots* (empty-arr))


