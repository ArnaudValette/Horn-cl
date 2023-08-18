(in-package :horn-system)

(defun horn-node-to-string (stream value)
  (format stream "~vt~a, ~a, ~a ~a ~%" (* 5 (level value)) (level value) (text value) (type-of-node value) (length (glitter-nodes value)))
  (if (children value)
      (progn
      (map 'vector (lambda (el) (horn-node-to-string stream el)) (children value)
      ))))

(defun hash-table-to-string (hash-table)
  (with-output-to-string (stream)
    (maphash
     (lambda (key value)
       (horn-node-to-string stream value))
     hash-table)))

(defun list-of-horn-to-string (l)
  (with-output-to-string (stream)
    (map 'vector (lambda (el)
              (horn-node-to-string stream el)) l)))


;;; TEST GLITTER-HORN-
(defun replace-helper (node)
  (replace-files node)
  (replace-links node)
  (replace-images node)
  (replace-verbatim node)
  (replace-bold node)
  (replace-italic node))

(defun process-glitters (a-horn-node)
  (replace-helper a-horn-node)
  (if (children a-horn-node)
      (recurse-and-process-glitters-in-horns (slot-value a-horn-node 'children))))

(defun recurse-and-process-glitters-in-horns (some-forest)
  (map 'vector (lambda (horn)
                 (process-glitters horn)) some-forest))

