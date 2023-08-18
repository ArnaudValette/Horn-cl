(in-package :horn-system)

(defun heading-node-p (line)
  (cl-ppcre:scan "^\\*+\\s" line))

(defun list-node-p (line)
  (cl-ppcre:scan "^\\s*-\\s" line))

(defun src-node-p (line)
  (or
   (cl-ppcre:scan "^#\\+name:" line)
   (cl-ppcre:scan "^#\\+begin_src" line)
   (cl-ppcre:scan "^#\\+end_src" line)
   ))
