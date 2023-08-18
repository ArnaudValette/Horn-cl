(in-package :horn-system)

;;; CLASS DEFINITION

(defclass glitter-node ()
  ((type-of-node
    :initarg :type-of-node
    :accessor type-of-node)
   (text
    :initarg :text
    :initform 'nil
    :accessor text)
   (href
    :initarg :href
    :initform 'nil
    :accessor href)
   (src
    :initarg :src
    :initform 'nil
    :accessor src)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   ))

;;; HELPER FUNCTIONS

(defun find-inner-content (target s e)
  (subseq target (+ 1 s) (- e 1)))

(defun find-anchor-ref (target s e)
  (let ((first-seq (subseq target (+ 3 s))))
    (subseq first-seq 0 (position #\] first-seq))))

(defun find-href (target s e)
  (let ((first-seq (subseq target (+ 7 s))))
    (subseq first-seq 0 (position #\] first-seq))))

(defun find-src (target s e)
  (subseq target (+ 2 s) (- e 2)))

(defun pos-after-char (char text)
  (+ 1 (position char text)))

(defun find-text (target s e)
  (let ((first-seq (subseq target (+ 2 s) (- e 2))))
    (subseq first-seq (pos-after-char #\[ first-seq))))

;;; MACRO DEFINITIONS

(defmacro defreplace (name pattern handler)
  `(defun ,name (node)
     (setf (slot-value node 'text) 
           (cl-ppcre:regex-replace-all ,pattern (slot-value node 'text)
                                       #'(lambda (target x y s e &rest regs)
                                           (,handler target s e node))))))

(defmacro defhandler (name node-type finders)
  `(defun ,name (text s e node)
     (let* (
            (glitter (make-instance 'glitter-node
                                    :type-of-node ,node-type
                                    ,@(loop for finder in finders append (list (car finder) `(,(cadr finder) text s e)))
                                    :start s
                                    :end e)))
       (vector-push-extend glitter (glitter-nodes node)))
     (make-string (- e s) :initial-element #\-)
     ))


;;; FUNCTION DEFINITIONS

(defhandler handle-link-found 'anchor ((:href find-anchor-ref) (:text find-text)))
(defhandler handle-image-found 'image ((:src find-src)))
(defhandler handle-file-found 'file ((:href find-href)
                                     (:text find-text)
                                     ))
(defhandler handle-verbatim-found 'verbatim ((:text find-inner-content)))
(defhandler handle-bold-found 'bold ((:text find-inner-content)))
(defhandler handle-italic-found 'italic ((:text find-inner-content)))

(defreplace replace-links "\\[\\[\\*.+\\]\\[.+\\]\\]" handle-link-found)
(defreplace replace-images "\\[\\[[^\\]]+\\]\\]" handle-image-found)
(defreplace replace-files "\\[\\[file:.+?\\]\\[.+?\\]\\]" handle-file-found)
(defreplace replace-verbatim "(~.+~|=.+=)" handle-verbatim-found)
(defreplace replace-bold "\\*.+\\*" handle-bold-found)
(defreplace replace-italic "/.+/" handle-italic-found)






