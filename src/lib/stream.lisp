(in-package :horn-system)

(defun get-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun org-to-horn (line)
  (node-switch line))

(defun org-file-to-horns (path)
  (mapcar #'org-to-horn (get-file path)))

