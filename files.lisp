(in-package :com.gigamonkeys.utilities)

(defconstant +block-size+ (expt 2 12))

(defun dump-file (file stream &key (element-type (stream-element-type stream)))
  "Dump the contents of file to stream."
  (with-open-file (in file :element-type element-type)
    (let ((buffer (make-array +block-size+ :element-type element-type)))
      (loop for end = (read-sequence buffer in)
	   do (write-sequence buffer stream :end end)
	   until (< end (length buffer))))))

(defun file-text (pathname)
  (with-output-to-string (out)
    (dump-file pathname out)))

(defun file-bytes (file)
  (with-open-file (in file) (file-length in)))

(defun file-char-length (file &key (external-format :utf-8))
  "The length of the file in characters for a given external-format."
  (with-open-file (in file :external-format external-format)
    (loop for c = (read-char in nil nil)
       while c count t)))

(defun copy-file (from to)
  (with-open-file (out to :direction :output :if-exists :supersede :element-type 'unsigned-byte)
    (dump-file from out))
  (truename to))


(defun file->lines (filename)
  (when (probe-file filename)
    (with-open-file (in filename)
      (loop for item = (read-line in nil nil) while item collect item))))

(defun file->list (filename &optional (package *package*))
  (let ((*package* package))
    (when (probe-file filename)
      (with-open-file (in filename)
	(loop for item = (read in nil nil) while item collect item)))))

(defun file->sexp (filename &optional (package *package*))
  (let ((*package* package))
    (with-open-file (in filename)
      (read in nil nil))))

(defmacro with-output-to-file ((out file &key (ensure-directories nil)) &body body)
  (once-only (file)
    `(with-open-file (,out (if ,ensure-directories (ensure-directories-exist ,file) ,file)
                           :direction :output :if-exists :supersede)
       ,@body
       (truename ,out))))