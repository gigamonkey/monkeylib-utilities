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
  (cond
    ((and (probe-file to) (equalp (truename from) (truename to)))
     (warn "Not copying file ~a onto itself." from))
    (t
     (with-open-file (out (ensure-directories-exist to) :direction :output :if-exists :supersede :element-type 'unsigned-byte)
       (dump-file from out))))
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
  "Write to a file, creating if it does not exist and superseding if
it does. Returns the truename of the file created.
If :ensure-directories is true, create parent directories too."
  (once-only (file)
    `(with-open-file (,out (if ,ensure-directories (ensure-directories-exist ,file) ,file)
                           :direction :output :if-exists :supersede)
       ,@body
       (truename ,out))))

(defmacro with-data-io-syntax (&body body)
  "Do i/o with standard io-syntax except with *print-case* bound
to :downcase, *read-eval* bound to nil, and *package* the keyword
package. Useful for more or less safely reading and writing data."
  `(with-standard-io-syntax
     (let ((*print-case* :downcase)
           (*read-eval* nil)
           (*package* (find-package :keyword)))
       ,@body)))

(defmacro with-data-to-file ((out file &key (ensure-directories nil)) &body body)
  "Write data to a file using data-io-syntax, appending to the file if
it exists and creating it if necessary. If :ensure-directories is
true, create parent directories too."
  (once-only (file)
    `(with-open-file (,out (if ,ensure-directories (ensure-directories-exist ,file) ,file)
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
     (with-data-io-syntax ,@body))))


(defmacro with-lock-file ((filename) &body body)
  "Simple minded technique for using the existence of a file as a
locking mechanism to keep cooperative threads or processes from
stomping on eachother."
  (once-only (filename)
    `(progn
       ;; On SBCL anyway, the following OPEN should result in a call
       ;; to open() with O_EXCL which does what we need.
       (loop for x = (open (ensure-directories-exist ,filename) :direction :output :if-exists nil :if-does-not-exist :create)
          when x return (close x))
       (unwind-protect (progn ,@body)
         (delete-file ,filename)))))
