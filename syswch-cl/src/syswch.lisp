(defpackage syswch
  (:use
    cl)
  (:export
    wepl-start
    wepl-stop))
(in-package :syswch)

;;; Load libraries
(ql:quickload :alexandria :silent t)

;;; Variables
(defparameter *watch-interval* 0.5)
(defparameter *watch-filename* "wepl.lisp")
(defparameter *watch-system* nil)
(defparameter *wepl-signal* :stopped)

;;; Read file
(defun slurp (path)
  (alexandria:read-file-into-string path :external-format :utf-8))

;;; Write file
(defun spit (path content)
  (alexandria:write-string-into-file content path 
    :external-format :utf-8 :if-exists :supersede))

;;; Signal stop
(defun wepl-stop ()
  (setf *wepl-signal* :stopped))

;;; Wrap source code with "progn"
(defun envelope (source)
  (format nil "(~a)" source))

;;; Read source string and create forms
(defun string-to-forms (source)
  (read-from-string source))

;;; Read source
(defun read-source ()
  (string-to-forms (envelope (slurp *watch-filename*))))

;;; Get file write date
(defun write-date ()
  (file-write-date *watch-filename*))

;;; Write separator
(defun print-separator ()
  (format *standard-output* "--~%"))

;;; Start Watch-Eval-Print-Loop
(defun wepl-start ()
  (let* ((last-modified (write-date))
         (interval *watch-interval*))

    ;; Signal running
    (setf *wepl-signal* :running)

    ;; Start loop
    (loop 
        ;; Exit when stop signaled
        (when (equal *wepl-signal* :stopped)
          (return-from wepl-start))

        ;; Sleep
        (sleep interval)

        ;; Eval when file modified (check file write date)
        (when (not (equal last-modified (write-date)))
          (setf last-modified (write-date))
          (print-separator)

          ;; Block for catch error caused by user code
          (block eval-source
            (handler-bind
              ((error #'(lambda (cond)
                          (format *standard-output* "ERROR: ~a" cond) 
                          (return-from eval-source))))
              (format *standard-output* "~%~a~%" (eval (car (reverse (read-source)))))
              (finish-output *standard-output*))))

        ;; Flush *standard-output*
        (finish-output *standard-output*))))

(in-package :cl-user)

;; Execute
(syswch:wepl-start)
(cl-user::exit)

