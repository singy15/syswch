(defpackage syswch
  (:use
    cl)
  (:export
    start-server))
(in-package :syswch)

;;; Variables
(defparameter *watch-interval* 0.5)
(defparameter *watch-filename* "i.lisp")
(defparameter *watch-system* nil)
(defparameter *wepl-signal* :stopped)
(defparameter *syswch-server* nil)
(defparameter *syswch-watcher* nil)

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
              (format *standard-output* "~%~a~%" 
                      (eval (car (reverse (read-source)))))
              (finish-output *standard-output*))))

        ;; Flush *standard-output*
        (finish-output *standard-output*))))

;;; Start server
(defun start-server (port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port))
         (thread nil)
         (stdout *standard-output*))

    (format stdout "Starting syswch server on port=~a...~%" port)

    ;; Start thread
    (setf thread 
          (bordeaux-threads:make-thread 
            (lambda ()
              (let ((*standard-output* stdout)
                    (connection (usocket:socket-accept socket :element-type 'character)))
                (loop
                  (usocket:wait-for-input connection)
                  (let ((msg (read-line (usocket:socket-stream connection))))
                    (format *standard-output* "RELOAD: ~a~%" msg)
                    (block reload-source
                      (handler-bind
                        ((error #'(lambda (cond)
                                    (format *standard-output* "ERROR: ~a" cond)
                                    (return-from reload-source))))
                        (cond 
                          ((equal "i.lisp" (file-namestring msg)) nil)
                          ((equal "lisp" (pathname-type msg)) (load msg))
                          ((equal "asd" (pathname-type msg)) nil)
                          (t nil))))))))))

    ;; Launch filesystem watcher
    (let ((syswch-cl-dir (asdf:system-relative-pathname :syswch-cl "../syswch-cs/"))
          (dir (sb-posix:getcwd)))
      (sb-posix:chdir syswch-cl-dir)
      (setf *syswch-watcher* (sb-ext:run-program 
          "C:\\Program Files\\dotnet\\dotnet.exe" 
           (list "run" (ppcre:regex-replace-all "/" (namestring dir)  "\\") (write-to-string port)) 
          :output t 
          ; :input *standard-input* 
          :wait nil))
      (sb-posix:chdir dir))

    ;; Start wepl
    (bordeaux-threads:make-thread #'wepl-start)))


(in-package :cl-user)

