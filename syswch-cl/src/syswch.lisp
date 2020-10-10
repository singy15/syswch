(defpackage syswch
  (:use
    cl
    easy-routes)
  (:export
    start-server))
(in-package :syswch)

;;; Variables
(defparameter *watch-interval* 0.5)
(defparameter *watch-filename* "wepl.lisp")
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

;;; Start syswch server
(defun start-server (port)
  (setf *syswch-server* 
        (make-instance 'easy-routes:easy-routes-acceptor :port port))

  (hunchentoot:start *syswch-server*)

  (let ((syswch-cl-dir (asdf:system-relative-pathname :syswch-cl "../syswch-cs/"))
        (dir (sb-posix:getcwd)))
    (sb-posix:chdir syswch-cl-dir)
    (setf *syswch-watcher* (sb-ext:run-program 
        "C:\\Program Files\\dotnet\\dotnet.exe" 
        (list "run" (ppcre:regex-replace-all "/" (namestring dir) "\\") (write-to-string port)) 
        :output t :input *standard-input* :wait nil))
    (sb-posix:chdir dir))

  (wepl-start))

;;; Define GET /reload
(defroute get-reload ("/reload" :method :get) (file)
  (hunchentoot:log-message* :INFO "reloaded ~a" file)

  (cond 
    ((equal "wepl.lisp" (file-namestring file)) nil)
    ((equal "lisp" (pathname-type file)) (load file))
    ((equal "asd" (pathname-type file)) nil))

  ; (load file) 
  (format nil "reload ~a" file))

(in-package :cl-user)

