(cl:in-package :claw)

(define-constant +adapted-variable-prefix+ "__v_claw_"
  :test #'equal)


(defun load-static-adapter-template ()
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :claw "src/template/static.c")))


(defun load-dynamic-adapter-template ()
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname :claw "src/template/dynamic.c")))


(defun preprocess-template (source &rest args)
  (labels ((%replace (name value source)
             (let ((regex (format nil "{{\\s*~A\\s*}}"
                                  (ppcre:quote-meta-chars name))))
               (ppcre:regex-replace-all regex source value)))
           (%to-source (result arg)
             (destructuring-bind (name . value) arg
               (%replace name value result))))
    (reduce #'%to-source (alexandria:plist-alist args) :initial-value source)))


(defun preprocess-static-adapter-template (header-file function-definitions)
  (preprocess-template (load-static-adapter-template)
                       "header-file" header-file
                       "function-definitions" function-definitions))


(defun preprocess-dynamic-adapter-template (header-file
                                            loader-name
                                            function-pointers
                                            function-pointers-init
                                            function-definitions)
  (preprocess-template (load-dynamic-adapter-template)
                       "header-file" header-file
                       "loader-name" loader-name
                       "function-pointers" function-pointers
                       "function-pointers-init" function-pointers-init
                       "function-definitions" function-definitions))


(defun %make-variable-name (name)
  (format nil "~A~A" +adapted-variable-prefix+ name))


(defun %generate-function-variable (function out)
  (format out "static ")
  (let ((variable-name (%make-variable-name (library-function-name function))))
    (generate-adapted-function-variable function variable-name out))
  (format out ";"))


(defun %generate-function-variable-init (function out)
  (format out "~A = claw_get_proc_addr(\"~A\");"
          (%make-variable-name (library-function-name function))
          (library-function-name function)))


(defun %generate-adapted-function-definitions (functions)
  (with-output-to-string (out)
    (loop for fu in functions
          do (format out "~&~%__CLAW_API ")
             (generate-adapted-function-definition fu))))


(defun generate-static-adapter (library &optional output)
  (format output "~A"
          (preprocess-static-adapter-template
           (library-header-file (list-adapted-functions library))
           (%generate-adapted-function-definitions library))))


(defun generate-dynamic-adapter (library &optional output)
  (let ((functions (list-adapted-functions library)))
    (flet ((function-variables ()
             (with-output-to-string (out)
               (loop for fu in functions
                     do (format out "~%")
                        (%generate-function-variable fu out))))
           (function-variable-inits ()
             (with-output-to-string (out)
               (loop for fu in functions
                     do (format out "~%")
                        (%generate-function-variable-init fu out)))))
      (format output "~A"
              (preprocess-dynamic-adapter-template
               (library-header-file library)
               (library-loader-name library)
               (function-variables)
               (function-variable-inits)
               (%generate-adapted-function-definitions functions))))))


(defun build-static-adapter (library target-file)
  (declare (ignore library target-file))
  (error "unimplemented"))


(defun build-dynamic-adapter (library target-file)
  (declare (ignore library target-file))
  (error "unimplemented"))
