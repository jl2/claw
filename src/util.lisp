(cl:in-package :claw)

 ;; misc

(declaim (special *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*))

(define-constant +byte-size+ 8)
(define-constant +path-search-regex+
  "\\> search starts here:\\s*((.|\\s)*)?\\s*End of search list"
  :test #'equal)

(define-constant +stupid-darwin-framework-postfix+
  " (framework directory)"
  :test #'equal)

(defun substr* (str start &optional end)
  "Make a shared substring of STR using MAKE-ARRAY :displaced-to"
  (let* ((end (or end (length str)))
         (len (- end start)))
    (make-array len :element-type (array-element-type str)
                    :displaced-to str
                    :displaced-index-offset start)))

(declaim (inline string+))
(defun string+ (string &rest strings)
  (apply #'concatenate 'string (string string)
         (mapcar #'string strings)))

(defun find-prefix (list &key (pred 'string))
  (let* ((sorted-fields (sort (map 'vector pred list) #'string<))
         (first (elt sorted-fields 0))
         (last (when (> (length sorted-fields) 1)
                 (elt sorted-fields (1- (length sorted-fields))))))
    (if (and first last)
        (let ((n (mismatch first last)))
          (cond
            ;; Never allow blanks
            ((and n (> n 0) (= n (length first))) (1- n))
            (n n)
            (t 0)))
        0)))

(defun prefix-trim (list &key (pred 'string) regex)
  (if regex
      (let ((scanner (ppcre:create-scanner regex)))
        (mapcar (lambda (x) (ppcre:regex-replace-all scanner (funcall pred x) "")) list))
      (let* ((prefix-end (find-prefix list :pred pred)))
        (map 'list (lambda (x) (subseq (funcall pred x) prefix-end)) list))))

 ;; alists

(declaim (inline akey aval (setf aval)))
(defun akey (val alist &key (test 'eql)) (car (rassoc val alist :test test)))
(defun aval (key alist &key (test 'eql)) (cdr (assoc key alist :test test)))
(defun (setf aval) (value key alist &key (test 'eql))
  (setf (cdr (assoc key alist :test test)) value))

(defmacro alist-bind ((&rest vars) alist &body body)
  "Inefficient but doesn't really matter here"
  (once-only (alist)
    `(let (,@(mapcar (lambda (x)
                       (if (consp x)
                           `(,(car x) (aval ,(cadr x) ,alist))
                           `(,x (aval ,(make-keyword x) ,alist))))
                     vars))
       ,@body)))

 ;; Symbol trimming

(defun trim-symbols-to-alist (list &optional regex)
  (let* ((scanner (ppcre:create-scanner "(\\W)(.*?)\\1"))
         (trimmed-symbols
           (mapcar (lambda (x)
                     (ppcre:regex-replace scanner (string x) "\\2"))
                   list))
         (keyword-symbols (mapcar #'make-keyword (prefix-trim trimmed-symbols :regex regex))))
    (loop for symbol in list
          for keyword in keyword-symbols
          collect ``(,',keyword . ,,symbol))))

 ;; output

(defun write-nicely (stream object)
  (write object
         :stream stream
         :case :downcase
         :circle t
         :pretty t
         :readably t)
  (format stream "~%~%"))

 ;; testing

(defun included-p (thing includes)
  (when thing
    (loop for scanner in includes do
      (when (cl-ppcre:scan scanner thing)
        (return t)))))

(defun excluded-p (name location)
  (and (or (included-p name *exclude-definitions*)
           (and (included-p location *exclude-sources*)
                (not (included-p name *include-definitions*))))
       (not (or (included-p name *include-definitions*)
                (and (included-p location *include-sources*)
                     (not (included-p name *exclude-definitions*)))))))

(defun anonymous-p (form)
  (etypecase form
    (foreign-type
     (null (symbol-package (foreign-type-name form))))
    (cons
     (or (string= "" (aval :name form))
         (and (string= ":array" (aval :tag form))
              (string= "" (aval :name (aval :type form))))))))


 ;; files

(defun find-file-for-paths (file paths)
  (loop for path in paths
        as filename = (merge-pathnames file path)
        do (when (probe-file filename)
             (return filename))))

 ;; ASDF paths

(defun asdf-path (system &rest path)
  (asdf:component-pathname
   (or (asdf:find-component (asdf:find-system system t) path)
       (error "System ~S path not found: ~S" system path))))

(defun path-or-asdf (form)
  (etypecase form
    ((or string pathname) form)
    (list (apply #'asdf-path (car form) (cdr form)))))

 ;; Conditions

;; from pergamum
(defun report-simple-condition (condition stream)
  (apply #'format stream (simple-condition-format-control condition) (simple-condition-format-arguments condition)))

;; from pergamum
(defmacro define-simple-condition-for (base-type &key object-initarg (simple-condition-type 'simple-error) (signaler 'error)
                                                   (name (format-symbol t "SIMPLE-~A" base-type)))
  `(progn
     (define-condition ,name (,base-type ,simple-condition-type)
       ()
       (:report report-simple-condition))
     (defun ,base-type (,@(when object-initarg `(o)) format-control &rest format-arguments)
       (,signaler ',name ,@(when object-initarg `(,object-initarg o)) :format-control format-control :format-arguments format-arguments))))

;; from pergamum
(defmacro define-simple-error-for (base-type &key name object-initarg)
  "Define a simple error subclassing from BASE-TYPE and a corresponding
function, analogous to ERROR, but also optionally taking the object
against which to err, and passing it to ERROR via the OBJECT-INITARG
keyword. The name of the simple error is constructed by prepending
'SIMPLE-' to BASE-TYPE.
Whether or not the error signaller will require and pass the
object is specified by OBJECT-INITARG being non-NIL."
  `(define-simple-condition-for ,base-type :object-initarg ,object-initarg :simple-condition-type simple-error :signaler error
                                ,@(when name `(:name ,name))))


(defun by-removing-prefix (prefix)
  (list (format nil "^~A\\w+$" prefix)
        (lambda (name)
          (subseq name (length prefix)))))


(defun by-removing-prefixes (&rest prefixes)
  (flet ((by-prefix-length (this-prefix that-prefix)
           (> (length this-prefix)
              (length that-prefix))))
    (mapcar #'by-removing-prefix (stable-sort prefixes #'by-prefix-length))))


(defun by-changing (from to)
  (list (list (format nil "^~A$" from)
              (lambda (name) (declare (ignore name)) (string to)))))


(defun in-pipeline (&rest processors)
  (reduce #'append processors))


(defun by-removing-complex-prefix (regex symbols-to-cut)
  (list (list regex (lambda (name) (subseq name symbols-to-cut)))))


#+ccl
(defmacro with-float-traps-masked-on-ccl ((&rest masks) &body body)
  (flet ((expand-mask (mask)
           (list (case mask
                   (:divide-by-zero :division-by-zero)
                   (t mask))
                 nil)))
    (let ((args (reduce #'nconc (mapcar #'expand-mask masks))))
      (with-gensyms (current-mode)
        `(let ((,current-mode (ccl:get-fpu-mode)))
           (unwind-protect
                (progn
                  (ccl:set-fpu-mode ,@args)
                  ,@body)
             (apply #'ccl:set-fpu-mode ,current-mode)))))))


#+ecl
(defmacro with-float-traps-masked-on-ecl ((&rest masks) &body body)
  (declare (ignore masks))
  `(let ((masked-bits (si::trap-fpe 'cl:last t)))
     (unwind-protect
          (progn
            (si::trap-fpe masked-bits nil)
            ,@body)
       (si::trap-fpe masked-bits t))))



(defmacro with-float-traps-masked ((&rest masks) &body body)
  (let* ((masks (or masks
                    '(:overflow
                      :underflow
                      :inexact
                      :invalid
                      :divide-by-zero)))
         (masking #+sbcl `(sb-int:with-float-traps-masked ,masks)
                  #+ccl `(with-float-traps-masked-on-ccl ,masks)
                  #+ecl `(with-float-traps-masked-on-ecl ,masks)
                  #-(or sbcl ccl ecl) '(progn)))
    `(,@masking
      ,@body)))


(defun dump-gcc-include-paths (lang)
  (handler-case
      (let* ((command (format nil "echo | gcc -x~A -E -v -" lang))
             (paths (with-output-to-string (out)
                      (uiop:run-program command
                                        :output out :error-output out)))
             (bounds (third (multiple-value-list (ppcre:scan +path-search-regex+ paths)))))
        (when bounds
          (ppcre:split "(\\r|\\n)+\\s*" (subseq paths (aref bounds 0) (aref bounds 1)))))
    (t ()
      (warn "Failed to obtain GCC search paths for language ~A" lang)
      nil)))


(defun %darwin-framework-path-p (path)
  (ends-with-subseq +stupid-darwin-framework-postfix+ path :test #'equal))


(defun dump-all-gcc-include-paths ()
  (remove-duplicates (remove-if #'%darwin-framework-path-p
                                (append (dump-gcc-include-paths "c")
                                        (dump-gcc-include-paths "c++")))
                     :test #'equal))


(defun dump-all-darwin-framework-paths ()
  (flet ((cut-darwin-postfix (path)
           (subseq path 0 (- (length path) (length +stupid-darwin-framework-postfix+)))))
    (remove-duplicates
     (mapcar #'cut-darwin-postfix
             (remove-if (complement #'%darwin-framework-path-p)
                        (append (dump-gcc-include-paths "c")
                                (dump-gcc-include-paths "c++"))))
     :test #'equal)))


(defun dump-gcc-version ()
  (handler-case
      (string-trim '(#\Tab #\Space #\Newline)
                   (with-output-to-string (out)
                     (uiop:run-program "gcc -dumpversion" :output out)))
    (t () "")))


(declaim (inline ptr))
(defun ptr (wrapper)
  (etypecase wrapper
    (cffi:foreign-pointer wrapper)
    (integer (cffi:make-pointer wrapper))
    (null (cffi:null-pointer))))


(declaim (inline null-pointer-p))
(defun null-pointer-p (value)
  (etypecase value
    (cffi:foreign-pointer (cffi-sys:null-pointer-p value))
    (null t)))


;;;
;;; Inclusion rules
;;;
(defun explicitly-included-p (name location)
  (or (included-p name *include-definitions*)
      (and (included-p location *include-sources*)
           (not (included-p name *exclude-definitions*)))))

(defun explicitly-excluded-p (name location)
  (or (included-p name *exclude-definitions*)
      (and (included-p location *exclude-sources*)
           (not (included-p name *include-definitions*)))))

(defun finally-included-p (name location)
  (and (explicitly-included-p name location)
       (not (explicitly-excluded-p name location))))

(defun form-finally-included-p (form)
  (let ((name (aval :name form))
        (location (aval :location form)))
    (and (explicitly-included-p name location)
         (not (explicitly-excluded-p name location)))))
