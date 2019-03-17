(cl:in-package :claw)

(defclass library-definition () ())

(defgeneric load-library-definition (library))
(defgeneric save-library-definition (library))

(defgeneric list-adapted-functions (library))
(defgeneric library-header-file (library))
(defgeneric library-loader-name (library))

(defgeneric generate-bindings (library))

(defgeneric library-function-name (function))
(defgeneric generate-adapted-function-definition (function &optional output))
(defgeneric generate-adapted-function-variable (function name &optional output))
