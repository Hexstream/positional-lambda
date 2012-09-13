(in-package #:positional-lambda)

(defun %positional-ref-p (object)
  (when (keywordp object)
    (let ((name (symbol-name object)))
      (when (and (plusp (length name))
                 (every #'digit-char-p name))
        (if (and (> (length name) 1)
                 (char= (char name 0) #\0))
            (error "~S is a malformed parameter reference (leading 0)."
                   object)
            (let ((parsed (parse-integer name)))
              (if (zerop parsed)
                  (error "~S positional parameter reference found ~
                          but such references are 1-based."
                         object)
                  parsed)))))))

(defun %gensym-generator (&optional default-base)
  (setf default-base (princ-to-string default-base))
  (let ((count 0))
    (lambda (&optional override-base)
      (let ((base (or override-base default-base)))
        (if base
            (gensym (format nil "~A~D-" base (incf count)))
            (error "override-base must be specified because no default-base."))))))

(defvar *%context* nil)

(defun %process-local-special-forms (body)
  (if (let ((foundp nil))
        (labels ((%search (expression)
                   (when (consp expression)
                     (let* ((old-context *%context*)
                            (*%context*
                             (if (member (first expression) '(:let :once))
                                 (if old-context
                                     (error "Found ~S nested within ~S ~
                                             local special form."
                                            expression (first old-context))
                                     (setf foundp expression))
                                 old-context)))
                       ;; Dotted-list-aware (some #'%search expression).
                       (do ((tail expression (rest tail)))
                           ((atom tail))
                         (%search (first tail)))))))
          (%search body))
        foundp)
      (let ((vars nil) (forms nil)
            (gen-cache-gensym (%gensym-generator '#:cache))
            (gen-cachedp-gensym (%gensym-generator '#:cachedp)))
        (labels ((recurse (expression)
                   (etypecase expression
                     ((cons (eql :let))
                      (destructuring-bind (form) (rest expression)
                        (let ((var (gensym (string '#:precomputed))))
                          (prog1 var
                            (push var vars)
                            (push form forms)))))
                     ((cons (eql :once))
                      (destructuring-bind (form) (rest expression)
                        (let ((cache-var (funcall gen-cache-gensym))
                              (cachedp-var (funcall gen-cachedp-gensym)))
                          (prog1 `(if ,cachedp-var
                                      ,cache-var
                                      (prog1 (setf ,cache-var ,form)
                                        (setf ,cachedp-var t)))
                            (push cache-var vars)
                            (push 'nil forms)
                            (push cachedp-var vars)
                            (push 'nil forms)))))
                     (cons
                      ;;Dotted-list-aware (mapcar #'recurse expression).
                      (labels ((process (current)
                                 (cons (recurse (first current))
                                       (let ((tail (rest current)))
                                         (if (atom tail)
                                             tail
                                             (process tail))))))
                        (process expression)))
                     (t expression))))
          (values (mapcar #'recurse body) (nreverse vars) (nreverse forms))))
      (values body nil nil)))

(defun %analyze-references (body &aux (alist nil) (restp nil))
  '(values references positions used-rest-p)
  (labels ((scan (expression &aux position)
             (cond
               ((listp expression)
                ;; Dotted-list-aware.
                (do ((tail expression (rest tail)))
                    ((atom tail) (when tail
                                   (scan tail)))
                  (scan (first tail))))
               ((eq expression :rest)
                (setf restp t))
               ((setf position (%positional-ref-p expression))
                (push (cons expression position) alist)))))
    (scan body)
    (let ((alist (sort (delete-duplicates alist :key #'car)
                       #'< :key #'cdr)))
      (values (mapcar #'car alist) (mapcar #'cdr alist) restp))))

(defun %compute-lambda-list/ignored-vars/replacements
    (references positions min-length-indicator rest-kind)
  '(values lambda-list ignored-vars replacements-alist)
  (let ((lambda-list nil)
        (ignored-vars nil)
        (replacements-alist nil)
        (min-length (%positional-ref-p min-length-indicator))
        (used-ignored-generator (%gensym-generator)))
    (flet ((add-vars (kind how-many
                           &optional (gensym-generator used-ignored-generator))
             (let ((base (princ-to-string (ecase kind
                                            (:used '#:used)
                                            (:ignored '#:ignored)))))
               (dotimes (i how-many (when (= how-many 1)
                                      (first lambda-list)))
                 (let ((var (funcall gensym-generator base)))
                   (push var lambda-list)
                   (when (eq kind :ignored)
                     (push var ignored-vars))))))
           (add-replacement (old new)
             (push (cons old new) replacements-alist)))
      (map-bind (mapc) ((previous-position (cons 0 positions))
                        (current-ref references)
                        (current-position positions))
        (add-vars :ignored (- current-position previous-position 1))
        (add-replacement current-ref (add-vars :used 1)))
      (when min-length
        (add-vars :ignored
                  (- min-length
                     (if positions (first (last positions)) 0))))
      (when rest-kind
        (push '&rest lambda-list)
        (let ((rest-var
               (add-vars rest-kind 1
                         (lambda (base)
                           (gensym (format nil "~A-~A" base '#:rest))))))
          (when (eq rest-kind :used)
            (add-replacement :rest rest-var)))))
    (values (nreverse lambda-list)
            (nreverse ignored-vars)
            (nreverse replacements-alist))))

(defun %parse-body (body)
  '(values body min-length-indicator accept-rest-p)
  (multiple-value-bind (body min-length-indicator)
      (if (and (rest body) (%positional-ref-p (first body)))
          (values (rest body) (first body))
          (values body nil))
    (multiple-value-bind (body accept-rest-p) (if (eq (first body) :rest)
                                                  (values (rest body) t)
                                                  (values body nil))
      (values body min-length-indicator accept-rest-p))))

(defmacro plambda (&body body)
  (multiple-value-bind (body min-length-indicator accept-rest-p)
      (%parse-body body)
    (multiple-value-bind (references positions used-rest-p)
        (%analyze-references body)
      (multiple-value-bind (lambda-list ignored-vars replacements-alist)
          (%compute-lambda-list/ignored-vars/replacements
           references
           positions
           min-length-indicator
           (cond (used-rest-p :used)
                 (accept-rest-p :ignored)
                 (t nil)))
        (multiple-value-bind (body let-vars let-forms)
            (%process-local-special-forms body)
          (let ((main
                 `(lambda ,lambda-list
                    ,@(and ignored-vars (list `(declare (ignore ,@ignored-vars))))
                    ,@(sublis replacements-alist body))))
            (if let-vars
                `(let ,(mapcar #'list let-vars let-forms)
                   ,main)
                main)))))))
