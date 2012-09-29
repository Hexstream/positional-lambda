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

(defun %parse-body (body)
  '(values body min-length-indicator accept-rest-p)
  (multiple-value-bind (body min-length-indicator)
      (if (and (rest body) (%positional-ref-p (first body)))
          (values (rest body) (first body))
          (values body nil))
    (multiple-value-bind (body accept-rest-p)
        (if (eq (first body) :rest)
            (values (rest body) t)
            (values body nil))
      (values body
              :min-length-indicator min-length-indicator
              :accept-rest-p accept-rest-p))))

(defun %make-positional-gaps-filler (callback &key (start 0) min-end)
  '(values accumulate-next-position finish)
  (flet ((fill-gap (function marker exclusive-start exclusive-end)
           (dotimes (i (- exclusive-end exclusive-start 1))
             (funcall function marker (+ exclusive-start i 1)))))
    (let ((previous start))
      (values (lambda (current &key (object nil objectp))
                (unless (> current previous)
                  (error "Positions must be strictly increasing. (<= ~A ~A)."
                         current previous))
                (fill-gap callback :gap previous current)
                (setf previous current)
                (multiple-value-call callback
                  :there current (if objectp object (values)))
                (values))
              (lambda ()
                (when min-end
                  (fill-gap callback :pad previous (1+ min-end)))
                (values))))))

(defun %make-bindings-accumulator ()
  '(values add-binding finish-bindings)
  (let ((bindings nil))
    (values (lambda (new-binding)
              (destructuring-bind (var form) new-binding
                (declare (ignore form))
                (check-type var (and symbol (not null)))
                (prog1 var
                  (push new-binding bindings))))
            (lambda ()
              (nreverse bindings)))))

(defun %make-positional-vars-accumulator (var-function)
  '(values ensure-positional-var finish)
  (let ((blist nil))
    (values (lambda (position)
              (check-type position integer)
              (let ((existing (assoc position blist :test #'=)))
                (if existing
                    (second existing)
                    (let ((new-var (funcall var-function position)))
                      (prog1 new-var
                        (push (list position new-var)
                              blist))))))
            (lambda ()
              (sort blist #'< :key #'car)))))

(defun %make-lambda-list-accumulator ()
  '(values add-var finish)
  (let ((lambda-list nil)
        (ignored-vars nil)
        (rest-usage nil)
        (rest-var nil))
    (values (lambda (var &key (usage :used) (lambda-kind :required))
              (check-type var (and symbol (not null)))
              (check-type usage (member :used :ignored))
              (check-type lambda-kind (member :required &rest))
              (prog1 var
                (ecase lambda-kind
                  (:required
                   (push var lambda-list)
                   (when (eq usage :ignored)
                     (push var ignored-vars)))
                  (&rest (if rest-usage
                             (error "Multiple &rest vars prohibited.")
                             (setf (values rest-usage rest-var)
                                   (values usage var)))))))
            (lambda ()
              '(values lambda-list ignored-vars)
              (when rest-usage
                (push '&rest lambda-list)
                (push rest-var lambda-list))
              (when (eq rest-usage :ignored)
                (push rest-var ignored-vars))
              (values (nreverse lambda-list)
                      (nreverse ignored-vars))))))

(defun %compute-lambda-list (used-required-vars min-length rest-usage rest-var)
  '(values lambda-list ignored-vars)
  (multiple-value-bind (add-var finish-lambda-list)
      (%make-lambda-list-accumulator)
    (multiple-value-bind (acc finish-gaps-filling)
        (%make-positional-gaps-filler
         (lambda (marker position &optional var)
           (multiple-value-call add-var
             (ecase marker
               (:there var)
               ((:gap :pad)
                (values (gensym (format nil "~A~D-" '#:ignored position))
                        :usage :ignored)))))
         :min-end min-length)
      (dolist (position-var used-required-vars)
        (destructuring-bind (position var) position-var
          (funcall acc position :object var)))
      (funcall finish-gaps-filling))
    (when rest-usage
      (funcall add-var rest-var :usage rest-usage :lambda-kind '&rest))
    (funcall finish-lambda-list)))

(defun %make-walking-state-closures (&key min-length-indicator accept-rest-p
                                     &allow-other-keys)
  '(values process-thing finish &key add-let-binding)
  (let ((rest-var nil))
    (multiple-value-bind (add-let-binding
                          finish-let-bindings
                          ensure-positional-var
                          finish-positional-vars)
        (multiple-value-call #'values
          (%make-bindings-accumulator)
          (%make-positional-vars-accumulator
           (lambda (position)
             (gensym (format nil "~A~D-" '#:used position)))))
      (values (lambda (thing)
                (let ((position nil))
                  (cond
                    ((eq thing :rest)
                     (or rest-var
                         (setf rest-var (gensym (string '#:used-rest-)))))
                    ((setf position (%positional-ref-p thing))
                     (funcall ensure-positional-var position))
                    (t nil))))
              (lambda ()
                '(values &key let-bindings lambda-list ignored-vars)
                (multiple-value-bind (lambda-list ignored-vars)
                    (multiple-value-call #'%compute-lambda-list
                      (funcall finish-positional-vars)
                      (%positional-ref-p min-length-indicator)
                      (cond (rest-var (values :used rest-var))
                            (accept-rest-p
                             (values :ignored (gensym (string '#:ignored-rest-))))
                            (t (values nil nil))))
                  (values :let-bindings (funcall finish-let-bindings)
                          :lambda-list lambda-list
                          :ignored-vars ignored-vars)))
              :add-let-binding add-let-binding))))

(defun %walk (body process-possible-terminal)
  (labels ((recurse (expression)
             expression
             (multiple-value-bind (result definitely-terminal-p)
                 (funcall process-possible-terminal expression)
               (cond
                 ((or result definitely-terminal-p)
                  result)
                 ((atom expression)
                  expression)
                 (t (labels ((process (tail)
                               (cons (recurse (first tail))
                                     (let ((tail (rest tail)))
                                       (if (atom tail)
                                           (recurse tail)
                                           (process tail))))))
                      (process expression)))))))
    (mapcar #'recurse body)))

(defun %standard-local-special-forms (&key add-let-binding &allow-other-keys)
  (list
   (cons 'plambda #'identity)
   (cons 'quote #'identity)
   (cons :quote
         (lambda (whole)
           (destructuring-bind (opaque) (rest whole)
             opaque)))
   #+nil
   (error "Found ~S nested within ~S local special form."
          expression (first old-context))
   (cons :let
         (let ((precomputed-gensym (%gensym-generator '#:precomputed)))
           (lambda (whole)
             (destructuring-bind (form) (rest whole)
               (let ((var (funcall precomputed-gensym)))
                 (prog1 var
                   (funcall add-let-binding var form)))))))
   (cons :cache
         (let ((cache-gensym (%gensym-generator '#:cache))
               (cachedp-gensym (%gensym-generator '#:cachedp)))
           (lambda (whole)
             (destructuring-bind (form) (rest whole)
               (let ((cache-var (funcall cache-gensym))
                     (cachedp-var (funcall cachedp-gensym)))
                 (prog1 `(if ,cachedp-var
                             ,cache-var
                             (prog1 (setf ,cache-var ,form)
                               (setf ,cachedp-var t)))
                   (funcall add-let-binding cache-var 'nil)
                   (funcall add-let-binding cachedp-var 'nil)))))))))

(defun %processing-local-special-forms (local-special-forms-alist else)
  (lambda (expression)
    (funcall (or (and (consp expression)
                      (cdr (assoc (first expression)
                                  local-special-forms-alist)))
                 else)
             expression)))

(defun %assemble (body &key let-bindings lambda-list ignored-vars
                  &allow-other-keys)
  '(values form)
  (let ((main
         `(lambda ,lambda-list
            ,@(and ignored-vars (list `(declare (ignore ,@ignored-vars))))
            ,@body)))
    (if let-bindings
        `(let ,let-bindings
           ,main)
        main)))

(defmacro multiple-value-&bind (lambda-list values-form &body body)
  (if (member #\& lambda-list :key (lambda (symbol)
                                     (char (symbol-name symbol) 0)))
      `(multiple-value-call (lambda ,lambda-list ,@body) ,values-form)
      `(multiple-value-bind ,lambda-list ,values-form ,@body)))

(defun %standard-expansion-process (parse-body
                                    make-walking-state
                                    make-local-special-forms
                                    assemble)
  (lambda (body)
    (multiple-value-call assemble
      (multiple-value-&bind (body &rest indicators) (funcall parse-body body)
        (multiple-value-&bind (process-thing finish &rest walk-control-closures)
            (apply make-walking-state indicators)
          (multiple-value-call #'values
            (%walk body
                   (%processing-local-special-forms
                    (apply make-local-special-forms walk-control-closures)
                    process-thing))
            (funcall finish)))))))

(let ((expand (%standard-expansion-process '%parse-body
                                           '%make-walking-state-closures
                                           '%standard-local-special-forms
                                           '%assemble)))
  (defmacro plambda (&body body)
    (funcall expand body)))
