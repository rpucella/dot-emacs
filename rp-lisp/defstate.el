;;; defstate.el --- React-style state variables for special modes  -*- lexical-binding: t -*-

(defmacro defstate (name vars render)
  "Macro to define a new local variable with a given NAME holding the state.
The state is a property list with the properties ('state properties') listed in VARS.
Record the RENDER function so that it can be invoked when the state is updated."
  (let ((proplist (mapcan (lambda (x) (list x nil)) vars)))
    `(progn
       (make-local-variable (quote ,name))
       (make-local-variable '**defstate--name**)
       (make-local-variable '**defstate--vars**)
       (make-local-variable '**defstate--render**)
       (setq ,name (quote,proplist))
       (setq **defstate--name** (quote ,name))
       (setq **defstate--vars** (quote ,vars))
       (setq **defstate--render** ,render))))

(defun getstate (prop)
  "Get the value of a state property."
  (if (not (memq prop **defstate--vars**))
      (error (format "unknown state property %s in getstate" prop))
    (plist-get (symbol-value **defstate--name**) prop)))

(defun setstate (&rest props)
  "Update some state properties and invoke the render function."
  (let* ((rest props)
         (prop nil)
         (val nil))
    (when (> (mod (length props) 2) 0)
      (error "arguments to setstate must have even length"))
    (while (not (null rest))
      (setq prop (car rest))
      (setq val (cadr rest))
      (setq rest (cddr rest))
      (if (not (memq prop **defstate--vars**))
          (error (format "unknown state property %s in setstate" prop))
        (plist-put (symbol-value **defstate--name**) prop val)))
    (funcall **defstate--render**)))

(defun refstate ()
  "Refresh by calling the state render function."
  (funcall **defstate--render**))

(provide 'defstate)
