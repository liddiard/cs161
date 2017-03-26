;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File unify.lisp: Unification functions

(require "patmatch")

(defun IS-VAR (var)
    (cond
    ((listp var) (equal (first var) 'V))
    ( T NIL )))

(defun get-binding (x theta)
    (second (first (member x theta :test #'check-binding)))
)

(defun check-binding (x y)
    (and (listp y) (equal x (second (first y))))
)

(defun unify (x y &optional (bindings 'no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings 'fail) 'fail)
        ((eql x y) bindings)
        ((IS-VAR x) (unify-variable x y bindings))
        ((IS-VAR y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t 'fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        (t (extend-bindings var x bindings))))


(defun make-binding (var val) (cons var val))

;;; ==============================

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy +no-bindings+
        (if (eq bindings 'no-bindings)
            nil
            bindings)))

;;; ==============================

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings 'fail) 'fail)
        ((eq bindings 'no-bindings) x)
        ((and (IS-VAR x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

;;; ==============================

(defun unifier (x y)
 "Return something that unifies with both x and y (or 'fail)."
 (subst-bindings (unify x y) x))
