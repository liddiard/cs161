
; Throughout the code, nil is used to represent the empty set of bindings.
; 'failure is used to indicate failure.  This is converted to the hw3
; spec in the "unify" function.

; This implementation contains some inefficiencies.  You're welcome to
; speed this up a bit if you feel so inclined.

; this version of hw3 represents variables using the (V x) convention

(defun atomicvariables () nil)

; variable? returns true iff the expression is a variable
; such as (V x)

(defun variable? (exp)
  (and (listp exp) (equal (first exp) 'V))
  )

; lookup-binding returns the value to which var is
; bound in theta.  If var is not bound in theta, nil
; is returned.

(defun lookup-binding (theta var)
  (cond
   ((null theta) nil)
   ((equal (first (first theta)) var) (second (first theta)))
   (t  (lookup-binding (cdr theta) var))
   )
)

; bound? returns true iff var is bound in theta

(defun bound? (theta var)
  (not (null (lookup-binding theta var)))
)

; unify-var adds var/x to theta iff doing so doesn't
; conflict with any information already in theta.
; If this is not possible, it returns 'failure.

(defun unify-var (var x theta)

  (cond

   ((bound? theta var)
    (_unify (lookup-binding theta var) x theta))

   ((and (variable? x) (bound? theta x))
    (_unify var (lookup-binding theta x) theta))

   ; ((occurs-in? var x theta) 'failure)

   (t (cons (list var x) theta))
   )
  )

; _unify returns a theta such that
; (_substitute exp1 theta) = exp2
; if this is not possible, it returns 'failure

(defun _unify (exp1 exp2 theta)
  (cond

   ((equal theta 'failure) 'failure)

   ((or (equal exp1 exp2) (not exp1)) theta)

   ((variable? exp1) (unify-var exp1 exp2 theta))

   ((variable? exp2) (unify-var exp2 exp1 theta))

   ((and (not (atom exp1)) (not (atom exp2)))
    (_unify (cdr exp1) (cdr exp2) (_unify (car exp1) (car exp2) theta)))

   (t 'failure)
   )

  )

(defun _unify-helper (exp1 exp2 theta)
    (_unify-helper-2 exp1 exp2 (_unify-helper exp1 exp2 theta))
)

(defun _unify-helper-2 (exp1 exp2 theta)
    (cond
        ((not exp2) theta)
        ((not (listp exp2)) (_unify-helper exp1 exp2 theta))
        ( T
            (let ((newtheta (_unify-helper exp1 (rest exp2) theta)))
                (cond
                    ((or (equal newtheta 'failure)) (_unify exp1 (rest exp2) theta))
                    ( T (_unify-helper-2 exp1 (rest exp2) (append theta newtheta)))
                )
            )
        )
    )
)

; substitute replaces rewrites exp.  Any variables in exp that are bound
; in theta are replaced with the expression that the variable is bound
; to in theta.

(defun _substitute (exp theta)

  (cond

   ((equal theta 'failure) 'failure)

   ((null theta) exp)

   ((and (variable? exp) (bound? theta exp))
    (_substitute (lookup-binding theta exp) theta))

   ((atom exp) exp)

   (t (cons (_substitute (car exp) theta)
	    (_substitute (cdr exp) theta)))

   )
  )

; subst-bindings recurses through theta to get rid of any variables
; that occur as "bindees" in theta.  In other words, no variable that
; is bound in theta should appear as in an expression to which a variable
; is bound

(defun subst-bindings (b theta)
  (cond
   ((null b) nil)
   (t  (cons (list (first (first b)) (_substitute (second (first b)) theta))
	     (subst-bindings (cdr b) theta)))
   )
  )

; unifier returns a set of bindings, theta, such that
; (_substitute exp1 theta) = (_substitute exp2 theta).
; The empty set of bindings is indicated with nil.
; Failure is indicated with 'failure.
; The same conventions are used for the input value 'theta'.

(defun unifier (exp1 exp2 theta)
  (let ((new_theta (_unify exp1 exp2 theta)))
    (cond
     ((equal new_theta 'failure) 'failure)
     (t (subst-bindings new_theta new_theta))
     )
    )
  )

; unify is just like unify, except that it uses
; '(nil) to indicate the empty set of bindings, and
; nil to indicate failure.  The same conventions are used
; for the input value 'theta'.

(defun unify (exp1 exp2 theta)

  (let ((theta_converted
	 (cond
	  ((equal theta '(nil)) nil)
	  ((equal theta nil) 'failure)
	  (t theta)
	  )
	 ))

    (let ((result (unifier exp1 exp2 theta_converted)))
      (cond
       ((null result) '(nil))
       ((equal result 'failure) nil)
       (t result)
       )
      )
    )
  )

; Test cases and examples

(lookup-binding '(((V w) a)) '(V w))
; A


(lookup-binding nil '(V w))
; NIL


(bound? '(((V w) a)) '(V w))
; T


; (bound? '(((V w) a)) '(V y))
; NIL


; (unify-var '(V x) 'A nil)
; (((V X) A))


; (unify-var '(V x) 'A '(((V x) B)))
; FAILURE


; (_substitute '(f (V x)) '(((V x) B)))
; (F B)


; (subst-bindings '(((V x) (V y)) ((V y) A)) '(((V x) (V y)) ((V y) A)))
; (((V X) A) ((V Y) A))


; (unifier '(LOVES (BRO SUSIE) (V X)) '(LOVES (V Y) (V Y)) nil)
; (((V X) (BRO SUSIE)) ((V Y) (BRO SUSIE)))


(UNIFY '(KNOWS  (V X)   (STEALS FRANK (HONDA (OWNEDBY (V X))) (V S1)) (V S2))
       '(KNOWS JOHN (STEALS (V Y)   (V Z)  S4)  (V S))
       '(((V W) BILL)))
;
;
;(((V S2) (V S)) ((V S1) S4) ((V Z) (HONDA (OWNEDBY JOHN)))
; ((V Y) FRANK) ((V X) JOHN) ((V W) BILL))
