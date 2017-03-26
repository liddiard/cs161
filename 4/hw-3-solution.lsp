; CS 161 Winter 2016: HW3
; Solution

; [!] Include OUR HW2 solution here!
(load "../2/hw-2-solution")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; Predicate to check if input is a variable
(defun is-variable (x)
    (if (listp x) (equal (first x) 'V) nil)
)

; Predicate to check if input is a frame
; Frame is a list starting with an atom that is not V
(defun is-frame (x)
    (and (listp x) (atom (first x)) (not (equal (first x) 'V)))
)

; Predicate that determines whether or not the input x is
; a list of frames, i.e., a list starting with a frame
(defun is-frame-list (x)
    (and (listp x) (is-frame (first x)))
)

; Simple helper function that returns the list composed of all elements of x
; except for the nth, indexing starting at 0
(defun remove-nth (n x)
    (cond
        ((or (= n 0) (null x)) (rest x))
        (t (cons (car x) (remove-nth (- n 1) (rest x))))
    )
)

; Helper that takes in two items x and y and returns equivalence based on:
;   - #'equal when x and y are not frames
;   - #'EQUAL-SF when x and y are frames
; Used in UNIFY-FR so that we can safely compare two input types that aren't
; necessarily homogenous
(defun robust-equal (x y)
    (if (and (is-frame x) (is-frame y))
        ; If frames, do frame comparison
        (EQUAL-SF x y)
        ; Else, ordinary comparison
        (equal x y)
    )
)

; Test function for the get-binding function that compares variable
; names if they are still in the (V VAR) list format
(defun check-binding (x y)
    (and (listp y) (equal x (second (first y))))
)

; Helper to check a binding list to see if it has a binding for that var,
; and if so, return it
(defun get-binding (x bds)
    (second (first (member x bds :test #'check-binding)))
)

; Small helper to extract the premises of a given rule
(defun get-premises (rule)
    (rest (first rule))
)

; Helper function; only returns fact in a list format if it is something
; new to the current epmem
(defun is-new-discovery (fact epmem)
    (cond
        ; We did unify... but we didn't learn anything new
        ((member fact epmem :test #'EQUAL-SF) nil)
        ; We did unify, AND we learned something new
        ((first fact) (list fact))
        ; We simply didn't unify :(
        (t nil)
    )
)


; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; FUNCTION: UNIFY-FR
; PURPOSE:  Unifies the given variables, frames, or lists of frames by the
;           criteria listed in the spec
; INPUT:    LFR1: a variable, frame, or list of frames
;           LFR2: a variable, frame, or list of frames
;           BDS: [Optional; default: '(T)] A binding list being built during
;                execution
; OUTPUT:   Binding list
(defun UNIFY-FR (lfr1 lfr2 &optional (bds '(T)))
    (cond
        ; Failure detection; we'll return nil for bds if the unification fails at any point
        ((null bds) nil)
        ; Check if lfr1 and lfr2 are already equal; if frames, use EQUAL-SF, else ordinary
        ((robust-equal lfr1 lfr2) bds)
        ; Check if either arg is a variable
        ((is-variable lfr1) (UNIVAR lfr1 lfr2 bds))
        ((is-variable lfr2) (UNIVAR lfr2 lfr1 bds))
        ; If both frames, then check predicates and unify slot-fillers
        ((and (is-frame lfr1) (is-frame lfr2)) (UNIFRAME (rest lfr1) lfr2 (UNIFY-FR (first lfr1) (first lfr2) bds)))
        ; If lists, see if first element of lfr1 can unify with anything in lfr2
        ((and (is-frame-list lfr1) (is-frame-list lfr2))
            (if (null lfr1)
                ; Base case: we checked everything in the list lfr1 already
                bds
                (loop for r on lfr2 do
                    ; Result is the list (new-bds, lfr2-frame), where lfr2-frame is the frame that
                    ; was used for the binding to lfr1; we'll then enforce the uniqueness constraint
                    ; of our unification by setting up "next" to unite over lfr2 with lfr2-frame removed
                    (let* ((result (UNILIST (first lfr1) r bds))
                           (next (UNIFY-FR (rest lfr1) (remove (second result) lfr2) (first result))))
                        ; If the remainder of unification succeeds, then return that... otherwise,
                        ; we'll try another unification of the (first lfr1) with everything but the
                        ; first element of lfr2
                        (if next (return next))
                    )
                )
            )
        )
        ; Fall-through: failure
        (t nil)
    )
)


; FUNCTION: UNIVAR
; PURPOSE:  Helper function for UNIFY-FR. See pseudo-code from the textbook
;           algorithm UNIFY-VAR, from the same page as UNIFY referenced in
;           UNIFY-FR. You should not implement or call OCCUR-CHECK?
; INPUTS:   var: a variable (list formatted as (V var))
;           x: frame, variable, or frame list, as in UNIFY-FR
;           bds: a binding list, same format as in UNIFY-FR
; OUTPUT:   A binding-list (bds)
(defun UNIVAR (var x bds)
    (let* ((var-binding (get-binding (second var) bds))
           (x-binding (get-binding (second x) bds)))
        (cond
            (var-binding (UNIFY-FR var-binding x bds))
            ((and (is-variable x) x-binding) (UNIFY-FR var x-binding bds))
            ((not (null x)) (append bds (list (list (list 'V (second var)) x))))
            (t nil)
        )
    )
)


; FUNCTION: UNIFRAME
; PURPOSE:  Helper function for UNIFY-FR; attempt to unify each slot-filler in the two frames, although
;           it is OK if frm2 has extra frames
; INPUT:    sf-list: a list of the first frame's slot/fillers
;           frm2: the frame on which to unify frm1
;           bds: binding list
; OUTPUT:   A binding list (bds)
(defun UNIFRAME (sf-list frm2 bds)
    (cond
        ; Failed somewhere, so terminate
        ((null bds) nil)
        ; Ran out of slots to check from sf-list, so we're good!
        ((null sf-list) bds)
        ; Try to unify first slot in slot-filler list
        (t (let* ((slotname (first sf-list))
                  (firstfiller (second sf-list))
                  ; Actually attempt the unification
                  (binding (UNIFY-FR firstfiller (GET-SF slotname frm2) bds)))
               ; Now recurse on the rest of the sf list
               (UNIFRAME (nthcdr 2 sf-list) (rm-slot slotname frm2) binding))
        )
    )
)

; FUNCTION: UNILIST
; PURPOSE: Helper function for UNIFY-FR; attempt to unify a single frame with a list of frames f-list
;          (any element of f-list will do).
; INPUTS:  frame: a single frame on which to unify something from the f-list
;          f-list: a list of frames
;          bds: the current binding list
; OUTPUT:  (new-bds f-list-frame) where new-bds is the updated binding list, and f-list-frame
;          is the frame that was used for the binding from the f-list
(defun UNILIST (frame f-list bds)
    (cond
        ; Ran out of frames; failure
        ((null f-list) nil)
        ; Else, check whether we can bind with the first frame
        (t (let* ((binding (UNIFY-FR frame (first f-list) bds)))
            (if (null binding)
                ; We couldn't, but keep trying...
                (UNILIST frame (rest f-list) bds)
                ; Else, we did find a binding; return it!
                (list binding (first f-list))
            ))
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made
(defun SUBST-FR (frm bds)
    (if (or (null bds) (equal bds '(T)))
        frm
        (SUBST-EXEC frm (rest bds)))
)


; Helper for SUBST-FR, goes through the sf-list replacing variables
; with bindings from bds
(defun SUBST-SLOTS (sf bds)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch SUBST-FR on our first filler
        (t (append (append (list (first sf))                     ; rebuild our first slot-filler pair
                           (list (SUBST-EXEC (second sf) bds)))  ; dispatch SUBST-FR on the filler
                 (SUBST-SLOTS (nthcdr 2 sf) bds)))               ; recurse on rest of sf
    )
)

; Helper function that simply begins the execution of SUBST-FR assuming that the
; binding list was not empty
(defun SUBST-EXEC (frm bds)
    (cond
        ; Base case: got a variable, so replace it with its binding, OR just the frame if no binding
        ((is-variable frm) (or (get-binding (second frm) bds) frm))
        ; Base case: empty, or single pred frame
        ((<= (length frm) 1) frm)
        ; Main case: dispatch SUBST-SLOTS on our slot-filler list
        (t (cons (first frm) (SUBST-SLOTS (rest frm) bds)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: MP-INFER
; PURPOSE:  Attempts to unify the given rule's premises on the given facts,
;           and if successful, returns the conclusion of the rule with SUBST-FR
;           called on it using the successful binding list
; INPUT:    RULE: an if-then rule
;           O-FRAMES: a list of facts / concepts
; OUTPUT:   conclusion if successfully unified; nil otherwise
(defun MP-INFER (rule o-frames)
    (let* ((premises (get-premises rule))
           (conclusion (second (second rule)))
           (preds (loop for p in premises collect (first p)))
           ; Get a list of relevant o-frames, i.e. ones containing a predicate in premises
           (relevant-o-frames (loop for f in o-frames append
                               (if (member (first f) preds) (list f))))
           (theta (UNIFY-FR premises relevant-o-frames))
           (new (SUBST-FR conclusion theta)))

      (cond
          ; Our binding failed, so return nil
          ((null theta) nil)
          ; Our binding succeeded, so return the discovery
          (t new)
      )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: FRW-CHAIN
; PURPOSE:  Performs simplified forward chaining given the list of rules and
;           facts, returning any new conclusions that are derived
; INPUT:    RULES: a list of if-then rules
;           EPMEM: a list of facts storing an episodic memory
;           NEW-EPMEM: [Optional: default: nil] the list of newly discovered
;                      facts grown through code execution and returned at the
;                      end
; OUTPUT:   NEW-EPMEM
(defun FRW-CHAIN (rules epmem &optional (new-epmem nil))
    (let* ((new (loop for rule in rules append
                    ; Store the result of MP-INFERing on the current rule
                    ; and epmem
                    ; ...if that succeeded, then to collect our new
                    ; discovery, we need to wrap it in a list, but if it
                    ; failed, just return nil
                    (let* ((result (MP-INFER rule epmem)))
                        (if (is-new-discovery result epmem) (list result) nil)
                    ))))

        (if (null new)
            ; if nothing new, return our discoveries, if any
            new-epmem
            ; else, recurse with newly discovered facts added to the STORY-CNS
            (FRW-CHAIN rules (append new epmem) (append new new-epmem))
        )
    )
)

; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Problem 4 Rule Definitions
; -----------------------------------------------------------------------------

(setq RULE-1 '((PREMISES
                 (TEACH AGENT (V x)
                        SITU (V sa))
               )
               (CONCLU
                 (STATE AGENT (V x)
                        TYPE (EMOTIONAL)
                        VALUE (HAPPY)
                        SITU (V sa))
               ))

      ; RULE-2 was done for you! Yay!
      RULE-2 '((PREMISES
                 (INFORM AGENT (V x)
                         RECIP (V y)
                         OBJECT (V z)
                         SITU (V sa))

                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (KNOWS AGENT (V y)
                        OBJECT (V z)
                        SITU (V sb))
               ))

      RULE-3 '((PREMISES
                 (KNOWS AGENT (V x)
                        OBJECT (STATE AGENT (V x)
                                      OBJECT (CANCER))
                        SITU (V sa))
               )
               (CONCLU
                 (STATE TYPE (EMOTIONAL)
                        AGENT (V x)
                        VALUE (SAD)
                        SITU (V sa))
               ))

      RULE-4 '((PREMISES
                 (MARRIED AGENT (V x)
                          OBJECT (V y)
                          SITU (V sa))
                 (STATE TYPE (PHYSICAL)
                        AGENT (V y)
                        VALUE (PREGNANT)
                        SITU (V sb))
               )
               (CONCLU
                  (SEX-ACT AGENT (V x)
                           OBJECT (V y)
                           SITU (V sa))
               ))

      RULE-5 '((PREMISES
                 (TEACH AGENT (V x)
                        OBJECT (CHEM)
                        SITU (V sa))
                 (STATE AGENT (V x)
                        VALUE (SAD)
                        SITU (V sb))
                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (MAKES AGENT (V x)
                        OBJECT (COCAINE)
                        SITU (V sb))
               ))

      RULE-6 '((PREMISES
                 (INGEST AGENT (V x)
                         OBJECT (COCAINE)
                         SITU (V sa))
                 (STATE AGENT (V x)
                        OBJECT (LESIONS AREA (NOSE))
                        SITU (V sb))
                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (CAUSE ANTE (INGEST AGENT (V x)
                                     OBJECT (COCAINE)
                                     SITU (V sa))
                        CONSEQ (STATE AGENT (V x)
                                      OBJECT (LESIONS AREA (NOSE))
                                      SITU (V sb)))
               ))

      RULE-7 '((PREMISES
                 (MAKES AGENT (V x)
                        OBJECT (COCAINE)
                        SITU (V sa))
                 (INGEST AGENT (V y)
                         OBJECT (COCAINE)
                         SITU (V sb))
                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (ACQUIRED AGENT (V y)
                           OBJECT (COCAINE)
                           FROM (V x)
                           SITU (V sb))
               ))
)

; -----------------------------------------------------------------------------
