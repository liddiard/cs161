; CS 161 Winter 2016: HW3

; [!] Include OUR HW2 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-2-solution using *our* path, which will likely
;     differ from yours (Note: HW2 also includes HW1)
(load "../2/hw-2-solution.lsp")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

(defun IS-VAR (var)
    (cond
    ((listp var) (equal (first var) 'V))
    ( T NIL )))

(defun FRAMES-EQUAL (fr1 fr2)
    (cond
        ((and (IS-COMPOUND fr1) (IS-COMPOUND fr2)) (EQUAL-SF fr1 fr2))
        ( T (equal fr1 fr2))))

(defun IS-COMPOUND (item)
    (and (listp item) (not (equal (first item) 'V)) (atom (first item))))

(defun get-binding (x theta)
    (second (member x theta :test #'check-binding))
)

(defun check-binding (x y)
    (and (listp y) (equal x (second y)))
)

(defun UNIFY-VAR (var x bds)
    (let* ((var-binding (get-binding (second var) bds))
           (x-binding (get-binding (second x) bds)))
        (cond
            (var-binding (FR-UNIFY var-binding x bds))
            ((and (IS-VAR x) x-binding) (FR-UNIFY var x-binding bds))
            ((not (null x)) (append bds (list (list (list 'V (second var)) x))))
            (t 'failure)
        )
    )
)

(defun FR-UNILIST (frame f-list theta)
   (cond
       ; Ran out of frames; failure
       ((not f-list) nil)
       ; Else, check whether we can bind with the first frame
       (t (let* ((binding (UNIFY-FR frame (first f-list) theta)))
           (if (null binding)
               ; We couldn't, but keep trying...
               (FR-UNILIST frame (rest f-list) theta)
               ; Else, we did find a binding; return it!
               (list binding (first f-list))
           ))
       )
   )
)

(defun GET-SF-HELPER (slot frame)
    (cond
        ((not (listp frame)) frame)
        ( T (GET-SF slot frame))
    )
)

(defun FR-UNIFRAME (sf-list frm2 theta)
    (cond
        ; Failed somewhere, so terminate
        ((null theta) nil)
        ; Ran out of slots to check from sf-list, so we're good!
        ((or (null sf-list) (not (listp sf-list))) theta)
        ; Try to unify first slot in slot-filler list
        (t (let* ((slotname (first sf-list))
                  (firstfiller (second sf-list))
                  ; Actually attempt the unification
                  (binding (UNIFY-FR firstfiller (GET-SF-HELPER slotname frm2) theta)))
               ; Now recurse on the rest of the sf list
               (FR-UNIFRAME (nthcdr 1 sf-list) (DELETE-SLOT slotname frm2) binding))
        )
    )
)
;(trace FR-UNIFRAME)

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
        ((equal bds 'failure) NIL)
        ((or (FRAMES-EQUAL lfr1 lfr2) (equal lfr1 'NIL) (equal lfr2 'NIL)) bds)
        ((IS-VAR lfr1) (UNIFY-VAR lfr1 lfr2 bds))
        ((IS-VAR lfr2) (UNIFY-VAR lfr2 lfr1 bds))
        ((and (IS-COMPOUND lfr1) (IS-COMPOUND lfr2)) (FR-UNIFRAME (rest lfr1) lfr2 (UNIFY-FR (first lfr1) (first lfr2) bds)))
        ((and
            (and (listp lfr1))
            (and (listp lfr2)))
            bds
            (UNIFY-FR (rest lfr1) (rest lfr2) (UNIFY-FR (first lfr1) (first lfr2) bds))
        )
        ( T NIL )))
(trace UNIFY-FR)


; -----------------------------------------------------------------------------


; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made
(defun SUBST-FR (frm bds)
    'UNIMPLEMENTED
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
    'UNIMPLEMENTED
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
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Problem 4 Rule Definitions
; -----------------------------------------------------------------------------

(setq RULE-1 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
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
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))

      RULE-4 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))

      RULE-5 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))

      RULE-6 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))

      RULE-7 '((PREMISES
                 ; TODO
               )
               (CONCLU
                 ; TODO
               ))
)

; -----------------------------------------------------------------------------
