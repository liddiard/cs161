; CS 161 Winter 2016: HW1
; Solution


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: front-slot
; PURPOSE:  Return the name of the first SLOT in FRAME
; INPUT:    frame
; OUTPUT:   atom: name of first SLOT
(defun front-slot (frame)
    (second frame)
)

; FUNCTION: front-filler
; PURPOSE:  Return the FILLER of the first SLOT in FRAME
; INPUT:    frame
; OUTPUT:   frame/gap: filler of first SLOT in FRAME
(defun front-filler (frame)
    (third frame)
)

; FUNCTION: pop-slot
; PURPOSE:  Return a copy of FRAME with its first slot-filler removed
; INPUT:    frame
; OUTPUT:   frame (with first slot-filler removed)
(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame))
)

; FUNCTION: f-pred
; PURPOSE:  retrieves the front-predicate of a frame, or the symbol name if
;           it's a gap
; INPUTS:   frame: a gap or a frame
; OUTPUT:   the predicate if it's a frame, or the symbol name if it's a gap
(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

; FUNCTION: f-length
; PURPOSE:  Safely checks the length of the input if it's a frame vs gap
; INPUTS:   frame: a gap or a frame
; OUTPUT:   length of frame if it's a frame; 1 if it's a gap
(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

; FUNCTION: rm-slot
; PURPOSE:  Return a copy of FRAME, but with a single slot-filler removed
; INPUT:    frame: frame to remove slot from
;           slot: slot name to be removed
; OUTPUT:   frame
(defun rm-slot (slot frame)
    (cond
        ; Base case: no slots left, so we're done
        ((<= (length frame) 1) frame)
        ; Base case: front slot matches, so just pop it
        ((equal (front-slot frame) slot) (pop-slot frame))
        ; Recursive case: front slot doesn't match, so keep looking
        (t (append (rm-slot slot (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)


; -----------------------------------------------------------------------------
; Main Functions
; -----------------------------------------------------------------------------

; FUNCTION: GET-SF
; PURPOSE:  Returns the filler of the given slot-name
; INPUTS:   slot: an atom designating the slot name
;           frame: a frame
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
(defun GET-SF (slot frame)
    (cond
        ; Base case: predicate with no slots (or empty frame)
        ((<= (length frame) 1) nil)
        ; If first slot matches, return its filler.
        ((equal slot (front-slot frame)) (front-filler frame))
        ; Else, first slot does not match, so test the rest of the slots
        (t (GET-SF slot (pop-slot frame)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: GET-NESTED-SF
; PURPOSE:  Returns the filler at the end of the given slot-path, or nil if
;           the path does not exist in the given frame
; INPUTS:   slots: a path of slots
;           concept: a frame
; OUTPUT:   The requested filler, which will be a frame or gap
(defun GET-NESTED-SF (slots concept)
    (cond
        ; Base case: got to the last slot, so stop recursing
        ((null slots) concept)
        ; Base case: null concept
        ((null concept) nil)
        ; If our concept is a gap to expand, we need to recurse on the frame
        ; Return nil if it's a gap but not bound
        ((atom concept) (if (boundp concept) (GET-NESTED-SF slots (eval concept)) nil))
        ; Recursive case: continue following path on sub-frame matched by filler
        ; of current path element
        (t (GET-NESTED-SF (rest slots) (GET-SF (first slots) concept)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: EXPAND-SLOTS
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching EXPAND
;           on each filler.
; INPUTS:   sf: list of (slot filler) pairs
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
(defun EXPAND-SLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch EXPAND on our first filler
        (t (append (append (list (first sf))               ; rebuild our first slot-filler pair
                           (list (EXPAND (second sf))))    ; dispatch EXPAND on the filler
                           (EXPAND-SLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

; FUNCTION: EXPAND
; PURPOSE:  Returns the frame represented by the given atom (if it is bound)
;           with all gaps replaced by their values
; INPUTS:   atom: a symbol (possibly) representing a frame
; OUTPUT:   That frame with all bound gaps replaced by their values
(defun EXPAND (atom)
    (cond
        ; Base case: safety for getting nil input - just return nil
        ((null atom) nil)
        ; Base case: we got a non-NIL atom, so evaluate it if bound
        ((atom atom) (if (boundp atom) (EXPAND (eval atom)) atom))
        ; Base case: empty, or single pred frame
        ((<= (length atom) 1) atom)
        ; Main case: dispatch EXPAND-SLOTS on our slot-filler list
        (t (cons (first atom) (EXPAND-SLOTS (rest atom))))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: AMEND-SF-EXEC
; PURPOSE:  (Workhorse Helper for AMEND-SF)
;           Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist) [Same inputs / outputs]
(defun AMEND-SF-EXEC (slot filler frame)
    (cond
        ; Base case: single predicate, so add the slot
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ; Base case: If first slot is target, replace the filler, keep rest slots
        ((equal slot (front-slot frame))
         (cons (first frame) (append (append (list slot) (list filler) (nthcdr 3 frame))))
        )
        ; Recursive case: First slot not target, so pop and recurse
        (t (append (AMEND-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

; FUNCTION: AMEND-SF
; PURPOSE:  Returns a copy of the input concept with the given slot-filler
;           pair added. If the slot already exists in the frame, its filler
;           will be replaced by the given input filler
; INPUTS:   slot: an atom representing the slot name
;           filler: a filler to place in the corresponding slot
;           frame: the frame being operated on
; OUTPUT:   Frame with added / replaced slot-filler pair
(defun AMEND-SF (slot filler frame)
    ; Check to see if we've been given a bound gap (just in case, you know?)
    (if (atom frame) 
        (if (boundp frame) (AMEND-SF-EXEC slot filler (eval frame)) frame)
        (AMEND-SF-EXEC slot filler frame)
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: EQUAL-SF-COMP
; PURPOSE:  Helper function that performs actual workhorse of frame comparison
;           on EXPANDed frame inputs
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: [EXPANDED] FRAME (first frame to compare)
;           frame2: [EXPANDED] FRAME (second frame to compare)
(defun EQUAL-SF-COMP (frame1 frame2)
    (cond
        ; Base case: empty frames match
        ((and (null frame1) (null frame2)) t)
        ; Base case: frames with different preds do not match
        ((not (equal (f-pred frame1) (f-pred frame2))) NIL)
        ; Base case: frames of different lengths do not match
        ((not (= (f-length frame1) (f-length frame2))) NIL)
        ; Base case: bare predicates (or matching symbols) match
        ((<= (f-length frame1) 1) t)
        ; Base case: variables match iff they have the same name
        ; [!] Not required for HW1, will not be tested
        ((equal (first frame1) 'V) (equal frame1 frame2))
        
        ; Recursive case: check frame1's front slot, then remove and recurse to
        ; check the rest of the slots.
        (t (let ((front (front-slot frame1))) 
            (and (EQUAL-SF-COMP (GET-SF front frame1) (GET-SF front frame2))
                 (EQUAL-SF-COMP (pop-slot frame1) (rm-slot front frame2)))
            )
        )
    )
)

; FUNCTION: EQUAL-SF
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
(defun EQUAL-SF (frame1 frame2)
    (let ((EX-frame1 (EXPAND frame1)) (EX-frame2 (EXPAND frame2)))
        (EQUAL-SF-COMP EX-frame1 EX-frame2)
    )
)

; -----------------------------------------------------------------------------


; Mark as included for examples file
; (this is not necessary for solution correctness, but avoids some warnings
; on the hw-1-examples.lsp)

(setq HW-1-SOLN-INCLUDED t)
