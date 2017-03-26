; CS 161 Winter 2016: HW1
; 404-174-240 


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; [!] TODO: Place any helpers that you'd like here!


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
        (
            (not frame) nil
        )
        (
            (equal (first frame) slot)
            (second frame)
        )
        ( T (GET-SF slot (rest frame)))
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
    (let ((new_sf (GET-SF (first slots) concept)))
        (cond
            ((not slots) concept)
            ( T (GET-NESTED-SF (rest slots) new_sf))
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: EXPAND
; PURPOSE:  Returns the frame represented by the given atom (if it is bound)
;           with all gaps replaced by their values
; INPUTS:   atom: a symbol (possibly) representing a frame
; OUTPUT:   That frame with all bound gaps replaced by their values
(defun EXPAND-ATOM (_atom)
    (cond 
        ((or (not (atom _atom)) (not (boundp _atom))) _atom)
        ( T (eval _atom))
    )
)

(defun EXPAND (atom)
    (EXPAND-ATOM atom)
    ; (let ((atom-expanded (EXPAND-ATOM atom)))
    ;     (cond
    ;         ((listp atom-expanded) )
    ;     )
    ; )
)

(defun FILL-FRAME (frame)
    (cond
        ;( T sf)
        ; no frames remaining
        ((null frame) nil)
        ((not (listp (first frame))) frame)
        ( T (cons (list (first (first frame))
        (EXPAND (second (first frame)))) 
        (FILL-FRAME (rest frame))))
    )
)

(defun EXPAND (_atom)
    (cond
        ; atom exists, evaluate if bound
        ((not (listp _atom)) (if (boundp _atom) (EXPAND (eval _atom)) _atom))
        ; frame of length 0 or 1
        ((< (length _atom) 2) _atom)
        ; call FILL-FRAME for the remainder
        ( T (cons (first _atom) (FILL-FRAME (rest _atom))))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: AMEND-SF
; PURPOSE:  Returns a copy of the input concept with the given slot-filler
;           pair added. If the slot already exists in the frame, its filler
;           will be replaced by the given input filler
; INPUTS:   slot: an atom representing the slot name
;           filler: a filler to place in the corresponding slot
;           frame: the frame being operated on
; OUTPUT:   Frame with added / replaced slot-filler pair
(defun FIND-IN-LIST (slot frame index)
    (cond
        ((not frame) -1) ; not found
        ((equal (first frame) slot) index)
        ( T (FIND-IN-LIST slot (rest frame) (+ index 1)))
    )
)

(defun REMOVE-FROM-LIST (frame index)
    (cond
        ((>= index 0) (append (subseq frame 0 index) (nthcdr (+ index 2) frame)))
        ( T frame)
    )
)

(defun AMEND-SF (slot filler frame)
    (let* ((frame (EXPAND-ATOM frame)) (index (FIND-IN-LIST slot frame 0)) (new_list (REMOVE-FROM-LIST frame index)))
        (append new_list (cons slot (list filler)))
    )
)

; -----------------------------------------------------------------------------

(defun GET-LEN (frame)
    (cond
        ((listp frame) (length frame))
        ( T 1 )
    )
)

(defun FIRST-EQUAL (frame1 frame2)
    (cond 
        ; frames are both lists - compare first item from each
        ((and (listp frame1) (listp frame2)) (equal (GET-FIRST frame1) (GET-FIRST frame2)))
        ; frames are both non-lists - compare
        ((and (not (listp frame1)) (not (listp frame2))) (equal frame1 frame2))
        ; we assume frames differ in type because they're not both atoms or lists and these are the only types we're dealing with
        ( T NIL )

    )
)

(defun DELETE-SLOT (frame slot)
    (cond
        ; no slots remain, except possibly the predicate which can't be removed; do nothing
        ((< (GET-LEN frame) 2) frame)
        ; first slot matches; remove it
        ((equal (GET-FIRST (second frame)) slot) (cons (GET-FIRST frame) (nthcdr 2 frame)))
        ; slot doesn't match, shorten the frame and continue searching 
        ( T (append (DELETE-SLOT (cons (GET-FIRST frame) (nthcdr 2 frame)) slot) (list (second frame))))
    )
)
(defun GET-FILLER (frame slot)
    (cond
        ; predicate without any slots, nothing to return
        ((< (GET-LEN frame) 2) NIL)
        ; first slot matches; return its contents 
        ((equal slot (GET-FIRST (second frame))) (second (second frame)))
        ; first slot doesn't match; shorten list and continue searching
        ( T (GET-FILLER slot (cons (GET-FIRST frame) (nthcdr 2 frame))))
    )
)

(defun GET-FIRST (frame)
    (cond
        ((listp frame) (first frame))
        ( T frame )
    )
)
; FUNCTION: EQUAL-SF
; PURPOSE:  Boolean predicate which compares two frames and returns tr
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
(defun EQUAL-SF (frame1 frame2)
    (cond
        ; both frames are empty; they're equal
        ((and (not frame1) (not frame2)) T)
        ; lengths differ; frames cannot be equal
        ((not (= (GET-LEN frame1) (GET-LEN frame2))) NIL)
        ; predicates differ; frames cannot be equal
        ((not (FIRST-EQUAL frame1 frame2)) NIL) 
        ; frames are equal, single-length predicates
        ((< (GET-LEN frame1) 2) T)
        ; return equality with frame1's first list item && its remaining list
        ( T (and (EQUAL-SF (GET-FILLER (GET-FIRST (second frame1)) frame1) (GET-FILLER (GET-FIRST (second frame1)) frame2))
                 (EQUAL-SF (cons (GET-FIRST frame1) (nthcdr 2 frame1)) (DELETE-SLOT frame2 (GET-FIRST (second frame1)))))) 
    ) 
)

; -----------------------------------------------------------------------------
