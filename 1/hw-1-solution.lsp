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
(defun EXPAND (atom)
    'UNIMPLEMENTED
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
(defun AMEND-SF (slot filler frame)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: EQUAL-SF
; PURPOSE:  Boolean predicate which compares two frames and returns tr
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
(defun EQUAL-SF (frame1 frame2)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------
