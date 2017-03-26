; CS 161 Winter 2016: HW2 Solution Skeleton

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
(load "../1/hw-1-solution-professor.lsp")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: NEWATM
; PURPOSE:  Produces a fresh, unused, unbound, unique symbol with the given
;           symName prefix and (ostensibly) random numerical suffix
; INPUT:    symName: symbol prefix for the new symbol
; OUTPUT:   new unbound symbol with symName prefix and numeric suffix
(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)

(defun PHRASE-INDEX (phrase list index)
    (cond
        ; base case: item not found
        ((not list) -1)
        ; base case: match at current element
        ((equal (first (first list)) phrase) index)
        ; recursive case: search the rest of the list
        ( T (PHRASE-INDEX phrase (rest list) (+ index 1)))
    )
)

(defun REMOVE-FROM-INDEX (list index)
    (cond
        ((= index 0) (rest list))
        ( T (append (subseq list 0 index) (nthcdr (+ index 1) list)))
    )
)

(defun MATCH-LENGTH (list phrase length)
    (cond
        ((or (not list) (not phrase)) length)
        ((equal (first list) (first phrase)) (MATCH-LENGTH (rest list) (rest phrase) (+ length 1)))
        ( T length )
    )
)

(defun LONGEST-MATCH (list phrase longest)
    (cond
        ((not list) longest)
        ((> (MATCH-LENGTH (first (first list)) phrase 0) (length (first longest))) (LONGEST-MATCH (rest list) phrase (first list)))
        ( T (LONGEST-MATCH (rest list) phrase longest))
    )
)

(defun FILL-GAPS (list)
    (cond
        ((not list) NIL)
        ( T (cons (list (first list) (NEWGAPS (second list))) (FILL-GAPS (rest list))))
    )
)

(defun NEWGAPS-HELPER (frame)
    (cond
        ((not frame) NIL)
        ( T (append (list (first frame) (NEWGAPS (second frame))) (NEWGAPS-HELPER (nthcdr 2 frame))))
    )
)

(defun FIND-SUBCLASS (entity taxonomy)
    ;(print (second (first taxonomy)))
    (cond
        ((not taxonomy) NIL)
        ((equal (second (first taxonomy)) entity) (first taxonomy))
        ( T (FIND-SUBCLASS entity (rest taxonomy)))
    )
)

(defun FIND-CON-HELPER (mycon dir class list)
    (let ((pos (position mycon list)))
        (cond
            ((not pos) NIL)
            ((equal dir 'BEF)
                (let ((previtem (nth (- pos 1) list)))
                    (cond
                        ; base case - mycon does not exist in list or we've run out of list
                        ; items to try in the desired direction
                        ((< (- pos 1) 0) NIL)
                        ((IS-SUBCLASS (first (eval previtem)) class) previtem)
                        ( T (FIND-CON-HELPER mycon dir class (append (subseq list 0 (- 2 pos)) (nthcdr (+ 1 pos) list))))
                    )
                )
            )
            ( T
                (let ((nextitem (nth (+ pos 1) list)))
                    (cond
                        ; base case - mycon does not exist in list or we've run out of list
                        ; items to try in the desired direction
                        ((> (+ pos 2) (length list)) NIL)
                        ((IS-SUBCLASS (first (eval nextitem)) class) nextitem)
                        ( T (FIND-CON-HELPER mycon dir class (append (subseq list 0 (+ pos 1)) (nthcdr (+ 2 pos) list))))
                    )
                )
            )
        )
    )
)

(defun EMBEDDED-IN-LIST (atom list)
    (cond
        ((not list) NIL)
        ((member atom (eval (first list))) T)
        ( T (EMBEDDED-IN-LIST atom (rest list)))
    )
)

(defun MAIN-ACT-HELPER (WM)
    (cond
        ((not WM) NIL)
        ((and (IS-SUBCLASS (first (eval (first WM))) 'ACT) (not (EMBEDDED-IN-LIST (first WM) WM))) (first WM))
        (T (MAIN-ACT-HELPER (rest WM)))
    )
)

; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------


; FUNCTION: ADD-TO-LM
; PURPOSE:  Adds the given (phrase frame demon) triplet to the global LEX-MEM,
;           making sure to not add duplicate phrases
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
; OUTPUT:   phrase-frame-demon triplet constructed
(defun ADD-TO-LM (phrase frame demons)
    (let ((index (PHRASE-INDEX phrase *LM 0)))
        (cond
            ((> index -1) (setq *LM (REMOVE-FROM-INDEX *LM index)))
        )
    )
    (setq *LM (cons (list phrase frame demons) *LM))
    (list phrase frame demons)
)

; -----------------------------------------------------------------------------


; FUNCTION: LOOKUP-PHRASE
; PURPOSE:  Looks for the longest phrase at the start of sent that matches a
;           corresponding phrase in the input lex-mem, and if found, returns:
;           ((phrase frame demons) rest-sent)
;           ...for corresponding (phrase frame demons) triplet found in lex-mem
;           ...and rest-sent being the rest of the sentence minus the found phrase
;
;           If NOT found, returns:
;           ((phrase nil nil) rest-sent)
; INPUT:    sent: a list of English words
;           lex-mem: a lexical memory with ((phrase frame demon)*) triplets
; OUTPUT:   (see above in purpose)
(defun LOOKUP-PHRASE (sent lex-mem)
    (let ((longestmatch (LONGEST-MATCH *LM sent NIL)))
        (cond
            ((not longestmatch) (list (list (list (first sent)) NIL NIL) (rest sent)))
            ( T (list longestmatch (nthcdr (MATCH-LENGTH (first longestmatch) sent 0) sent)))
        )
    )
)

; -----------------------------------------------------------------------------

; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names
(defun NEWGAPS (frame)
    (cond
        ((atom frame) (let* ((filler (NEWATM frame))) (set filler NIL) filler))
        ((< (length frame) 2) frame)
        ( T (cons (first frame) (NEWGAPS-HELPER (rest frame))))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship
(defun IS-SUBCLASS (entity class)
    (let ((subclass (FIND-SUBCLASS entity *TX)))
        (cond
            ; base case, not found
            ((not entity) NIL)
            ; base case, entity == class
            ((equal entity class) T)
            ; base case, they're equal, we're done
            ((equal (nth 2 subclass) class) T)
            ; recurse with found class
            ( T (IS-SUBCLASS (nth 2 subclass) class))
        )
    )
)

; -----------------------------------------------------------------------------

;(trace FIND-CON-HELPER)
; FUNCTION: FIND-CON
; PURPOSE:  Returns the CONatom found by searching the *WM for a CONatom with a
;           pred of the given class starting at mycon in direction dir within
;           the global *WM
; INPUT:    mycon: where to start searching in *WM
;           dir: either 'BEF or 'AFT indicating search direction
;           class: predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if not found
(defun FIND-CON (mycon dir class)
    (FIND-CON-HELPER mycon dir class *WM)
)

; -----------------------------------------------------------------------------


;(trace MAIN-ACT-HELPER)
; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom
(defun MAIN-ACT ()
    (MAIN-ACT-HELPER *WM)
)

; -----------------------------------------------------------------------------


; FUNCTION: GEN-DEMS
; PURPOSE:  Inserts the given CONATM at the front of every partial-demon-
;           instance in the given demons and then adds those completed
;           demon instantiations to the global *DM
; INPUT:    demons: a list of partial-demon-instantiations of the format:
;           ((demon-name arg2 arg3 ...)*)
;           conatm: a CONatom indicating which frame the demons work for
; OUTPUT:   current state of *DM after insertion
(defun GEN-DEMS (demons conatm)
    (cond
        ((not demons) *DM)
        ( T (setq *DM (cons (cons (first (first (last demons))) (cons conatm (rest (first (last demons))))) *DM)) (GEN-DEMS (subseq demons 0 (- (length demons) 1)) conatm))))

; -----------------------------------------------------------------------------
; Here There Be Demons
; -----------------------------------------------------------------------------

; FUNCTION: DEM-SRCH
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then sets the top-level gap in the
;           myslot slot in mycon to the found CONatom. Returns the found
;           CONatom if found, NIL otherwise.
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name of the gap in myslot to bind when found
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-SRCH (mycon myslot dir class)
    (let ((found-con (FIND-CON mycon dir class)))
        (cond
            ((not found-con) NIL)
            ( T (set (nth (+ (position myslot (eval mycon)) 1) (eval mycon)) found-con)))))

; -----------------------------------------------------------------------------


; FUNCTION: DEM-AMEND
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given myfiller.
;           Returns the found CONatom if found, NIL otherwise.
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           myfiller: the filler of myslot to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-AMEND (mycon myslot myfiller dir class)
    (let ((found-con (FIND-CON mycon dir class)))
        (cond
            ((not found-con) NIL)
            ( T (set found-con (append (eval found-con) (cons myslot (list myfiller)))) found-con))))

; -----------------------------------------------------------------------------


; FUNCTION: DEM-REF
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given mycon.
;           Returns the found CONatom if found, NIL otherwise.
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-REF (mycon myslot dir class)
    (let ((found-con (FIND-CON mycon dir class)))
        (cond
            ((not found-con) NIL)
            ( T (set found-con (append (eval found-con) (cons myslot (list mycon)))) found-con))))



; -----------------------------------------------------------------------------
; Workhorse Functions
; -----------------------------------------------------------------------------

; FUNCTION: DEM-EXEC
; PURPOSE:  Repeatedly calls each active demon instantiation within the global
;           *DM until all active demons return nil. Whenever a demon
;           returns something non-nil, we remove it from the global *DM
;           and then call each remaining demon again
; INPUT:    N/A
; OUTPUT:   Status of *DM after executing all active demons (a list of all
;           remaining, active demon instantiations)
(defun DEM-EXEC-HELPER (index restart)
    (cond
        ; base case – we have reached the end of DM and don't need to restart
        ((and (= index (length *DM)) (not restart)) *DM)
        ; base case – we have reached the end of DM and need to restart
        ((= index (length *DM)) (DEM-EXEC-HELPER 0 NIL))
        ; Demon returned a truthy value; pop it and continue on
        ((apply (first (nth index *DM)) (rest (nth index *DM))) (setq *DM (REMOVE-FROM-INDEX *DM index)) (DEM-EXEC-HELPER index T))
        ; Demon returned a falsy value
        ( T (DEM-EXEC-HELPER (+ index 1) NIL))
    )
)

(defun DEM-EXEC ()
    (DEM-EXEC-HELPER 0 NIL)
)

; -----------------------------------------------------------------------------


; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------
