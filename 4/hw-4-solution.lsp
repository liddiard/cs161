; CS 161 Winter 2016: HW4 Solution

; [!] Include OUR HW3 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-3-solution using *our* path, which will likely
;     differ from yours (Note: HW3 also includes HW2, which includes HW1)
(load "hw-3-solution")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

(defun FIND-PATTERN (pred patterns)
    (let ((found-pat (loop for pattern in patterns do
                  (if (equal pred (first pattern)) (return pattern)))))
    (if found-pat found-pat NIL)))

(defun FIND-FILLER (pattern answer)
    (let ((pos (position (second (first pattern)) answer)))
        (if pos
            (nth (+ (position (second (first pattern)) answer) 1) answer)
            NIL)))

(defun BUILD-TRANSLATION (e-pats pattern answer translation)
    ;(print pattern)
    (cond
        ((not pattern) translation)
        ((equal (first (first pattern)) 'PHRASE) (BUILD-TRANSLATION e-pats (rest pattern) answer (append translation (rest (first pattern)))))
        ((equal (first (first pattern)) 'SL-NAME)
            (let ((filler (FIND-FILLER pattern answer)))
                (if filler
                    (BUILD-TRANSLATION e-pats (rest pattern) answer (append translation (FR-TO-ENG-HELPER e-pats filler '(QUESTION))))
                    (BUILD-TRANSLATION e-pats (rest pattern) answer translation))))
        ((equal (first (first pattern)) 'D-TREE)
            (let ((d-tree-result (EVAL-D-TREE (eval (second (first pattern))))))
                ;(print d-tree-result)
                (BUILD-TRANSLATION e-pats (rest pattern) answer (append translation (BUILD-TRANSLATION e-pats d-tree-result answer translation)))))
        ( T (BUILD-TRANSLATION e-pats (rest pattern) answer translation))))
;(trace BUILD-TRANSLATION)

; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; FUNCTION: FR-TO-ENG
; PURPOSE:  Translates a conceptual Q&A system answer into its English, human-
;           readable sentence equivalent.
; INPUT:    E-PATS: English patterns that take frame predicates and provide
;                   templates for how to translate the given frame
;           C-ANS: A conceptual frame answer to a question posed to our Q&A
;                  system. Assume this has been derived by a hypothetical
;                  inference system not shown.
;           Q-CON: A conceptual frame question created from a user-posed query.
; OUTPUT:   English sentence translation of C-ANS
(defun FR-TO-ENG (e-pats c-ans q-con)
    (setq *CUR-C-ANS c-ans)
    (FR-TO-ENG-HELPER e-pats c-ans q-con)
)

(defun FR-TO-ENG-HELPER (e-pats c-ans q-con)
    (setq *CUR-Q-CON q-con)
    (setq *CUR-FILLER c-ans)

    (let ((found-pattern (FIND-PATTERN (first c-ans) e-pats)))
    ;(print found-pattern)
    (if found-pattern (BUILD-TRANSLATION e-pats (rest found-pattern) c-ans '()) (list (first c-ans)))))

;(trace FR-TO-ENG-HELPER)

; -----------------------------------------------------------------------------


; FUNCTION: EVAL-D-TREE
; PURPOSE:  Takes the given decision tree, and uses the frames in the global
;           variables to return a replacement pattern, if any.
; INPUT:    D-TREE: a decision tree (NOT a symbol name for a decision tree,
;                   but the tree itself)
; OUTPUT:   Replacement pattern if a decision tree path is satisfied, else NIL.
(defun EVAL-D-TREE (d-tree)
    ;(print *CUR-FILLER)
    (cond
        ((not d-tree) NIL)
        ((eval (first d-tree)) (eval (first (last (first d-tree)))))
        ( T (EVAL-D-TREE (rest d-tree)))))

;(trace EVAL-D-TREE)
