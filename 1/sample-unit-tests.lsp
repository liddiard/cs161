;;; CS 161 Winter 2016: Example Test Framework


; [!] Load your solution here!
(load "hw-1-solution.lsp")


; -----------------------------------------------------------------------------
; Test Helper functions
; -----------------------------------------------------------------------------

; [!] TODO: Function to check if frames are equal -- this is a placeholder for
; your problem 5 for testing purposes!
(defun fr-equal (fr1 fr2)
    (cond
        ; TODO: Your problem 5!
    )
)

; Test function; make sure result is equal to expected, and if not, print expected value
(defun test-case (actual expected case-name)
    (cond
        ((equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; [!] TODO: Test function, with equality replaced with frame-equality
; Requires completion of fr-equal above to be functional
(defun test-case-fr (actual expected case-name)
    (cond
        ((not (and (listp actual) (listp expected))) t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
        ((fr-equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)


; -----------------------------------------------------------------------------
; Test Frames, Variables, & Globals
; -----------------------------------------------------------------------------

; A few examples from the spec (labeled as they were)

(setq CON-SENT1 '(STATE TYPE (EMOTION SENTIM (POS)
                                      SCALE (>NORM)) 
                        AGENT (HUMAN F-NAME (CHARLOTTE)
                                     L-NAME (NEWTON)
                                     GENDER (FEMALE)
                                     AGE (RANGE FROM (13)
                                                TO (19)
                                                UNIT (YEAR))))
      
      CON-SENT2 
      '(CO-HABITATE AGENT (HUMAN F-NAME (CHARLES)
                                 L-NAME (OAKLEY)
                                 GENDER (MALE)
                                 HISTORY (UNKNOWN)
                                 FAMREL (UNCLE-OF
                                         OBJECT (HUMAN
                                                 F-NAME (CHARLOTTE)
                                                 L-NAME (NEWTON)
                                                 GENDER (FEMALE))))
                    OBJECT (HUMAN F-NAME (EMMA)
                                  L-NAME ( )
                                  GENDER (FEMALE))
                    FAMREL (MOTHER-OF OBJECT (HUMAN F-NAME (CHARLOTTE)
                                                    L-NAME (NEWTON)
                                                    GENDER (FEMALE)))
                    TIME (FUTURE))
      
      CON-SENT3 '(THROW AGENT (HUMAN F-NAME (CHARLES)
                                     L-NAME ( )
                                     GENDER (MALE))
                        OBJECT (PRINTED-MATTER TYPE (NEWSPAPER)
                                               MATERIAL (PAPER)
                                               INFO (STORY)
                                               REF (INDEF))
                        INTO (CONTAINER SHAPE (CYLINDRICAL)
                                        FOR (TRASH)
                                        REF (DEF)
                                        MANNER (SECRETLY))
                        EXP-VIOL (SEE AGENT (HUMAN F-NAME (CHARLOTTE)
                                                   L-NAME ( )
                                                   GENDER (FEMALE))
                                      OBJECT (HUMAN F-NAME (CHARLES)
                                                    L-NAME ( )
                                                    GENDER (MALE)))
                        LOC (HOME OWNER (HUMAN F-NAME (CHARLOTTE)
                                               L-NAME ( )
                                               GENDER (FEMALE))))
      
      CON-SENT4 '(READ AGENT (HUMAN F-NAME (CHARLOTTE)
                                    L-NAME ( )
                                    GENDER (FEMALE))
                       OBJECT (PRINTED-MATTER TYPE (NEWSPAPER)
                                              MATERIAL (PAPER)
                                              INFO (STORY)
                                              REF (DEF)))
      
      CON-SENT5 '(SEEK AGENT (HUMAN NUMBER ( >1)
                                    ROLE (POLICE))
                       OBJECT (HUMAN GENDER (MALE)
                                     NICNAME (M-WIDOW-M)
                                     ROLE (MURDERER)))
      
      CON-SENT6 '(KILL AGENT (HUMAN GENDER (MALE)
                                    NICNAME (M-WIDOW-M)
                                    ROLE (MURDERER))
                       OBJECT (HUMAN F-NAME (SAMANTHA)
                                     L-NAME (VODNER)
                                     GENDER (FEMALE)
                                     ROLE (WIDOW))
                       MOTIVE (JEWELRY))
      
      CON-SENT7 '(GIVE AGENT (HUMAN F-NAME (CHARLES)
                                    L-NAME ( )
                                    GENDER (MALE))
                       TO (HUMAN F-NAME (CHARLOTTE)
                                 L-NAME ( )
                                 GENDER (FEMALE))
                       OBJECT (RING CONTAINS (EMERALD)
                                    COST (>NORM)))
      
      CON-SENT8 '(SEE AGENT (HUMAN F-NAME (CHARLOTTE)
                                   L-NAME ( )
                                   GENDER (FEMALE))
                      OBJECT (PRINTED-MATTER
                              TYPE (INITIALS F-INIT (S)
                                             L-INIT (V))
                              ON (RING REF (DEF))))
      
      CON-SENT9 '(BELIEVE AGENT (HUMAN F-NAME (CHARLOTTE)
                                       L-NAME ( )
                                       GENDER (FEMALE))
                          OBJECT (KILL AGENT (HUMAN F-NAME (CHARLES)
                                                    L-NAME ( )
                                                    GENDER (MALE))
                                       OBJECT (HUMAN F-NAME (SAMANTHA)
                                                     L-NAME ( )
                                                     GENDER (FEMALE))))
      
      SENT3-GAPPED '(THROW AGENT AG001
                           OBJECT OBJ001
                           INTO (CONTAINER SHAPE (CYLINDRICAL)
                                           FOR (TRASH)
                                           REF (DEF)
                                           MANNER (SECRETLY))
                           EXP-VIOL EXPV001
                           LOC (HOME OWNER (HUMAN F-NAME (CHARLOTTE)
                                                  L-NAME ( )
                                                  GENDER (FEMALE))))
      AG001 '(HUMAN F-NAME (CHARLES)
                    L-NAME ( )
                    GENDER (MALE))
      
      OBJ001 '(PRINTED-MATTER TYPE (NEWSPAPER)
                              MATERIAL (PAPER)
                              INFO (STORY)
                              REF (INDEF))
      
      EXPV001 'CON004
      
      CON004 '(SEE AGENT AG002
                   OBJECT OBJ002)
      
      AG002 'CON005
      
      CON005 '(HUMAN F-NAME (CHARLOTTE)
                     L-NAME ( )
                     GENDER (FEMALE))
      
      OBJ002 '(HUMAN F-NAME (CHARLES)
                     L-NAME ( )
                     GENDER (MALE))
)


; -----------------------------------------------------------------------------
; Unit Test Flags
; -----------------------------------------------------------------------------

; Unit test activations: only run the unit tests marked with t
; (useful for filtering the tests you're not interested in)
(setq
    GET-SF-tests t
    GET-NESTED-SF-tests t
    EXPAND-tests t
    AMEND-SF-tests t
    EQUAL-SF-tests t
)


; -----------------------------------------------------------------------------
; Unit Tests
; -----------------------------------------------------------------------------

(format t "===========================~%")
; Function 1 (GET-SF)
(cond (GET-SF-tests
(format t "Testing GET-SF...~%")
    
    (test-case (GET-SF 'F-NAME '(HUMAN F-NAME (BOB)))
               '(BOB) "GET-SF_ex_1")
    (test-case (GET-SF 'LAMESLOT '(HUMAN F-NAME (BOB)))
               nil "GET-SF_ex_2")
    (test-case (GET-SF 'AGENT CON-SENT1)
               '(HUMAN F-NAME (CHARLOTTE)
               L-NAME (NEWTON) GENDER (FEMALE)
               AGE (RANGE FROM (13)
               TO (19)
               UNIT (YEAR))) "GET-SF_ex_3")
    
(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Function 2 (GET-NESTED-SF)
(cond (GET-NESTED-SF-tests
(format t "Testing GET-NESTED-SF...~%")
    
    (test-case (GET-NESTED-SF '() '(HUMAN F-NAME (CHARLOTTE))) 
               '(HUMAN F-NAME (CHARLOTTE)) "GET-NESTED-SF_ex_1")

    (test-case (GET-NESTED-SF '(MOTHER F-NAME) '(HUMAN F-NAME (CHARLOTTE))) 
               nil "GET-NESTED-SF_ex_2")

    (test-case (GET-NESTED-SF '(FAMREL OBJECT) CON-SENT2)
               '(HUMAN F-NAME (CHARLOTTE)
               L-NAME (NEWTON) GENDER (FEMALE)) "GET-NESTED-SF_ex_4")
    
(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Function 3 (EXPAND)
(cond (EXPAND-tests
(format t "Testing EXPAND...~%")
    
    ; TODO
    
(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Function 4 (AMEND-SF)
(cond (AMEND-SF-tests
(format t "Testing AMEND-SF...~%")
    
    ; TODO
    
(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Function 5 (EQUAL-SF)
(cond (EQUAL-SF-tests
(format t "Testing EQUAL-SF...~%")
    
    ; TODO
    
(format t "===========================~%")
))


