;;; CS 161 Winter 2016: Example Test Framework

; [!] Load your HW3 solution here!
(load "./hw-3-solution.lsp")
(load "unify-ucla")

; [!] IMPORTANT ===============================================================
; It is assumed that, by this point, you have included either the HW1 solution,
; or your own HW2 solution that includes our HW1. This is important for the
; following tests because they will employ the EQUAL-SF function, and some
; other helpers that are ommitted in these sample tests
; =============================================================================

; -----------------------------------------------------------------------------
; Test Helper functions
; -----------------------------------------------------------------------------

; Test function; makes sure result is equal to expected, and if not, print expected value
(defun test-case (actual expected case-name)
    (cond
        ((equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function; makes sure actual is frame-equivalent to the expected
(defun test-case-fr (actual expected case-name)
    (cond
        ((EQUAL-SF actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function, with equality condition a set-comparison between two lists with
; each matching element being compared using equivalence (#'EQUAL-SF)
(defun test-case-fr-set (actual expected case-name)
    (cond
        ((and (subsetp actual expected :test #'EQUAL-SF) (subsetp expected actual :test #'EQUAL-SF)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function, with equality condition a set-comparison between two lists with
; each matching element being compared using equivalence (#'equal)
(defun test-case-set (actual expected case-name)
    (cond
        ((and (subsetp actual expected :test #'equal) (subsetp expected actual :test #'equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Takes in two binding list items of the format:
; ((V varname) (filler))
; ...and determines if they are equivalent
(defun binding-list-items-equal (actual expected)
    (if (or (atom actual) (atom expected)) (equal actual expected)
        (and (equal (first actual) (first expected))
             (EQUAL-SF (second actual) (second expected)))
    )
)

; Test function; Takes in two binding lists of the format:
; (((V varname1) (filler1)) ... ((V varnameN) (fillerN)))
; ...and determines if they are set-theoretically equivalent
(defun test-case-binding-list (actual expected case-name)
    (cond
        ((and (subsetp expected actual :test #'binding-list-items-equal) (subsetp actual expected :test #'binding-list-items-equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected binding list equivalent to ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)


; -----------------------------------------------------------------------------
; Test Frames, Variables, & Globals
; -----------------------------------------------------------------------------

(setq FR1 '(ENABLES ANTE (HAVE AGENT (V XX01)
                               OBJECT (MONEY))
                    CONSEQ (TRAVEL AGENT (V XX01)
                                   TO (V QQ01)))

      FR2 '(ENABLES CONSEQ (TRAVEL TO (FRANCE)
                                   AGENT (V YY01))
                    ANTE (HAVE AGENT (JOE)
                               OBJECT (V ZZ01)))

      FR3 '(ENABLES ANTE (HAVE AGENT (V XX01)
                               OBJECT (MONEY))
            CONSEQ (TRAVEL AGENT (V XX01)
                           TO (FRANCE)))

      FR4 '(ENABLES ANTE (HAVE AGENT (JOE)
                               OBJECT (V ZZ01))
                    CONSEQ (TRAVEL AGENT (V YY01)
                                   TO (V YY01)))

      FR6 '(KNOWS AGENT (V HX1)
                  OBJECT (STATE TYPE (PHYSICAL)
                                AGENT (V HX1)
                                OBJECT (CANCER TYPE (V TY01)))
                  SITU (V SS01))

      FRM-A '(INFORM RECIP (HUMAN F-NAME (GEORGE))
                     OBJECT (STATE OBJECT (CANCER TYPE (TERMINAL)))
                     AGENT (V HUMX08))

      FRM-B '(INFORM SITU (S1)
                     RECIP (V HUMX09)
                     AGENT (HUMAN ROLE (ONCOLOGIST))
                     OBJECT (STATE OBJECT (V OBJX07)
                                   AGENT (HUMAN F-NAME (GEORGE))))



      LST1 '(
              (INFORM SITU (S1)
                      RECIP (V HUMX01)
                      AGENT (HUMAN ROLE (ONCOLOGIST))
                      OBJECT (STATE AGENT (V HUMX01)
                                    OBJECT (V OBJX01)))

              (AFTER ANTE (S1)
                     CONSEQ (V SXB1))
            )
      LST2 '(
              (INFORM AGENT (V HUMX02)
                      RECIP (HUMAN F-NAME (GEORGE)
                                   GENDER (MALE))
                      OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                                                 GENDER (MALE))
                                    OBJECT (CANCER TYPE (TERMINAL)))
                      SITU (V SXA1))

              (AFTER CONSEQ (S2)
                     ANTE (V SXA1))
            )



      RULE-51 '((PREMISES
                  (OWNS AGENT (V a1)
                        OBJECT (V o1))

                  (ISA OBJECT (V o1)
                       TYPE (ICE-CREAM))
                )
                (CONCLU
                  (HAPPY AGENT (V a1))
                ))

      RULE-52 '((PREMISES
                  (HAPPY AGENT (V a1))
                )
                (CONCLU
                  (AWESOME AGENT (V a1))
                ))



      EP0 '(AFTER ANTE (S0)
                  CONSEQ (S1))

      EP1 '(TEACH AGENT (HUMAN F-NAME (GEORGE)
                               GENDER (MALE))
                  RECIP (HUMAN TYPE (STUDENTS))
                  OBJECT (CHEM)
                  LOC (HIGHSCHOOL)
                  SITU (S1))

      EP2 '(INFORM AGENT (HUMAN ROLE (ONCOLOGIST))
                   RECIP (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                                              GENDER (MALE))
                                 OBJECT (CANCER TYPE (TERMINAL)))
                   SITU (S1))

      EP3 '(STATE TYPE (PHYSICAL)
                  AGENT (HUMAN F-NAME (WINNIE)
                               GENDER (FEMALE))
                  VALUE (PREGNANT)
                  SITU (S2))

      EP4 '(MARRIED AGENT (HUMAN F-NAME (GEORGE)
                                 GENDER (MALE))
                    OBJECT (HUMAN F-NAME (WINNIE)
                                  GENDER (FEMALE))
                    SITU (S1))

      EP5 '(AFTER ANTE (S1)
                  CONSEQ (S2))

      EP6 '(AFTER ANTE (S2)
                  CONSEQ (S3))

      EP7 '(AFTER ANTE (S3)
                  CONSEQ (S4))

      EP8 '(INGEST AGENT (HUMAN F-NAME (RICK)
                                GENDER (MALE))
                   OBJECT (COCAINE)
                   SITU (S4))

      EP9 '(AFTER ANTE (S4)
                  CONSEQ (S5))

      EP10 '(STATE AGENT (HUMAN F-NAME (RICK)
                                GENDER (MALE))
                   OBJECT (LESIONS AREA (NOSE))
                   SITU (S5))

      EP11 '(AFTER ANTE (S2)
                   CONSEQ (S4))

      EPMEM (LIST EP0 EP1 EP2 EP3 EP4 EP5 EP6 EP7 EP8 EP9 EP10 EP11)


      INF1 '(STATE AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   TYPE (EMOTIONAL)
                   VALUE (HAPPY)
                   SITU (S1))

      INF2 '(KNOWS AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                                              GENDER (MALE))
                                 OBJECT (CANCER TYPE (TERMINAL)))
                   SITU (S2))

      INF3 '(STATE TYPE (EMOTIONAL)
                   AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   VALUE (SAD)
                   SITU (S2))

      INF4 '(SEX-ACT AGENT (HUMAN F-NAME (GEORGE)
                                  GENDER (MALE))
                     OBJECT (HUMAN F-NAME (WINNIE)
                                   GENDER (FEMALE))
                     SITU (S1))

      INF5 '(MAKES AGENT (HUMAN F-NAME (GEORGE)
                                GENDER (MALE))
                   OBJECT (COCAINE)
                   SITU (S2))

      INF6 '(CAUSE ANTE (INGEST AGENT (HUMAN F-NAME (RICK)
                                             GENDER (MALE))
                                OBJECT (COCAINE)
                                SITU (S4))
                   CONSEQ (STATE AGENT (HUMAN F-NAME (RICK)
                                              GENDER (MALE))
                                 OBJECT (LESIONS AREA (NOSE))
                                 SITU (S5)))

      INF7 '(ACQUIRED AGENT (HUMAN F-NAME (RICK)
                                   GENDER (MALE))
                      OBJECT (COCAINE)
                      FROM (HUMAN F-NAME (GEORGE)
                                  GENDER (MALE))
                      SITU (S4))
)



; -----------------------------------------------------------------------------
; Unit Test Flags
; -----------------------------------------------------------------------------

; Unit test activations: only run the unit tests marked with t
(setq
    UNIFY-FR-tests t
    SUBST-FR-tests t
    MP-INFER-tests t
    FRW-CHAIN-tests t
)


; -----------------------------------------------------------------------------
; Unit Tests
; -----------------------------------------------------------------------------

; Test cases: (UNIFY-FR)
(cond (UNIFY-FR-tests
(format t "Testing UNIFY-FR...~%")

(format t "===========================~%")


    (test-case (UNIFY '(
                (HUMAN F-NAME (V X))
                (DRUG NAME (V Y)) )
                '(
                (DRUG NAME (COCAINE)) (HUMAN L-NAME LN2) (HUMAN L-NAME LN1
                F-NAME (GEORGE))))
                '(T ((V X) (GEORGE)) ((V Y) (COCAINE))) "UNIFY-FR_ex_3")

    (test-case (UNIFIER '(
                (HUMAN F-NAME (V X))
                (DRUG NAME (V Y)) )
                '(
                (DRUG NAME (COCAINE)) (HUMAN L-NAME LN2) (HUMAN L-NAME LN1
                F-NAME (GEORGE))))
                '(T ((V X) (GEORGE)) ((V Y) (COCAINE))) "UNIFIER_ex_3")


(UNIFY '(HUMAN F-NAME (V X))
       '(HUMAN L-NAME (DYER) F-NAME (MICHAEL))
       '())

(UNIFY '(HUMAN F-NAME (V X))
       '(HUMAN F-NAME (MICHAEL) L-NAME (DYER))
       '())

(UNIFY '(KNOWS (V X) (STEALS FRANK (HONDA (OWNEDBY (V X))) (V S1)) (V S2))
       '(KNOWS JOHN (STEALS (V Y) (V Z) S4) (V S))
       '(((V W) BILL)))

(UNIFY '((DRUG NAME (V Y)) (HUMAN F-NAME (V X)))
'((DRUG NAME (COCAINE)) (HUMAN F-NAME (GEORGE) L-NAME LN1) (HUMAN L-NAME LN2)))

(UNIFY-FR '((HUMAN F-NAME (V X)) (DRUG NAME (V Y)) )
'((DRUG NAME (COCAINE)) (HUMAN L-NAME LN2) (HUMAN L-NAME LN1 F-NAME (GEORGE))))

))

; -----------------------------------------------------------------------------


; Test cases: (SUBST-FR)
(cond (SUBST-FR-tests
(format t "Testing SUBST-FR...~%")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (MP-INFER)
(cond (MP-INFER-tests
(format t "Testing MP-INFER...~%")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (FRW-CHAIN)
(cond (FRW-CHAIN-tests
(format t "Testing FRW-CHAIN...~%")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------
