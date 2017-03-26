;;; CS 161 Winter 2016: Example Test Framework

; [!] Load your HW2 solution here!
(load "hw-2-solution.lsp")

; [!] IMPORTANT ===============================================================
; It is assumed that, by this point, you have included either the HW1 solution,
; or your own HW2 solution that includes our HW1. This is important for the
; following tests because they will employ the EQUAL-SF function, and some
; other helpers that are ommitted in these sample tests
; =============================================================================

; -----------------------------------------------------------------------------
; Test Helper functions
; -----------------------------------------------------------------------------

; Testing function used to wipe our globals clean
; between tests
(defun EX-CLEAR-GLOBALS ()
    ; Clear out all of our example bindings
    (every #'makunbound *WM)

    (setq *LM NIL)
    (setq *WM NIL)
    (setq *DM NIL)
    (setq *TX NIL)

    ; Good to go for a new set of tests!
)

; Testing function used to set up globals with their spec-values
(defun SETUP-GLOBALS ()
    (setq *WM NIL)
    (setq *DM NIL)
    (setq *LM '(
      ((HIGH SCHOOL) (INSTITUTION TYPE (HIGHSCHOOL)) ((DEM-REF LOC BEF ACT)))
      ((IS) (BEING AGENT AGENT
                   OBJECT OBJECT)
            ((DEM-SRCH AGENT BEF OBJECT)
             (DEM-SRCH OBJECT AFT OBJECT)))
      ((A) NIL ((DEM-AMEND REF (INDEF) AFT CONCEPT)))
      ((AT) (LOC TYPE TYPE) ((DEM-SRCH TYPE AFT INSTITUTION)))
      ((CHEMISTRY) (KNOWLEDGE TYPE (CHEM)) NIL)
      ((STUDENTS) (HUMAN TYPE (STUDENTS)) NIL)
      ((TEACHES) (TEACH AGENT AGENT
                        RECIP RECIP
                        OBJECT OBJECT)
                 ((DEM-SRCH AGENT BEF HUMAN)
                  (DEM-SRCH RECIP AFT HUMAN)
                  (DEM-SRCH OBJECT AFT ABSTRACT)))
      ((GEORGE) (HUMAN F-NAME (GEORGE)
                       GENDER (MALE)) NIL)
      ((DRUG) (SUBSTANCE TYPE (DRUG)
                         NAME NM1) NIL)
      ((DRUG DEALER) (HUMAN OCCUPATION (DEALER)
                            F-NAME FN1) NIL)
      ((DRUG DEALER LAB) (LOC TYPE (LABORATORY)
                              CONTAINS (DRUGS)
                              CONOTATION (ILLICIT)) NIL)
      ((DEALS) (ACT AGENT AGENT
                    RECIP RECIP
                    OBJECT DG)
               ((DEM-SRCH AGENT BEF HUMAN)
                (DEM-SRCH RECIP AFT HUMAN)
                (DEM-SRCH OBJECT AFT DRUG)))
      ((COCAINE) (DRUG NAME (COCAINE)
                    TYPE (STIMULANT)) nil)
    ))
    (setq *TX '(
      (MEMB HUMAN ANIMATE)
      (MEMB ANIMATE OBJECT)
      (MEMB HOME LOC)
      (MEMB THEATER LOC)
      (MEMB FIDO CANINE)
      (MEMB CANINE ANIMATE)
      (MEMB INGEST PHYS-ACT)
      (MEMB COMMUN MENTAL-ACT)
      (MEMB TEACH MENTAL-ACT)
      (MEMB PHYS-ACT ACT)
      (MEMB THINK ACT)
      (MEMB BEING ACT)
      (MEMB MENTAL-ACT ACT)
      (MEMB INSTITUTION SOCIAL-ENT)
      (MEMB KNOWLEDGE ABSTRACT)
      (MEMB SOCIAL-ENT CONCEPT)
      (MEMB COCAINE DRUG)
      (MEMB WEED DRUG)
      (MEMB MJ WEED)
      (MEMB WEED MJ)
      (MEMB DRUG PHYS-OBJ)
    ))
)

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
; each matching element being compared using equivalence (#'equal)
(defun test-case-set (actual expected case-name)
    (cond
        ((and (subsetp actual expected :test #'equal) (subsetp expected actual :test #'equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Get "boolean value," i.e. NIL vs NON-NIL
; Best effort to test for NEWGAPS symbols that may have different orderings of
; names
(defun b-val (v)
    (if (null v) nil 'NON-NIL)
)

; Test function; make sure result boolean equal (non-nil = non-nil; nil = nil)
(defun test-case-bool (actual expected case-name)
    (cond
        ((equal (b-val actual) (b-val expected)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Helper function; lex-equal determines if two lexical entries of format:
; (phrase frame demons)
; are equivalent between actual and expected
(defun lex-equal (actual expected)
    (and (equal (first actual) (first expected))
         (EQUAL-SF (second actual) (second expected))
         (and (subsetp (third expected) (third actual) :test #'equal) (subsetp (third actual) (third expected) :test #'equal)))
)

; Test function; see if the phrase-frame-demon triplets are the same
(defun test-case-lex-entry (actual expected case-name)
    (cond
        ((lex-equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function; see if the lexical entries of the format:
; ((phrase frame demons)*)
; are equivalent between actual and expected
(defun test-case-lex-mem (actual expected case-name)
    (cond
        ((and (subsetp expected actual :test #'lex-equal) (subsetp actual expected :test #'lex-equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function; see if the ((phrase frame demon) (rest-sent)) pair for LOOKUP-PHRASE
; are equivalent between actual and expected
(defun test-case-lookup-phrase (actual expected case-name)
    (cond
        ((and (lex-equal (first actual) (first expected)) (equal (second actual) (second expected))) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)


; -----------------------------------------------------------------------------
; Test Frames, Variables, & Globals
; -----------------------------------------------------------------------------

; Global definitions
(setq *LM NIL)
(setq *WM NIL)
(setq *DM NIL)
(setq *TX NIL)
(EX-CLEAR-GLOBALS)

; -----------------------------------------------------------------------------
; Unit Test Flags
; -----------------------------------------------------------------------------

; Unit test activations: only run the unit tests marked with t
(setq
    ADD-TO-LM-tests t
    LOOKUP-PHRASE-tests t
    NEWGAPS-tests t
    IS-SUBCLASS-tests t
    FIND-CON-tests t
    MAIN-ACT-tests t
    GEN-DEMS-tests t
    DEM-SRCH-tests t
    DEM-AMEND-tests t
    DEM-REF-tests t
    DEM-EXEC-tests t
    PARSE-SENT-tests t
)


; -----------------------------------------------------------------------------
; Unit Tests
; -----------------------------------------------------------------------------

; Test cases: (ADD-TO-LM)
(cond (ADD-TO-LM-tests
(format t "Testing ADD-TO-LM...~%")

    (test-case-lex-entry (ADD-TO-LM '(GEORGE)
                              '(HUMAN F-NAME (GEORGE)
                                      GENDER (MALE))
                              '((D1 A1) (D2 A21 A22)))
                         '((GEORGE) (HUMAN F-NAME (GEORGE)
                                           GENDER (MALE)) ((D1 A1) (D2 A21 A22))) "ADD-TO-LM_ex_1")
    (test-case-lex-mem *LM
                       '(
                          ((GEORGE) (HUMAN F-NAME (GEORGE)
                                           GENDER (MALE)) ((D1 A1) (D2 A21 A22)))
                        ) "ADD-TO-LM_ex_2")

    (test-case-lex-entry (ADD-TO-LM '(HIGH SCHOOL) '(INSTITUTION TYPE (HIGHSCHOOL)) nil)
                         '((HIGH SCHOOL) (INSTITUTION TYPE (HIGHSCHOOL)) nil) "ADD-TO-LM_ex_3")

    (test-case-lex-mem *LM
                        '(((HIGH SCHOOL) (INSTITUTION TYPE (HIGHSCHOOL)) nil) ((GEORGE) (HUMAN F-NAME (GEORGE)
                         GENDER (MALE)) ((D1 A1) (D2 A21 A22)))
                         ) "ADD-TO-LM_ex_4")

    (test-case-lex-entry (ADD-TO-LM '(HIGH SCHOOL) '(INSTITUTION TYPE (HIGHSCHOOL)
                          NAME (RIDGEMONT)) '((D1 A2)))
                         '((HIGH SCHOOL) (INSTITUTION TYPE (HIGHSCHOOL)
                          NAME (RIDGEMONT)) ((D1 A2))) "ADD-TO-LM_ex_5")

    (test-case-lex-mem *LM
                        '(
                            ((HIGH SCHOOL) (INSTITUTION TYPE (HIGHSCHOOL)
                            NAME (RIDGEMONT)) ((D1 A2)))
                            ((GEORGE) (HUMAN F-NAME (GEORGE)
                            GENDER (MALE)) ((D1 A1) (D2 A21 A22)))
                         )
                        "ADD-TO-LM_ex_6")


(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (LOOKUP-PHRASE)
(cond (LOOKUP-PHRASE-tests
(format t "Testing LOOKUP-PHRASE...~%")
    (SETUP-GLOBALS)

    (setq *LM '( ((GEORGE) (HUMAN F-NAME (GEORGE)
        GENDER (MALE)) NIL) ((DORON) (HUMAN F-NAME (DORON)
        GENDER (MALE)) NIL) ((*POSS) NIL NIL)
        ((IS) NIL NIL)
        ((A) (REF TYPE (INDEF)) NIL)
        ((DRUG) NIL NIL)
        ((DRUG ENFORCEMENT AGENT) (HUMAN ROLE (NARC)) NIL) ((BROTHER IN LAW) NIL NIL)
        ((BROTHER) NIL NIL))
    )

    (test-case-lookup-phrase (LOOKUP-PHRASE
                              '(GEORGE *POSS BROTHER IN LAW DORON SMITH IS A DRUG
                               ENFORCEMENT AGENT) *LM)
                               '(((GEORGE) (HUMAN F-NAME (GEORGE) GENDER (MALE)) NIL)
                               (*POSS BROTHER IN LAW DORON SMITH IS A DRUG ENFORCEMENT AGENT)
                                ) "LOOKUP-PHRASE_ex_1")

    (test-case-lookup-phrase (LOOKUP-PHRASE
                              '(BROTHER IN LAW DORON SMITH IS A DRUG ENFORCEMENT AGENT) *LM)
                              '(((BROTHER IN LAW) NIL NIL)
                                (DORON SMITH IS A DRUG ENFORCEMENT AGENT)
                               ) "LOOKUP-PHRASE_ex_2")

    (test-case-lookup-phrase (LOOKUP-PHRASE '(SMITH IS A DRUG ENFORCEMENT AGENT) *LM)
                             '(
                                  ((SMITH) NIL NIL)
                                  (IS A DRUG ENFORCEMENT AGENT)
                                ) "LOOKUP-PHRASE_ex_3")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (NEWGAPS)
(cond (NEWGAPS-tests
(format t "Testing NEWGAPS...~%")

(test-case (NEWGAPS '(TEACH AGENT (HUMAN F-NAME FN) RECIP RECIP
            OBJECT X))
           '(TEACH AGENT (HUMAN F-NAME FN0001)
             RECIP RECIP0002
             OBJECT X0003) "NEWGAPS_ex_1")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (IS-SUBCLASS)
(cond (IS-SUBCLASS-tests
(format t "Testing IS-SUBCLASS...~%")
    (SETUP-GLOBALS)

    (test-case (IS-SUBCLASS 'HUMAN 'ANIMATE) T "IS-SUBCLASS_ex_1")
    (test-case (IS-SUBCLASS 'HUMAN 'ACT) NIL "IS-SUBCLASS_ex_2")
    (test-case (IS-SUBCLASS 'TEACH 'ACT) T "IS-SUBCLASS_ex_3")
    (test-case (IS-SUBCLASS 'ACT 'THINK) NIL "IS-SUBCLASS_ex_4")
    (test-case (IS-SUBCLASS 'WEED 'WEED) T "IS-SUBCLASS_ex_a")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (FIND-CON)
(cond (FIND-CON-tests
(format t "Testing FIND-CON...~%")
    (SETUP-GLOBALS)

    (setq *WM '(CON0 CON1 CON2 CON3))
    (setq CON0 '(HUMAN))
    (setq CON1 '(INGEST AGENT AG01 OBJECT OBJ01))
    (setq CON2 '(SIZE VAL (>NORM)))
    (setq CON3 '(FOOD))

    ;(trace IS-SUBCLASS)

    (test-case (FIND-CON 'CON1 'AFT 'FOOD) 'CON3 "FIND-CON_ex_1")
    (test-case (FIND-CON 'CON1 'BEF 'FOOD) NIL "FIND-CON_ex_2")
    (test-case (FIND-CON 'CON1 'BEF 'ANIMATE) 'CON0' "FIND-CON_ex_2")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (MAIN-ACT)
(cond (MAIN-ACT-tests
(format t "Testing MAIN-ACT...~%")
    (SETUP-GLOBALS)

    (setq *TX '((MEMB INGEST PHYS-ACT)
              (MEMB COMMUN MENTAL-ACT)
              (MEMB TEACH MENTAL-ACT)
              (MEMB THINK ACT)
              (MEMB PHYS-ACT ACT)
              (MEMB MENTAL-ACT ACT)))

    (setq *WM '(CON1 CON2 CON3 CON4 CON5))
    (setq CON1 '(HUMAN F-NAME (DORON)
                GENDER (MALE)))
    (setq CON2 '(THINK AGENT CON1
                 OBJECT CON4))
    (setq CON3 '(HUMAN F-NAME (MARY)
                 GENDER (FEMALE)))
    (setq CON4 '(TEACH AGENT CON1
                 RECIP CON5))
    (setq CON5 '(HUMAN F-NAME (BETTY)
                 GENDER (FEMALE)))
    (setq CON6 '(HUMAN F-NAME (GEORGE)
                 ACTION CON4))

    (test-case (MAIN-ACT) 'CON2 "MAIN-ACT_ex_1")

    (setq *WM '(CON0 CON1 CON3 CON4 CON5 CON6))

    (test-case (MAIN-ACT) NIL "MAIN-ACT_ex_2")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (GEN-DEMS)
(cond (GEN-DEMS-tests
(format t "Testing GEN-DEMS...~%")
    (EX-CLEAR-GLOBALS)

    (setq *DM '())
    (test-case (GEN-DEMS '(
                (DEM-SRCH TYPE AFT INSTITUTION)
                (DEM-REF LOC BEF ACT)) 'CON44)
              '((DEM-SRCH CON44 TYPE AFT INSTITUTION)
                (DEM-REF CON44 LOC BEF ACT)) "GEN-DEMS_ex_1")

    (test-case (GEN-DEMS '((DEM-SRCH AGENT BEF HUMAN)) 'CON21)
                '((DEM-SRCH CON21 AGENT BEF HUMAN)
                (DEM-SRCH CON44 TYPE AFT INSTITUTION)
                (DEM-REF CON44 LOC BEF ACT)) "GEN-DEMS_ex_2")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (DEM-SRCH)
(cond (DEM-SRCH-tests
(format t "Testing DEM-SRCH...~%")
    (SETUP-GLOBALS)

    (setq *WM '(CON0 CON1))
    (setq CON0 '(HUMAN F-NAME (GEORGE)
                GENDER (MALE)))
    (setq CON1 '(TEACH AGENT AGENT1 RECIP RECIP1
                OBJECT OBJECT1))

    (test-case (DEM-SRCH 'CON1 'AGENT 'BEF 'HUMAN)
                'CON0 "GEN-DEMS_ex_1a")

    (test-case AGENT1 'CON0 "GEN-DEMS_ex_1b")

    (test-case (DEM-SRCH 'CON1 'AGENT 'AFT 'HUMAN)
                NIL "GEN-DEMS_ex_1a")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (DEM-AMEND)
(cond (DEM-AMEND-tests
(format t "Testing DEM-AMEND...~%")
    (SETUP-GLOBALS)

    (setq *WM '(CON1 CON2))
    (setq CON1 '(TEACH AGENT AGENT1
                RECIP RECIP1
                OBJECT OBJECT1))
    (setq CON2 '(INSTITUTION TYPE (HIGHSCHOOL)))

    (test-case (DEM-AMEND 'CON1 'LOC '(STATE NAME (CALIFORNIA)) 'AFT 'SOCIAL-ENT)
                'CON2 "DEM-AMEND_ex_1a")

    (test-case CON2
               '(INSTITUTION TYPE (HIGHSCHOOL)
                LOC (STATE NAME (CALIFORNIA))) "DEM-AMEND_ex_1b")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (DEM-REF)
(cond (DEM-REF-tests
(format t "Testing DEM-REF...~%")
    (SETUP-GLOBALS)

    (setq *WM '(CON1 CON2))
    (setq CON1 '(TEACH AGENT AGENT1
                RECIP RECIP1
                OBJECT OBJECT1))
    (setq CON2 '(INSTITUTION TYPE (HIGHSCHOOL)))

    (test-case (DEM-REF 'CON2 'LOC 'BEF 'ACT)
                'CON1 "DEM-REF_ex_1a")

    (test-case CON1
               '(TEACH AGENT AGENT1
                RECIP RECIP1 OBJECT OBJECT1 LOC CON2) "DEM-REF_ex_1b")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (DEM-EXEC)
(cond (DEM-EXEC-tests
(format t "Testing DEM-EXEC...~%")
    (EX-CLEAR-GLOBALS)

    (defun TRUE (arg) T)

    (setq *DM '(
     (DEM-SRCH CON1 AGENT BEF HUMAN)
     (DEM-SRCH CON2 OBJECT AFT ACT)
     (DEM-REF CON2 LOC BEF ACT)
     (TRUE 'BOB)))

    (test-case (DEM-EXEC) '((DEM-SRCH CON1 AGENT BEF HUMAN) (DEM-SRCH CON2 OBJECT AFT ACT)
                            (DEM-REF CON2 LOC BEF ACT)) "DEM-EXEC_ex_1")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (PARSE-SENT)
(cond (PARSE-SENT-tests
(format t "Testing PARSE-SENT...~%")
    (SETUP-GLOBALS)

    ; TODO: Add more tests!

(format t "===========================~%")
))

; -----------------------------------------------------------------------------
