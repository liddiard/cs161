;;; CS 161 Winter 2016: Example Test Framework

; [!] Load your HW4 solution here!
(load "hw-4-solution.lsp")

; [!] IMPORTANT ===============================================================
; It is assumed that, by this point, you have included either the HW3 solution,
; or your own HW3 solution that includes our HW1, 3. This is important for the
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
; each matching element being compared using equivalence (#'equal)
(defun test-case-set (actual expected case-name)
    (cond
        ((and (subsetp actual expected :test #'equal) (subsetp expected actual :test #'equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)


; -----------------------------------------------------------------------------
; Test Frames, Variables, & Globals
; -----------------------------------------------------------------------------

(setq *CUR-Q-CON nil
      *CUR-C-ANS nil
      *CUR-FILLER nil

      COCAINE-DEALER-ENG-PATS '(
         (TEACH (SL-NAME AGENT) (PHRASE TEACHES) (SL-NAME OBJECT) (PHRASE AT)
                (SL-NAME LOC) (PHRASE TO) (SL-NAME RECIP))
         (HUMAN (D-TREE DTR-1))
         (CHEM (PHRASE CHEMISTRY))
         (HS (SL-NAME REF) (PHRASE HIGH SCHOOL))
         (STATE (D-TREE DTR-2))
         (INFORM (SL-NAME AGENT) (PHRASE TOLD) (SL-NAME RECIP) (PHRASE THAT)
                 (SL-NAME OBJECT))
         (CANCER (SL-NAME TYPE) (PHRASE CANCER))
         (INGEST (D-TREE DTR-3))
         (NOSE (PHRASE NOSE OF) (SL-NAME PARTOF))
         (DEF (PHRASE THE))
         (PHYSICIAN (D-TREE DTR-4) (PHRASE IS) (SL-NAME OBJECT) (SL-NAME ROLE))
         (ATTENDED (D-TREE DTR-4) (PHRASE WAS) (SL-NAME AGENT) (SL-NAME OBJECT))
      )

      DTR-1 '(
        (AND (GET-SF 'ROLE *CUR-FILLER)
             '((SL-NAME REF) (SL-NAME ROLE)))
        (AND (GET-SF 'TYPE *CUR-FILLER)
             '((SL-NAME TYPE)))
        (AND (GET-SF 'F-NAME *CUR-FILLER)
             '((SL-NAME F-NAME) (SL-NAME L-NAME)))
        (AND (GET-SF 'GENDER *CUR-FILLER)
             '((SL-NAME GENDER)))
      )

      DTR-2 '(
        (AND (SETQ AGNT (GET-NESTED-SF '(AGENT F-NAME) *CUR-FILLER))
             (SETQ GENDER (GET-NESTED-SF '(AGENT GENDER) *CUR-FILLER))
             (EQUAL AGNT (GET-NESTED-SF '(RECIP F-NAME) *CUR-C-ANS))
             (EQUAL GENDER '(MALE))
             '((PHRASE HE HAS) (SL-NAME OBJECT)))
        (AND '((PHRASE SHE HAS) (SL-NAME OBJECT)))
      )

      DTR-3 '(
        (AND (EQUAL (first *CUR-Q-CON) 'CAUSE)
             (IS-VARIABLE (GET-SF 'ANTE *CUR-Q-CON))
             (EQUAL (first (GET-SF 'TO *CUR-C-ANS)) 'NOSE)
             '((PHRASE BECAUSE) (SL-NAME AGENT) (PHRASE PUT)
               (SL-NAME OBJECT) (PHRASE INTO) (SL-NAME TO)))
      )
)

(setq DTR-4 '((AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(MALE)) '((PHRASE HE)))
              (AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(FEMALE)) '((PHRASE SHE)))
              (AND '((PHRASE IT)))))


; -----------------------------------------------------------------------------
; Unit Test Flags
; -----------------------------------------------------------------------------

; Unit test activations: only run the unit tests marked with t
(setq
    FR-TO-ENG-tests t
    EVAL-D-TREE-tests t
)

(setq Q-CON1 '(ACT AGENT (HUMAN F-NAME (GEORGE)
              GENDER (MALE)) LOC (HS REF (DEF))
              CLASS (V WHAT?)))

(setq C-ANS-1 '(TEACH AGENT (HUMAN F-NAME (GEORGE)
                GENDER (MALE)) RECIP (HUMAN TYPE (STUDENTS))
                OBJECT (CHEM)
                LOC (HS REF (DEF)) SITU (S1)))

(setq Q-CON2 '(CAUSE ANTE (V WHY?)
              CONSEQ (APPEAR OBJECT (LESIONS LOC (NOSE PARTOF
              (HUMAN F-NAME (RICK)
              GENDER (MALE)))))))

(setq C-ANS-2 '(INGEST AGENT (HUMAN F-NAME (RICK)
                GENDER (MALE)) OBJECT (COCAINE)
                TO (NOSE PARTOF (HUMAN F-NAME (RICK) GENDER (MALE)))
                SITU (S4)))

(setq Q-CON3 '(ENABLES ANTE (V HOW?)
               CONSEQ (KNOWS AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE))
               OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE))
               OBJECT (DISEASED)))))

(setq C-ANS-3 '(INFORM AGENT (HUMAN ROLE (ONCOLOGIST)
                REF (DEF)) RECIP (HUMAN F-NAME (GEORGE)
                GENDER (MALE))
                OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
                GENDER (MALE)) OBJECT (CANCER TYPE (TERMINAL)))
                SITU (S3)))

; -----------------------------------------------------------------------------
; Unit Tests
; -----------------------------------------------------------------------------

; Test cases: (FR-TO-ENG)
(cond (FR-TO-ENG-tests
(format t "Testing FR-TO-ENG...~%")

(setq EX21-A-CON '(TEACH AGENT (GEORGE) OBJECT (CHEM)
                  LOC (HS REF (DEF)) RECIP (STUDENTS)))

(setq Q-CON* '(QUESTION))

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS EX21-A-CON Q-CON*)
            '(GEORGE TEACHES CHEMISTRY AT THE HIGH SCHOOL TO STUDENTS)
            "FR-TO-ENG_ex_1")

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-2 Q-CON2)
            '(BECAUSE RICK PUT COCAINE INTO NOSE OF RICK)
            "FR-TO-ENG_ex_2")

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-3 Q-CON3)
            '(THE ONCOLOGIST TOLD GEORGE THAT HE HAS TERMINAL CANCER)
            "FR-TO-ENG_ex_3")

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS '(PHYSICIAN F-NAME (DOC)
            GENDER (MALE)
            ROLE (ONCOLOGIST)
            OBJECT (HUMAN F-NAME (GEORGE)))
            '(Q))
            '(HE IS GEORGE ONCOLOGIST)
            "FR-TO-ENG_ex_4")

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS
            '(ATTENDED AGENT (HUMAN F-NAME (GEORGE))
            OBJECT (HS)) '(Q))
            '(IT WAS GEORGE HIGH SCHOOL)
            "FR-TO-ENG_ex_5")


;; extra test cases
(setq

COCAINE-DEALER-ENG-PATS '(
    (TEACH (SL-NAME AGENT) (PHRASE TEACHES) (SL-NAME OBJECT) (PHRASE AT)
           (SL-NAME LOC) (PHRASE TO) (SL-NAME RECIP))
    (HUMAN (D-TREE DTR-1))
    (CHEM (PHRASE CHEMISTRY))
    (HS (SL-NAME REF) (PHRASE HIGH SCHOOL))
    (STATE (D-TREE DTR-2))
    (INFORM (SL-NAME AGENT) (PHRASE TOLD) (SL-NAME RECIP) (PHRASE THAT)
            (SL-NAME OBJECT))
    (CANCER (SL-NAME TYPE) (PHRASE CANCER))
    (INGEST (D-TREE DTR-3))
    (NOSE (PHRASE NOSE OF) (SL-NAME PARTOF))
    (DEF (PHRASE THE))
    (PHYSICIAN (D-TREE DTR-4) (PHRASE IS) (SL-NAME OBJECT) (SL-NAME ROLE))
    (ATTENDED (D-TREE DTR-4) (SL-NAME AGENT) (SL-NAME OBJECT))
    (COMEDIAN (D-TREE DTR-4) (PHRASE MAKES ME LAUGH) (D-TREE DTR-4) (D-TREE DTR-4))
    (EXPLANATION (PHRASE IT WAS) (D-TREE DTR-5))
)

C-ANS-4 '(EXPLANATION AGENT (HUMAN F-NAME (GEORGE)
                                   GENDER (MALE))
                      TIME (MIDNIGHT)
                      DATE (JULY-4-2015)
                      LOC (HS REF (RIDGEMONT)))

DTR-4 '(
    (AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(MALE))
         '((PHRASE HE)))
    (AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(FEMALE))
         '((PHRASE SHE)))
    (AND '((PHRASE IT)))
)

DTR-5 '(
    (AND (EQUAL (GET-SF 'QUERY *CUR-Q-CON) '(WHO))
         '((SL-NAME AGENT)))
    (AND (EQUAL (GET-SF 'QUERY *CUR-Q-CON) '(WHEN))
         '((PHRASE AT) (SL-NAME TIME) (PHRASE ON) (SL-NAME DATE)))
    (AND (EQUAL (GET-SF 'QUERY *CUR-Q-CON) '(WHERE))
         '((SL-NAME LOC)))
    '(DUNNO)
))

; More caveman English as follows
(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS
           '(ATTENDED AGENT (COMEDIAN GENDER (MALE)))
           '(Q))
           '(IT HE MAKES ME LAUGH HE HE) "FR-TO-ENG_ex_6")

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-4 '(FOLLOW-UP QUERY (WHO)))
           '(IT WAS GEORGE) "FR-TO-ENG_ex_7")

(test-case (FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-4 '(FOLLOW-UP QUERY (WHEN)))
           '(IT WAS AT MIDNIGHT ON JULY-4-2015) "FR-TO-ENG_ex_8")


(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (EVAL-D-TREE)
(cond (EVAL-D-TREE-tests
(format t "Testing EVAL-D-TREE...~%")

(setq *CUR-FILLER '(HUMAN F-NAME (GEORGE) L-NAME (WHITE)))

(test-case (EVAL-D-TREE DTR-1)
            '((SL-NAME F-NAME) (SL-NAME L-NAME))
            "EVAL-D-TREE_ex_1")

(setq *CUR-FILLER '(TEACH AGENT (HUMAN F-NAME (GEORGE) GENDER (MALE)) OBJECT (CHEM)))
(setq *CUR-C-ANS '(PTRANS AGENT (DRUGEE) RECIP (HUMAN F-NAME (GEORGE)) OBJECT (MONEY)))

(test-case (EVAL-D-TREE DTR-2)
            '((PHRASE HE HAS) (SL-NAME OBJECT))
            "EVAL-D-TREE_ex_2")

(setq *CUR-FILLER '(HUMAN GENDER (MALE)))

(test-case (EVAL-D-TREE DTR-4)
            '((PHRASE HE))
            "EVAL-D-TREE_ex_3")

(format t "===========================~%")
))

; -----------------------------------------------------------------------------
