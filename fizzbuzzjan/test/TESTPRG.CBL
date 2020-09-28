       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FIZZBUZZJAN_M.
       AUTHOR. Jan.
       DATE-WRITTEN. Sep 28, 2020.
      *****************************************************************
      * FizzBuzz-Kata.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01  GOAL      PIC 999 VALUE 100.
       01  CURRENT   PIC 999.
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Beispiel Testsuite'
           DISPLAY SPACE

           MOVE 'Beispiel Testcase'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           PERFORM FIZZ
           Display 'Das ist ein Testcase'

           PERFORM UT-AFTER
           MOVE 'Alternativer Testcase'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 20 TO GOAL
           PERFORM FIZZ
           Display 'Das ist ein Testcase'
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.

           PERFORM FIZZ
           GOBACK
           .
       FIZZ SECTION.
           DISPLAY GOAL
           SUBTRACT 1 FROM GOAL
           PERFORM GOAL TIMES
             ADD 1 TO CURRENT
             PERFORM PRINT
           GOBACK

           .
       PRINT SECTION.
           IF FUNCTION MOD(CURRENT,3) = 0 THEN
             DISPLAY 'FIZZ'
           END-IF
           IF FUNCTION MOD(CURRENT,5) = 0 THEN
             DISPLAY 'BUZZ'
           END-IF
           IF FUNCTION MOD(CURRENT,3) > 0 AND FUNCTION MOD(CURRENT,5) > 0 THEN
             DISPLAY CURRENT
           GOBACK
           .
