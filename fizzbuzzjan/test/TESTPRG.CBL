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
           'Fizzbuzz Testsuite'
           DISPLAY SPACE

           MOVE 'Goal is default'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           PERFORM FIZZ
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENT TO UT-ACTUAL
           MOVE 100
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Goal is 20'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 020 TO GOAL
           PERFORM FIZZ
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENT TO UT-ACTUAL
           MOVE 020
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
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
           MOVE 000 TO CURRENT
           PERFORM GOAL TIMES
             ADD 1 TO CURRENT
             PERFORM PRINT
           END-PERFORM


           .
       PRINT SECTION.
           IF FUNCTION MOD(CURRENT,3) = 0 THEN
             DISPLAY 'FIZZ' WITH NO ADVANCING
           END-IF
           IF FUNCTION MOD(CURRENT,5) = 0 THEN
             DISPLAY 'BUZZ' WITH NO ADVANCING
           END-IF
           IF FUNCTION MOD(CURRENT,3) > 0 AND
              FUNCTION MOD(CURRENT,5) > 0 THEN
             DISPLAY CURRENT
           ELSE
      *      Newline
             DISPLAY ''
           END-IF

           .
