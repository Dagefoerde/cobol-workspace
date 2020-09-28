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
       01  GOAL                          PIC 999 VALUE 100.
       01  CURRENT                       PIC 999.
       01  CURRENTSTR REDEFINES CURRENT  PIC Z(3).
       01  CALC-RESULT                   PIC X(8) VALUE SPACE.
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
           MOVE '001 is literal'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 001 TO CURRENT
           PERFORM CALC
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CALC-RESULT TO UT-ACTUAL
           MOVE '001'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE '003 is fizz'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 003 TO CURRENT
           PERFORM CALC
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CALC-RESULT TO UT-ACTUAL
           MOVE 'FIZZ'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE '005 is buzz'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 005 TO CURRENT
           PERFORM CALC
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CALC-RESULT TO UT-ACTUAL
           MOVE 'BUZZ'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE '090 is fizzbuzz'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 090 TO CURRENT
           PERFORM CALC
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CALC-RESULT TO UT-ACTUAL
           MOVE 'FIZZBUZZ'
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
           INITIALIZE CURRENT
           PERFORM VARYING CURRENT FROM 1 BY 1
             UNTIL CURRENT = GOAL
             PERFORM PRINT
           END-PERFORM


           .
       PRINT SECTION.
           PERFORM CALC
           DISPLAY CALC-RESULT


           .
       CALC SECTION.
           INITIALIZE CALC-RESULT
           IF FUNCTION MOD(CURRENT,3) > 0 AND
              FUNCTION MOD(CURRENT,5) > 0 THEN
             MOVE CURRENTSTR TO CALC-RESULT
           ELSE
             IF FUNCTION MOD(CURRENT,3) = 0 AND
                FUNCTION MOD(CURRENT,5) = 0 THEN
               MOVE 'FIZZBUZZ' TO CALC-RESULT
             ELSE IF FUNCTION MOD(CURRENT,3) = 0 THEN
               MOVE 'FIZZ' TO CALC-RESULT
             ELSE IF FUNCTION MOD(CURRENT,5) = 0 THEN
               MOVE 'BUZZ' TO CALC-RESULT
             END-IF
           END-IF


           .

