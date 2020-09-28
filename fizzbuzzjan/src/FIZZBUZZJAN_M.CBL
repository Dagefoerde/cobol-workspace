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
       01  GOAL                          PIC 999 VALUE 100.
       01  CURRENT                       PIC 999.
       01  CURRENTSTR REDEFINES CURRENT  PIC Z(3).
       01  CALC-RESULT                   PIC X(8) VALUE SPACE.
       PROCEDURE DIVISION.
           PERFORM FIZZ
           GOBACK.
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

