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
       01  GOAL      PIC 999 VALUE 100.
       01  CURRENT   PIC 999.
       PROCEDURE DIVISION.
           PERFORM FIZZ
           GOBACK.
       FIZZ SECTION.
           DISPLAY GOAL
           SUBTRACT 1 FROM GOAL
           PERFORM GOAL TIMES
             ADD 1 TO CURRENT
             PERFORM PRINT
           GOBACK.

       PRINT SECTION.
           IF FUNCTION MOD(CURRENT,3) = 0 THEN
             DISPLAY 'FIZZ'
           END-IF
           IF FUNCTION MOD(CURRENT,5) = 0 THEN
             DISPLAY 'BUZZ'
           END-IF
           IF FUNCTION MOD(CURRENT,3) > 0 AND FUNCTION MOD(CURRENT,5) > 0 THEN
             DISPLAY CURRENT
           GOBACK.