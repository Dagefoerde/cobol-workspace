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
