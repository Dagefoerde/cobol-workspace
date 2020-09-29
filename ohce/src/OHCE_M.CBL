       IDENTIFICATION DIVISION.
       PROGRAM-ID.  OHCE_M.
      *****************************************************************
      * Implementierng der OHCE Kata.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ORIGINAL   PIC X(20).
       01  REVD    PIC X(20).
       01  PALINDROME PIC 9.
        88 ISPALINDROME VALUE 1.
       01  USERNAME   PIC X(20).
       PROCEDURE DIVISION.
           ACCEPT USERNAME
           DISPLAY "¡Hola " USERNAME "!"
           PERFORM UNTIL 1=2
             ACCEPT ORIGINAL
             IF ORIGINAL = "Stop!" THEN
               STOP RUN
             END-IF

             PERFORM DO-REVERSE
             DISPLAY REVD

             PERFORM DO-PALINDROME
             IF ISPALINDROME THEN
               DISPLAY "¡Bonita palabra!"

           END-PERFORM
           .

       DO-REVERSE SECTION.
           MOVE ORIGINAL TO REVD
       .

       DO-PALINDROME SECTION.
           INITIALIZE PALINDROME
           IF REVD IS EQUAL TO ORIGINAL THEN
             SET ISPALINDROME TO TRUE
           END-IF
       .