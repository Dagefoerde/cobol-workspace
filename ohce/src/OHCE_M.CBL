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
       01  ORIGINAL.
        05 ORIGINAL-BYTE OCCURS 20 PIC X.
       01  REVD       PIC X(20).
       01  PALINDROME PIC 9.
        88 ISPALINDROME VALUE 1.
       01  USERNAME   PIC X(20).
       01  LEN        PIC 99.
       01  READCHAR   PIC 99.
       01  WRITECHAR  PIC 99.
       01  CURRENT-DATE-DATA.
        05 DATETIME   PIC 9(16).
        05 CURRENT-OTHER  PIC S9(4).

       PROCEDURE DIVISION.
           ACCEPT USERNAME
           PERFORM DO-GREET.

           PERFORM UNTIL 1=2
             ACCEPT ORIGINAL
             IF ORIGINAL IS EQUAL TO "Stop!" THEN
               DISPLAY "Adios " USERNAME
               STOP RUN
             END-IF

             PERFORM DO-REVERSE
             DISPLAY REVD

             PERFORM DO-PALINDROME
             IF ISPALINDROME THEN
               DISPLAY "¡Bonita palabra!"

           END-PERFORM
           .

       DO-GREET SECTION.
           MOVE FUNCTION CURRENT-DATE to CURRENT-DATE-DATA
           EVALUATE CURRENT-DATE-DATA(9:2)
           WHEN 06 THROUGH 11
             DISPLAY "¡Buenos días " Function TRIM(USERNAME) "!"
           WHEN 12 THROUGH 19
             DISPLAY "¡Buenas tardes " Function TRIM(USERNAME) "!"
           WHEN OTHER
      *      20--05
             DISPLAY "¡Buenas noches " Function TRIM(USERNAME) "!"
           .

       DO-REVERSE SECTION.
           INITIALIZE WRITECHAR
           INITIALIZE REVD
           PERFORM COUNT-LENGTH
           PERFORM VARYING READCHAR FROM LEN BY -1 UNTIL READCHAR = 0
             ADD 1 TO WRITECHAR
             MOVE ORIGINAL-BYTE(READCHAR) TO REVD(WRITECHAR:1)
           END-PERFORM
           
       .

       COUNT-LENGTH SECTION.
      *    Courtesy of https://stackoverflow.com/a/24778914 🤷‍♂️.
           IF ORIGINAL EQUAL TO SPACE
             MOVE ZERO TO LEN
           ELSE
             MOVE LENGTH OF ORIGINAL TO LEN
             PERFORM 
             UNTIL ORIGINAL-BYTE ( LEN ) 
               NOT EQUAL TO SPACE
               SUBTRACT +1 FROM LEN
             END-PERFORM
           END-IF
           .

       DO-PALINDROME SECTION.
           INITIALIZE PALINDROME
           IF REVD IS EQUAL TO ORIGINAL THEN
             SET ISPALINDROME TO TRUE
           END-IF
       .