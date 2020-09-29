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
            COPY ZUTZCWS     .
       01  ORIGINAL   PIC X(20).
       01  REVD    PIC X(20).
       01  PALINDROME PIC 9.
        88 ISPALINDROME VALUE 1.
       01  USERNAME   PIC X(20).
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Test OHCE Kata'
           DISPLAY SPACE

           MOVE 'Reversing works (5 char)'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'ABCDE' TO ORIGINAL
           PERFORM DO-REVERSE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE REVD TO UT-ACTUAL
           MOVE "EDCBA"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Reversing works (1 char)'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'A' TO ORIGINAL
           PERFORM DO-REVERSE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE REVD TO UT-ACTUAL
           MOVE "A"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Reversing works (emptystring)'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE '' TO ORIGINAL
           PERFORM DO-REVERSE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE REVD TO UT-ACTUAL
           MOVE ""
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Reversing works with emoji'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'A😢' TO ORIGINAL
           PERFORM DO-REVERSE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE REVD TO UT-ACTUAL
           MOVE "😢A"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Can discover a palindrome'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'ABBA' TO ORIGINAL
           PERFORM DO-REVERSE
           PERFORM DO-PALINDROME
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF ISPALINDROME
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Can discover a notpalindrome'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'ABAB' TO ORIGINAL
           PERFORM DO-REVERSE
           PERFORM DO-PALINDROME
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF ISPALINDROME
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'FALSE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
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
       .

           .
       DO-PALINDROME SECTION.
           INITIALIZE PALINDROME
           IF REVD IS EQUAL TO ORIGINAL THEN
             SET ISPALINDROME TO TRUE
           END-IF
           .
       .
           .
           .
       .
