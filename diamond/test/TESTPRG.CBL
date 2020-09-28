       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIAMONDM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01  GOALLETTER         PIC A.
       01  GOALNUMBER         PIC 99.
       01  CURRENTNUMBER      PIC 99 VALUE 00.
       01  CURRENTLETTER      PIC A VALUE SPACE.
       01  CURRENTLINE        PIC X(51).
       01  CURRENTEND         PIC 99 VALUE 51.
       01  CURRENTDIFF        PIC 99 VALUE 00.

       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Testfälle für unseren Diamanten'
           DISPLAY SPACE

           MOVE 'Zeile A für D'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "D" TO GOALLETTER
               MOVE 1 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENTLINE TO UT-ACTUAL
           MOVE "   A   "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Zeile B für D'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "D" TO GOALLETTER
               MOVE 2 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENTLINE TO UT-ACTUAL
           MOVE "  B B  "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Zeile D für D'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "D" TO GOALLETTER
               MOVE 4 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENTLINE TO UT-ACTUAL
           MOVE "D     D"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Zeile E für E'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "E" TO GOALLETTER
               MOVE 5 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENTLINE TO UT-ACTUAL
           MOVE "E       E"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Buchstabe zu Zahl'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "E" TO GOALLETTER
               PERFORM GOALTONUMBER
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE GOALNUMBER TO UT-ACTUAL
           MOVE 05
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Zahl zu Buchstabe'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 8 TO CURRENTNUMBER
               PERFORM NUMBERTOLETTER
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CURRENTLETTER TO UT-ACTUAL
           MOVE "H"
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

           DISPLAY "Enter letter:"
           ACCEPT GOALLETTER
           PERFORM GOALTONUMBER
           PERFORM DIAMOND
           STOP RUN

           .
       DIAMOND SECTION.
           PERFORM VARYING CURRENTNUMBER FROM 1 BY 1
             UNTIL CURRENTNUMBER = GOALNUMBER
             PERFORM GETLINE
             DISPLAY CURRENTLINE
           END-PERFORM
           PERFORM VARYING CURRENTNUMBER FROM GOALNUMBER BY -1
             UNTIL CURRENTNUMBER = 0
             PERFORM GETLINE
             DISPLAY CURRENTLINE
           END-PERFORM

           .
       GETLINE SECTION.
      *    Initializations
           INITIALIZE CURRENTLINE
           MOVE GOALNUMBER TO CURRENTDIFF
           SUBTRACT CURRENTNUMBER FROM CURRENTDIFF
           COMPUTE CURRENTEND = GOALNUMBER * 2 - 1 - CURRENTDIFF
           ADD 1 TO CURRENTDIFF
      *    Transform
           PERFORM NUMBERTOLETTER
      *    Construct output line
           MOVE CURRENTLETTER TO CURRENTLINE (CURRENTDIFF:1)
           MOVE CURRENTLETTER TO CURRENTLINE (CURRENTEND:1)


           .
       GOALTONUMBER SECTION.
           EVALUATE GOALLETTER
               WHEN "A"
                   MOVE 1 TO GOALNUMBER
               WHEN "B"
                   MOVE 2 TO GOALNUMBER
               WHEN "C"
                   MOVE 3 TO GOALNUMBER
               WHEN "D"
                   MOVE 4 TO GOALNUMBER
               WHEN "E"
                   MOVE 5 TO GOALNUMBER
               WHEN "F"
                   MOVE 6 TO GOALNUMBER
               WHEN "G"
                   MOVE 7 TO GOALNUMBER
               WHEN "H"
                   MOVE 8 TO GOALNUMBER
               WHEN "I"
                   MOVE 9 TO GOALNUMBER
               WHEN "J"
                   MOVE 10 TO GOALNUMBER
               WHEN "K"
                   MOVE 11 TO GOALNUMBER
               WHEN "L"
                   MOVE 12 TO GOALNUMBER
               WHEN "M"
                   MOVE 13 TO GOALNUMBER
               WHEN "N"
                   MOVE 14 TO GOALNUMBER
               WHEN "O"
                   MOVE 15 TO GOALNUMBER
               WHEN "P"
                   MOVE 16 TO GOALNUMBER
               WHEN "Q"
                   MOVE 17 TO GOALNUMBER
               WHEN "R"
                   MOVE 18 TO GOALNUMBER
               WHEN "S"
                   MOVE 19 TO GOALNUMBER
               WHEN "T"
                   MOVE 20 TO GOALNUMBER
               WHEN "U"
                   MOVE 21 TO GOALNUMBER
               WHEN "V"
                   MOVE 22 TO GOALNUMBER
               WHEN "W"
                   MOVE 23 TO GOALNUMBER
               WHEN "X"
                   MOVE 24 TO GOALNUMBER
               WHEN "Y"
                   MOVE 25 TO GOALNUMBER
               WHEN "Z"
                   MOVE 26 TO GOALNUMBER
           END-EVALUATE

           .
       NUMBERTOLETTER SECTION.
           EVALUATE CURRENTNUMBER
               WHEN 1
                   MOVE "A" TO CURRENTLETTER
               WHEN 2
                   MOVE "B" TO CURRENTLETTER
               WHEN 3
                   MOVE "C" TO CURRENTLETTER
               WHEN 4
                   MOVE "D" TO CURRENTLETTER
               WHEN 5
                   MOVE "E" TO CURRENTLETTER
               WHEN 6
                   MOVE "F" TO CURRENTLETTER
               WHEN 7
                   MOVE "G" TO CURRENTLETTER
               WHEN 8
                   MOVE "H" TO CURRENTLETTER
               WHEN 9
                   MOVE "I" TO CURRENTLETTER
               WHEN 10
                   MOVE "J" TO CURRENTLETTER
               WHEN 11
                   MOVE "K" TO CURRENTLETTER
               WHEN 12
                   MOVE "L" TO CURRENTLETTER
               WHEN 13
                   MOVE "M" TO CURRENTLETTER
               WHEN 14
                   MOVE "N" TO CURRENTLETTER
               WHEN 15
                   MOVE "O" TO CURRENTLETTER
               WHEN 16
                   MOVE "P" TO CURRENTLETTER
               WHEN 17
                   MOVE "Q" TO CURRENTLETTER
               WHEN 18
                   MOVE "R" TO CURRENTLETTER
               WHEN 19
                   MOVE "S" TO CURRENTLETTER
               WHEN 20
                   MOVE "T" TO CURRENTLETTER
               WHEN 21
                   MOVE "U" TO CURRENTLETTER
               WHEN 22
                   MOVE "V" TO CURRENTLETTER
               WHEN 23
                   MOVE "W" TO CURRENTLETTER
               WHEN 24
                   MOVE "X" TO CURRENTLETTER
               WHEN 25
                   MOVE "Y" TO CURRENTLETTER
               WHEN 26
                   MOVE "Z" TO CURRENTLETTER
           END-EVALUATE

           .
       END PROGRAM DIAMONDM.
