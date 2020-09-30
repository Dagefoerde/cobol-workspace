       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOLDEM_M.
      *****************************************************************
      * Implementierung des Holdem Spiels.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01  HAND.
      *    KREUZ 10, HERZ  KÖNIG
         05  KARTE OCCURS 5.
           07  FARBE PIC X(5).
           07  FILLER PIC X.
           07  WERT PIC X(5).
       01  AUSGABE.
         05  KOMMENTAR PIC X(20).
           88  HoechstesPaerchen VALUE "Höchstes Pärchen:".
           88  LeereHand VALUE "Leere Hand!".
           88  HoechsteKarte VALUE "Höchste Karte:".
         05  A-WERT PIC X(5).
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Beispiel Testsuite'
           DISPLAY SPACE



           MOVE 'Leere Hand'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE SPACE TO HAND
               PERFORM Hand-Auswerten
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE AUSGABE TO UT-ACTUAL
           MOVE "Leere Hand!"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Eine Karte übergeben'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "KREUZ 10" TO KARTE(1)
               PERFORM Hand-Auswerten
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE A-WERT TO UT-ACTUAL
           MOVE "10"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF HoechsteKarte
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Zwei gleiche Karten übergeben'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "PIK   10" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               PERFORM Hand-Auswerten
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE AUSGABE TO UT-ACTUAL
           MOVE "Höchstes Pärchen: 10"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Zwei unterschiedliche Karten übergeben'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "PIK   9" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               PERFORM Hand-Auswerten
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE A-WERT TO UT-ACTUAL
           MOVE "10"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF HoechsteKarte
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.
               INITIALIZE AUSGABE

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.



           STOP RUN

           .
       Hand-Auswerten Section.
           IF HAND = SPACE THEN
               MOVE "Leere Hand!" TO AUSGABE
           ELSE
      *        MOVE KARTE(1) TO AUSGABE
               IF WERT(1) = WERT(2) THEN
                   SET HoechstesPaerchen TO TRUE
                   MOVE WERT(1) TO A-WERT
               ELSE
                   SET HoechsteKarte TO TRUE
                   MOVE WERT(1) TO A-WERT
               END-IF
           END-IF

           .
       bla Section.
           MOVE "KREUZ 10111XX" TO HAND
      *    DISPLAY HAND
           DISPLAY KARTE(1)
           DISPLAY KARTE(2)
           DISPLAY KARTE(3)
           DISPLAY KARTE(4)
           DISPLAY KARTE(5)
           STOP RUN
           .
