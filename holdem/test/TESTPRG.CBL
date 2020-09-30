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
           07  WERT PIC 99.
       01  AUSGABE.
         05  KOMMENTAR PIC X(20).
           88  HoechstesPaerchen VALUE "Höchstes Pärchen:".
           88  LeereHand VALUE "Leere Hand!".
           88  HoechsteKarte VALUE "Höchste Karte:".
         05  A-WERT PIC 99.
       01  HISTOGRAMM.
         05  ANZAHL OCCURS 14 PIC 9.
       01  KARTENINDEX PIC 9.

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
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF LeereHand
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Eine Karte übergeben'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 10 TO WERT(1)
               PERFORM Hand-Auswerten
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE A-WERT TO UT-ACTUAL
           MOVE 10
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
               MOVE "PIK   09" TO KARTE(1)
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
           MOVE 'Drei unterschiedliche Karten übergeben'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE "PIK   09" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               MOVE "KREUZ 11" TO KARTE(3)
               PERFORM Hand-Auswerten
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE A-WERT TO UT-ACTUAL
           MOVE "11"
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
           MOVE 'Pärchen im Histogramm'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 14 TO WERT(1)
               MOVE 10 TO WERT(2)
               MOVE 11 TO WERT(3)
               MOVE 09 TO WERT(4)
               MOVE 11 TO WERT(5)
               PERFORM Bastel-Histogramm
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE A-WERT TO UT-ACTUAL
           MOVE 11
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF HoechstesPaerchen
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Überprüfe Histogramm'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 11 TO WERT(1)
               MOVE 10 TO WERT(2)
               MOVE 11 TO WERT(3)
               MOVE 09 TO WERT(4)
               MOVE 11 TO WERT(5)
               PERFORM Bastel-Histogramm
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ANZAHL(11) TO UT-ACTUAL
           MOVE 3
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ANZAHL(10) TO UT-ACTUAL
           MOVE 1
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ANZAHL(09) TO UT-ACTUAL
           MOVE 1
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ANZAHL(08) TO UT-ACTUAL
           MOVE 0
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Unterschiedliche Karten im Histogramm'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 02 TO WERT(1)
               MOVE 10 TO WERT(2)
               MOVE 14 TO WERT(3)
               MOVE 09 TO WERT(4)
               MOVE 11 TO WERT(5)
               PERFORM Bastel-Histogramm
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE A-WERT TO UT-ACTUAL
           MOVE 14
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
               INITIALIZE HISTOGRAMM

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.


           ACCEPT Hand
           PERFORM Hand-Auswerten

           DISPLAY AUSGABE
           STOP RUN

           .
       Hand-Auswerten Section.
           IF HAND = SPACE THEN
               SET LeereHand TO TRUE
           ELSE
               PERFORM Bastel-Histogramm
           END-IF


           .
       Bastel-Histogramm Section.
           SET HoechsteKarte TO TRUE
           PERFORM VARYING KARTENINDEX FROM 1
               UNTIL KARTENINDEX > 5
               IF WERT(KARTENINDEX) > 0
                 ADD 1 TO ANZAHL(WERT(KARTENINDEX))
                 IF ANZAHL(WERT(KARTENINDEX)) = 2
                   IF NOT HoechstesPaerchen
                      OR WERT(KARTENINDEX) > A-WERT
                   THEN
                       MOVE WERT(KARTENINDEX) TO A-WERT
                   END-IF
                   SET HoechstesPaerchen TO TRUE
                 END-IF
                 IF NOT HoechstesPaerchen
                   IF WERT(KARTENINDEX) > A-WERT
                       MOVE WERT(KARTENINDEX) TO A-WERT
                   END-IF
                 END-IF
               END-IF
           END-PERFORM




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
