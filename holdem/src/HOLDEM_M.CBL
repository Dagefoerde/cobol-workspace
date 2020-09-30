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
           

           STOP RUN.

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
       
       bla Section.
           MOVE "KREUZ 10111XX" TO HAND
      *    DISPLAY HAND
           DISPLAY KARTE(1)
           DISPLAY KARTE(2)
           DISPLAY KARTE(3)
           DISPLAY KARTE(4)
           DISPLAY KARTE(5)
           STOP RUN.