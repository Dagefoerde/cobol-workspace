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
           
           ACCEPT Hand
           PERFORM Hand-Auswerten

           DISPLAY AUSGABE
           STOP RUN.

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
           STOP RUN.
