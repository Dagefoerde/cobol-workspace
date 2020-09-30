           TESTSUITE 'Beispiel Testsuite'

              
           BEFORE-EACH
               INITIALIZE AUSGABE
               INITIALIZE HISTOGRAMM
           END-BEFORE

           TESTCASE 'Leere Hand'
               MOVE SPACE TO HAND
               PERFORM Hand-Auswerten
           EXPECT LeereHand TO BE TRUE

           TESTCASE 'Eine Karte übergeben'
               MOVE 10 TO WERT(1)
               PERFORM Hand-Auswerten
           EXPECT A-WERT TO BE 10
           EXPECT HoechsteKarte TO BE TRUE

           TESTCASE 'Zwei gleiche Karten übergeben'
               MOVE "PIK   10" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               PERFORM Hand-Auswerten
           EXPECT AUSGABE TO BE "Höchstes Pärchen: 10"

      *    TESTCASE 'König konvertieren'

           TESTCASE 'Zwei unterschiedliche Karten übergeben'
               MOVE "PIK   09" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               PERFORM Hand-Auswerten
           EXPECT A-WERT TO BE "10"
           EXPECT HoechsteKarte TO BE TRUE

           
           TESTCASE 'Drei unterschiedliche Karten übergeben'
               MOVE "PIK   09" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               MOVE "KREUZ 11" TO KARTE(3)
               PERFORM Hand-Auswerten
           EXPECT A-WERT TO BE "11"
           EXPECT HoechsteKarte TO BE TRUE


           TESTCASE 'Pärchen im Histogramm'
               MOVE 14 TO WERT(1)
               MOVE 10 TO WERT(2)
               MOVE 11 TO WERT(3)
               MOVE 09 TO WERT(4)
               MOVE 11 TO WERT(5)
               PERFORM Bastel-Histogramm
           EXPECT A-WERT TO BE 11
           EXPECT HoechstesPaerchen TO BE TRUE

           TESTCASE 'Überprüfe Histogramm'
               MOVE 11 TO WERT(1)
               MOVE 10 TO WERT(2)
               MOVE 11 TO WERT(3)
               MOVE 09 TO WERT(4)
               MOVE 11 TO WERT(5)
               PERFORM Bastel-Histogramm
           EXPECT ANZAHL(11) TO BE 3
           EXPECT ANZAHL(10) TO BE 1
           EXPECT ANZAHL(09) TO BE 1
           EXPECT ANZAHL(08) TO BE 0

           TESTCASE 'Unterschiedliche Karten im Histogramm'
               MOVE 02 TO WERT(1)
               MOVE 10 TO WERT(2)
               MOVE 14 TO WERT(3)
               MOVE 09 TO WERT(4)
               MOVE 11 TO WERT(5)
               PERFORM Bastel-Histogramm
           EXPECT A-WERT TO BE 14
           EXPECT HoechsteKarte TO BE TRUE
