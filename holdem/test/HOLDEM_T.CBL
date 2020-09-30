           TESTSUITE 'Beispiel Testsuite'

              
           BEFORE-EACH
               INITIALIZE AUSGABE
           END-BEFORE

           TESTCASE 'Leere Hand'
               MOVE SPACE TO HAND
               PERFORM Hand-Auswerten
           EXPECT AUSGABE TO BE "Leere Hand!"

           TESTCASE 'Eine Karte übergeben'
               MOVE "KREUZ 10" TO KARTE(1)
               PERFORM Hand-Auswerten
           EXPECT A-WERT TO BE "10"
           EXPECT HoechsteKarte TO BE TRUE

           TESTCASE 'Zwei gleiche Karten übergeben'
               MOVE "PIK   10" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               PERFORM Hand-Auswerten
           EXPECT AUSGABE TO BE "Höchstes Pärchen: 10"

           TESTCASE 'Zwei unterschiedliche Karten übergeben'
               MOVE "PIK   9" TO KARTE(1)
               MOVE "KREUZ 10" TO KARTE(2)
               PERFORM Hand-Auswerten
           EXPECT A-WERT TO BE "10"
           EXPECT HoechsteKarte TO BE TRUE