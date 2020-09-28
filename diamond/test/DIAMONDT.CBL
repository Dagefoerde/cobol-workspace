           TESTSUITE 'Testfälle für unseren Diamanten'

           TESTCASE 'Zeile A für D'
               MOVE "D" TO GOALLETTER
               MOVE 1 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           EXPECT CURRENTLINE TO BE "   A   "

           TESTCASE 'Zeile B für D'
               MOVE "D" TO GOALLETTER
               MOVE 2 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           EXPECT CURRENTLINE TO BE "  B B  "

           TESTCASE 'Zeile D für D'
               MOVE "D" TO GOALLETTER
               MOVE 4 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           EXPECT CURRENTLINE TO BE "D     D"
               
           TESTCASE 'Zeile E für E'
               MOVE "E" TO GOALLETTER
               MOVE 5 TO CURRENTNUMBER
               PERFORM GOALTONUMBER
               PERFORM GETLINE
           EXPECT CURRENTLINE TO BE "E       E"

           TESTCASE 'Buchstabe zu Zahl'
               MOVE "E" TO GOALLETTER
               PERFORM GOALTONUMBER
           EXPECT GOALNUMBER TO BE 05

           TESTCASE 'Zahl zu Buchstabe'
               MOVE 8 TO CURRENTNUMBER
               PERFORM NUMBERTOLETTER
           EXPECT CURRENTLETTER TO BE "H"
