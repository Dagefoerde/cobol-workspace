           TESTSUITE 'Beispiel Testsuite'

           BEFORE-EACH
               PERFORM Init.
           END-BEFORE

           TESTCASE 'Tick from all 0 results in all 0'
               MOVE ZEROES TO MATRIX
               PERFORM Tick
           EXPECT MATRIX TO BE ZEROES

           TESTCASE 'Single cell dies'
               MOVE ZEROES TO MATRIX
               SET ALIVE(5,5) TO TRUE
               PERFORM Tick
           EXPECT DEAD(5,5) TO BE TRUE
