           TESTSUITE 'Beispiel Testsuite'

           BEFORE-EACH
               PERFORM Init.
           END-BEFORE

           TESTCASE 'Tick from all 0 results in all 0'
               MOVE ZEROES TO MATRIX
               PERFORM Tick
           EXPECT MATRIX TO BE EMPTYMATRIX

           TESTCASE 'Single cell dies'
               MOVE ZEROES TO MATRIX
               SET ALIVE IN MCELL(5,5) TO TRUE
               PERFORM Tick
           EXPECT DEAD IN MCELL(5,5) TO BE FALSE
