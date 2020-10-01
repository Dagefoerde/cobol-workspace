           TESTSUITE 'Beispiel Testsuite'

           BEFORE-EACH
               PERFORM Init.
           END-BEFORE

           TESTCASE 'Tick from all 0 results in all 0'
               PERFORM Tick
           EXPECT MATRIX TO BE ZEROES

           TESTCASE 'Single cell dies'
               SET ALIVE(5,5) TO TRUE
               PERFORM Tick
           EXPECT DEAD(5,5) TO BE TRUE

           TESTCASE 'dead, genau drei lebende Nachbarn -> alive'
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(5,6) TO TRUE
               PERFORM Tick
           EXPECT ALIVE(4,5) TO BE TRUE
           EXPECT ALIVE(6,5) TO BE TRUE
           EXPECT DEAD(4,4) TO BE TRUE
           EXPECT DEAD(6,4) TO BE TRUE

           TESTCASE 'Row indexes wrap at upper bound'
               MOVE 51 TO NEIGHBORROWIDX
               MOVE 1 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           EXPECT NEIGHBORROWIDX TO BE 01
           EXPECT NEIGHBORCOLIDX TO BE 01

           TESTCASE 'Col indexes wrap at upper bound'
               MOVE 1 TO NEIGHBORROWIDX
               MOVE 51 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           EXPECT NEIGHBORROWIDX TO BE 01
           EXPECT NEIGHBORCOLIDX TO BE 01

           TESTCASE 'Row indexes wrap at lower bound'
               MOVE 0 TO NEIGHBORROWIDX
               MOVE 1 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           EXPECT NEIGHBORROWIDX TO BE 50
           EXPECT NEIGHBORCOLIDX TO BE 01

           TESTCASE 'Col indexes wrap at lower bound'
               MOVE 1 TO NEIGHBORROWIDX
               MOVE 0 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           EXPECT NEIGHBORROWIDX TO BE 01
           EXPECT NEIGHBORCOLIDX TO BE 50