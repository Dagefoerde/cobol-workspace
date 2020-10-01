       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BASEM.
      *****************************************************************
      * Implementation des TIC TACT TOE Spiels.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
           COPY 'matrix.cpy' REPLACING ==TAG-== BY ====.
           COPY 'matrix.cpy' REPLACING ==TAG-== BY ==NM==.
       01  INDEXES.
         05  ROWIDX PIC 99.
         05  COLIDX PIC 99.
         05  NEIGHBORROWIDX PIC 99.
         05  NEIGHBORCOLIDX PIC 99.
         05  ROWDIFF PIC 99.
         05  COLDIFF PIC 99.
       01 NEIGHBOURS.
         05  NEIGHBOUR PIC X OCCURS 8.
       01  NEIGHBOURSIDX PIC 9.
       01  ALIVES PIC 9.
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Beispiel Testsuite'
           DISPLAY SPACE


           MOVE 'Tick from all 0 results in all 0'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE MATRIX TO UT-ACTUAL
           MOVE SPACES
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Single cell dies'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,5) TO TRUE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF DEAD(5,5)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'dead, genau drei lebende Nachbarn -> alive'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(5,6) TO TRUE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF ALIVE(4,5)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF DEAD(4,4)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'alive, zwei lebende Nachbarn -> alive'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(6,5) TO TRUE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF ALIVE(5,4)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF ALIVE(6,5)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'alive, ein lebender Nachbar -> dead'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF DEAD(5,4)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'alive, drei lebende Nachbarn -> alive'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(5,6) TO TRUE
               SET ALIVE(6,5) TO TRUE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF ALIVE(5,5)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'alive, vier lebende Nachbarn -> dead'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(5,6) TO TRUE
               SET ALIVE(6,5) TO TRUE
               SET ALIVE(4,5) TO TRUE
               PERFORM Tick
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF DEAD(5,5)
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Row indexes wrap at upper bound'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 51 TO NEIGHBORROWIDX
               MOVE 1 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORROWIDX TO UT-ACTUAL
           MOVE 01
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORCOLIDX TO UT-ACTUAL
           MOVE 01
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Col indexes wrap at upper bound'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 1 TO NEIGHBORROWIDX
               MOVE 51 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORROWIDX TO UT-ACTUAL
           MOVE 01
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORCOLIDX TO UT-ACTUAL
           MOVE 01
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Row indexes wrap at lower bound'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 0 TO NEIGHBORROWIDX
               MOVE 1 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORROWIDX TO UT-ACTUAL
           MOVE 50
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORCOLIDX TO UT-ACTUAL
           MOVE 01
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Col indexes wrap at lower bound'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 1 TO NEIGHBORROWIDX
               MOVE 0 TO NEIGHBORCOLIDX
               PERFORM Fix-Neighbor-Indexes
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORROWIDX TO UT-ACTUAL
           MOVE 01
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBORCOLIDX TO UT-ACTUAL
           MOVE 50
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'relevant neighbours are found'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(5,6) TO TRUE
               SET ROWIDX TO 5
               SET COLIDX TO 5
               PERFORM Collect-Neighbours
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBOURS TO UT-ACTUAL
           MOVE "   OO   "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'relevant neighbours are found (2)'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               SET ALIVE(5,4) TO TRUE
               SET ALIVE(5,5) TO TRUE
               SET ALIVE(5,6) TO TRUE
               SET ROWIDX TO 6
               SET COLIDX TO 5
               PERFORM Collect-Neighbours
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE NEIGHBOURS TO UT-ACTUAL
           MOVE "OOO     "
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.
               PERFORM Init

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.


           PERFORM Init

           PERFORM VARYING ROWIDX FROM 1 UNTIL ROWIDX > 50
               ACCEPT MROW(ROWIDX)
               INSPECT MROW(ROWIDX) REPLACING ALL "0" BY " "
               INSPECT MROW(ROWIDX) REPLACING ALL "1" BY "O"
           END-PERFORM

           PERFORM FOREVER
               PERFORM Tick
               PERFORM Ausgabe
               call "CBL_GC_NANOSLEEP" using "500000000" end-call
           END-PERFORM


           .
       Init SECTION.
           MOVE SPACES TO MATRIX
           MOVE SPACES TO NMMATRIX
           INITIALIZE INDEXES


           .
       Tick SECTION.
           MOVE SPACES TO NMMATRIX
           INITIALIZE INDEXES
           PERFORM VARYING ROWIDX FROM 1
               UNTIL ROWIDX > 50
                  PERFORM VARYING COLIDX FROM 1
                      UNTIL COLIDX > 50

                      PERFORM Collect-Neighbours
                      INITIALIZE ALIVES
                      INSPECT NEIGHBOURS TALLYING ALIVES FOR ALL "O"

                      IF DEAD(ROWIDX,COLIDX) THEN
                        IF ALIVES = 3
                          SET NMALIVE(ROWIDX,COLIDX) TO TRUE
                        END-IF
                      ELSE
                        EVALUATE ALIVES
                        WHEN < 2
                          SET NMDEAD(ROWIDX,COLIDX) TO TRUE
                        WHEN > 3
                          SET NMDEAD(ROWIDX,COLIDX) TO TRUE
                        WHEN OTHER
                          SET NMALIVE(ROWIDX,COLIDX) TO TRUE
                        END-EVALUATE
                      END-IF
                  END-PERFORM
           END-PERFORM
           MOVE NMMATRIX TO MATRIX


           .
       Collect-Neighbours SECTION.
           INITIALIZE NEIGHBOURS
           INITIALIZE NEIGHBOURSIDX
           PERFORM VARYING ROWDIFF FROM 0
               UNTIL ROWDIFF > 2
               PERFORM VARYING COLDIFF FROM 0
                   UNTIL COLDIFF > 2
                   COMPUTE NEIGHBORROWIDX = ROWIDX + ROWDIFF - 1
                   COMPUTE NEIGHBORCOLIDX = COLIDX + COLDIFF - 1
                   IF NEIGHBORCOLIDX NOT = COLIDX
                       OR NEIGHBORROWIDX NOT = ROWIDX
                   THEN
                       ADD 1 TO NEIGHBOURSIDX
                       PERFORM Fix-Neighbor-Indexes
                       MOVE MCELL(NEIGHBORROWIDX,NEIGHBORCOLIDX)
                           TO NEIGHBOUR(NEIGHBOURSIDX)
                   END-IF

               END-PERFORM
           END-PERFORM



           .
       Fix-Neighbor-Indexes SECTION.
           EVALUATE NEIGHBORROWIDX
           WHEN 0
               SET NEIGHBORROWIDX TO 50
           WHEN 51
               SET NEIGHBORROWIDX TO 01
           END-EVALUATE

           EVALUATE NEIGHBORCOLIDX
           WHEN 0
               SET NEIGHBORCOLIDX TO 50
           WHEN 51
               SET NEIGHBORCOLIDX TO 01
           END-EVALUATE


           .
       Ausgabe SECTION.
           CALL 'SYSTEM' USING 'clear'
           PERFORM VARYING ROWIDX FROM 1 UNTIL ROWIDX > 50
               DISPLAY MROW(ROWIDX)
           END-PERFORM

           .
