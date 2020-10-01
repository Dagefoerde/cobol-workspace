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
       01  MATRIX.
         05  MROW OCCURS 50.
           07  MCELL OCCURS 50 PIC 9.
             88  ALIVE VALUE 1.
             88  DEAD  VALUE 0.
       01  NEXTMATRIX.
         05  NMROW OCCURS 50.
           07  NMCELL OCCURS 50 PIC 9.
             88  NMALIVE VALUE 1.
             88  NMDEAD  VALUE 0.
       01  INDEXES.
         05  ROWIDX PIC 99.
         05  COLIDX PIC 99.
         05  NEIGHBORROWIDX PIC 99.
         05  NEIGHBORCOLIDX PIC 99.
         05  ROWDIFF PIC 99.
         05  COLDIFF PIC 99.
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
           MOVE ZEROES
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

           PERFORM UNTIL 2=1
               PERFORM Tick
           END-PERFORM

           .
       Init SECTION.
           MOVE ZEROES TO MATRIX
           MOVE ZEROES TO NEXTMATRIX
           INITIALIZE INDEXES


           .
       Tick SECTION.
           MOVE ZEROES TO NEXTMATRIX
           INITIALIZE INDEXES
           PERFORM VARYING ROWIDX FROM 1
               UNTIL ROWIDX > 50
                  PERFORM VARYING COLIDX FROM 1
                      UNTIL COLIDX > 50
                      PERFORM Check-Cell
                  END-PERFORM
           END-PERFORM
           MOVE NEXTMATRIX TO MATRIX


           .
       Check-Cell SECTION.
           PERFORM VARYING ROWDIFF FROM -1
               UNTIL ROWDIFF > 1
               PERFORM VARYING COLDIFF FROM -1
                   UNTIL COLDIFF > 1
                   COMPUTE NEIGHBORROWIDX = ROWIDX + ROWDIFF
                   COMPUTE NEIGHBORCOLIDX = COLIDX + COLDIFF
                   PERFORM Fix-Neighbor-Indexes

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
