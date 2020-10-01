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
               MOVE ZEROES TO MATRIX
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
               MOVE ZEROES TO MATRIX
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

                  END-PERFORM
           END-PERFORM
           MOVE NEXTMATRIX TO MATRIX

           .
