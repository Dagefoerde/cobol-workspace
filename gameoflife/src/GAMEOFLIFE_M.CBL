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
       
           PERFORM Init.

           PERFORM UNTIL 2=1
               PERFORM Tick.
           END-PERFORM

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
                      PERFORM Check-Cell.
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