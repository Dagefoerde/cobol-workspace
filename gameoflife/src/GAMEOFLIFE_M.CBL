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
                      
                  END-PERFORM
           END-PERFORM
           MOVE NEXTMATRIX TO MATRIX
           .
