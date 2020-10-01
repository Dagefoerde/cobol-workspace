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
           07  MCELL OCCURS 50 PIC X.
             88  ALIVE VALUE "O".
             88  DEAD  VALUE SPACE.
       01  NEXTMATRIX.
         05  NMROW OCCURS 50.
           07  NMCELL OCCURS 50 PIC X.
             88  NMALIVE VALUE "O".
             88  NMDEAD  VALUE SPACE.
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
       
           PERFORM Init

           PERFORM VARYING ROWIDX FROM 1 UNTIL ROWIDX > 50
               ACCEPT MROW(ROWIDX)
               INSPECT MROW(ROWIDX) REPLACING ALL "0" BY " "
               INSPECT MROW(ROWIDX) REPLACING ALL "1" BY "O"
           END-PERFORM

           PERFORM UNTIL 2=1
               PERFORM Tick
               PERFORM Ausgabe
      *>       Wait
               call "CBL_GC_NANOSLEEP" using "500000000" end-call
           END-PERFORM
           .

       Init SECTION.
           MOVE SPACES TO MATRIX
           MOVE SPACES TO NEXTMATRIX
           INITIALIZE INDEXES
           .

       Tick SECTION.
           MOVE SPACES TO NEXTMATRIX
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
           MOVE NEXTMATRIX TO MATRIX
           .

       Collect-Neighbours SECTION.
           INITIALIZE NEIGHBOURS.
           INITIALIZE NEIGHBOURSIDX.
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