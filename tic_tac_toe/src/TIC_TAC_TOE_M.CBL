       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TTTM.
      *****************************************************************
      * Implementation des TIC TAC TOE Spiels.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DISPLAYROW PIC 9.
       01  BOARD.
       05  ROWS OCCURS 3.
           07 TICK PIC X VALUE SPACE OCCURS 3.
               88 EMPTY VALUE SPACE.
               88 X VALUE "X".
               88 O VALUE "O".
       01  PUTWHERE PIC 9.
       01  PUTWHROW PIC 9.
       01  PUTWHCOL PIC 9.
       01  PLAYER   PIC X VALUE "X".
       01  COMMAND  PIC X(31) VALUE "X, choose a cell (123|456|789):".
       01  EMPTYFIELDS PIC 9.
       01  GAMESTATE  PIC 9 VALUE 0.
               88 RUNNING VALUE 0.
               88 WINX VALUE 1.
               88 WINO VALUE 2.
               88 BOARDFULL VALUE 3.
       01  CHECKIDX PIC 9.
       PROCEDURE DIVISION.
           MOVE SPACES TO BOARD
           PERFORM UNTIL NOT RUNNING
             PERFORM DOMOVE
             PERFORM DISPLAYBOARD
             
             PERFORM CHECKWIN
             IF RUNNING THEN
               PERFORM CHECKFULL
             END-IF
           END-PERFORM

           EVALUATE TRUE
           WHEN BOARDFULL
               DISPLAY "Everyone loses as the board is full."
           WHEN WINX
               DISPLAY "Player X wins!"
           WHEN WINO
               DISPLAY "Player O wins!"
           END-EVALUATE

           GOBACK.

       DOMOVE SECTION.
           MOVE PLAYER TO COMMAND(1:1)
           PERFORM PUTTICK

           EVALUATE PLAYER
             WHEN "X"   MOVE "O" TO PLAYER
             WHEN OTHER MOVE "X" TO PLAYER
           END-EVALUATE
           .

       PUTTICK SECTION.
           DISPLAY COMMAND
           ACCEPT PUTWHERE
           COMPUTE PUTWHROW = ((PUTWHERE - 1) / 3) + 1
           COMPUTE PUTWHCOL = PUTWHERE - ((PUTWHROW - 1) * 3)
           IF EMPTY(PUTWHROW, PUTWHCOL) THEN
             MOVE PLAYER TO TICK (PUTWHROW, PUTWHCOL)
           ELSE
             DISPLAY "Nooooooo!!!!"
             PERFORM PUTTICK
           END-IF
           .

       CHECKFULL Section.
           INITIALIZE EMPTYFIELDS
           INSPECT BOARD TALLYING EMPTYFIELDS FOR ALL SPACES
           IF EMPTYFIELDS EQUALS 0 THEN
               SET BOARDFULL TO TRUE
           END-IF
           .
       
       CHECKWIN Section.
           EVALUATE TRUE
           WHEN X(1,1) AND X(2,2) AND X(3,3)
             SET WINX TO TRUE
           WHEN X(3,1) AND X(2,2) AND X(1,3)
             SET WINX TO TRUE
           WHEN O(1,1) AND O(2,2) AND O(3,3)
             SET WINO TO TRUE
           WHEN O(3,1) AND O(2,2) AND O(1,3)
             SET WINO TO TRUE
           WHEN OTHER
             PERFORM CHECKWINROWS
           END-EVALUATE
           .
       
       CHECKWINROWS SECTION.
           INITIALIZE CHECKIDX
           PERFORM VARYING CHECKIDX FROM 1 BY 1 UNTIL CHECKIDX > 3
               EVALUATE TRUE
               WHEN X(CHECKIDX, 1) AND X(CHECKIDX, 2) AND X(CHECKIDX, 3)
                   SET WINX TO TRUE
               WHEN X(1, CHECKIDX) AND X(2, CHECKIDX) AND X(3, CHECKIDX)
                   SET WINX TO TRUE
               WHEN O(CHECKIDX, 1) AND O(CHECKIDX, 2) AND O(CHECKIDX, 3)
                   SET WINO TO TRUE
               WHEN O(1, CHECKIDX) AND O(2, CHECKIDX) AND O(3, CHECKIDX)
                   SET WINO TO TRUE
           END-PERFORM
           .

       DISPLAYBOARD SECTION.
           PERFORM VARYING DISPLAYROW FROM 1 BY 1 UNTIL DISPLAYROW > 3
             DISPLAY ROWS(DISPLAYROW)
           END-PERFORM.
           
