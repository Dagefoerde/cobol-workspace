       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH_M.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TRANSACTION-IN ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL       
           FILE STATUS IS TRANSACTION-IN-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-IN.
       01  TRANSACTION-F.
         05  BANKID-F    PIC 9(5).
         05  ACCOUNTID-F PIC 9(10).
         05  AMOUNT-F    PIC Z(7)9V99.
         05  COMMENT-F   PIC X(55).
       WORKING-STORAGE SECTION.
       01  TRANSACTION.
         05  BANKID      PIC 9(5).
         05  ACCOUNTID   PIC 9(10).
         05  AMOUNT      PIC 9(8)V99.
         05  COMMENT     PIC X(55).
       01  READSTATUS    PIC X VALUE SPACE.
         88  EOF         VALUE "X".
       01  TRANSACTION-IN-FILE-STATUS   PIC X(02).
       01  CURRENTBANK.
         05  CBID      PIC 9(5) VALUE 0.
         05  CBSALDO   PIC 9(8)V99 VALUE 0.
       01  CURRENTACCOUNT.
         05  CAID      PIC 9(10) VALUE 0.
         05  CASALDO   PIC 9(8)V99 VALUE 0.
       PROCEDURE DIVISION.
           OPEN INPUT TRANSACTION-IN
              PERFORM UNTIL EOF 
                 READ TRANSACTION-IN INTO TRANSACTION
                   AT END SET EOF TO TRUE
                   NOT AT END PERFORM PROCESS-TRANSACTION
                 END-READ
              END-PERFORM
           CLOSE TRANSACTION-IN
           PERFORM CLOSE-BANK
           GOBACK
           .
       
       PROCESS-TRANSACTION SECTION.
           IF CBID NOT = BANKID THEN
               PERFORM CLOSE-BANK
               MOVE BANKID TO CBID
           END-IF
           IF CAID NOT = ACCOUNTID THEN
               PERFORM CLOSE-ACCOUNT
               MOVE ACCOUNTID TO CAID
           END-IF


           ADD AMOUNT TO CBSALDO
           ADD AMOUNT TO CASALDO
           .
       
       CLOSE-BANK SECTION.
           IF CBID EQUAL 0 THEN
               EXIT SECTION
           END-IF

           PERFORM CLOSE-ACCOUNT
           DISPLAY "================================"
           DISPLAY "Total EUR for " CBID ": " CBSALDO
           DISPLAY SPACE

           INITIALIZE CURRENTBANK
           .

       CLOSE-ACCOUNT SECTION.
           IF CAID EQUAL 0 THEN
               EXIT SECTION
           END-IF

           DISPLAY "Account " CAID ":  " CASALDO

           INITIALIZE CURRENTACCOUNT
           .