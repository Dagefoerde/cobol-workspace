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
            COPY ZUTZCWS     .
       01  TRANSACTION.
         05  BANKID      PIC 9(5).
         05  ACCOUNTID   PIC 9(10).
         05  AMOUNT      PIC Z(7)9V99.
         05  COMMENT     PIC X(55).
       01  READSTATUS    PIC X VALUE SPACE.
         88  EOF         VALUE "X".
       01  TRANSACTION-IN-FILE-STATUS   PIC X(02).
       01  CURRENTBANK.
         05  CBID      PIC 9(5).
         05  CBSALDO   PIC Z(7)9V99.
       01  CURRENTACCOUNT.
         05  CAID      PIC 9(10).
         05  CASALDO   PIC Z(7)9V99.
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Batch Testsuite'
           DISPLAY SPACE

           MOVE 'Reading a line leads to sensible values'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE "4711112345678900005000000Testüberweisung"
               TO TRANSACTION
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE BANKID TO UT-ACTUAL
           MOVE 47111
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ACCOUNTID TO UT-ACTUAL
           MOVE 1234567890
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE AMOUNT TO UT-ACTUAL
           MOVE 00050000.00
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE COMMENT TO UT-ACTUAL
           MOVE "Testüberweisung"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION
           ADD 1 TO UT-FILE-COUNT
           SET UT-FILE-IX TO UT-FILE-COUNT
           MOVE 'TRANSACTION-IN' TO UT-INTERNAL-FILENAME(UT-FILE-IX)
           MOVE 'TRANSACTION-IN-FILE-STATUS' TO
                      UT-FILE-STATUS-FIELD-NAME(UT-FILE-IX)
           SET UT-FIND-FILE-MOCK TO TRUE
           MOVE 'TRANSACTION-IN' TO UT-MOCK-FIND-FILENAME
           PERFORM UT-LOOKUP-MOCK
           PERFORM UT-LOOKUP-FILE
           MOVE 'TRANSACTION-F' TO UT-RECORD-FIELD-NAME(UT-FILE-IX)

           .
       UT-END.

           OPEN INPUT TRANSACTION-IN
              PERFORM UNTIL EOF
                 READ TRANSACTION-IN INTO TRANSACTION
                   AT END SET EOF TO TRUE
                   NOT AT END PERFORM PROCESS-TRANSACTION
                 END-READ
              END-PERFORM
           CLOSE TRANSACTION-IN
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
           IF CBID EQUAL SPACE THEN
               EXIT SECTION
           END-IF

           PERFORM CLOSE-ACCOUNT
           DISPLAY "================================="
           DISPLAY "Total EUR for " CBID ": " CBSALDO

           INITIALIZE CBID CBSALDO


           .
       CLOSE-ACCOUNT SECTION.
           IF CAID EQUAL SPACE THEN
               EXIT SECTION
           END-IF

           DISPLAY "Account " CAID ": " CASALDO

           INITIALIZE CAID CASALDO

           .
