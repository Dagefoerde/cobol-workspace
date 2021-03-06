       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH_M.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TRANSACTION-IN ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS TRANSACTION-IN-FILE-STATUS.
       SELECT PROCESSING-OUT ASSIGN TO 'output.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PROCESSING-OUT-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-IN.
       01  TRANSACTION-F.
         05  BANKID-F    PIC 9(5).
         05  ACCOUNTID-F PIC 9(10).
         05  AMOUNT-F    PIC Z(7)9V99.
         05  COMMENT-F   PIC X(55).
       FD PROCESSING-OUT.
       01  PROCESSING-OUT-F.
         05  DESCRIPTION-F PIC X(21).
         05  OUTSALDO-F    PIC Z(08)9.99.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01  TRANSACTION.
         05  BANKID      PIC 9(5).
         05  ACCOUNTID   PIC 9(10).
         05  AMOUNT      PIC 9(8)V99.
         05  COMMENT     PIC X(55).
       01  READSTATUS    PIC X VALUE SPACE.
         88  EOF         VALUE "X".
       01  FILE-STATUS.
         05  TRANSACTION-IN-FILE-STATUS   PIC X(02).
         05  PROCESSING-OUT-FILE-STATUS   PIC X(02).
       01  CURRENTBANK.
         05  CBID      PIC 9(5) VALUE 0.
         05  CBSALDO   PIC 9(8)V99 VALUE 0.
       01  CURRENTACCOUNT.
         05  CAID      PIC 9(10) VALUE 0.
         05  CASALDO   PIC 9(8)V99 VALUE 0.
       01  ACCOUNTDESC PIC X(21) VALUE "Account XXXXXXXXXX:  ".
       01  BANKDESC    PIC X(21) VALUE "Total EUR for XXXXX: ".

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
           MOVE 'Closing a bank resets both bank and account'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           INITIALIZE CURRENTBANK CURRENTACCOUNT
           MOVE 47112 TO CURRENTBANK
           MOVE 1111111111 TO CURRENTACCOUNT
           PERFORM CLOSE-BANK
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CBID TO UT-ACTUAL
           MOVE 00000
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CAID TO UT-ACTUAL
           MOVE 0000000000
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Closing an account leaves bank intact'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           INITIALIZE CURRENTBANK CURRENTACCOUNT
           MOVE 47112 TO CURRENTBANK
           MOVE 1111111111 TO CURRENTACCOUNT
           PERFORM CLOSE-ACCOUNT
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CBID TO UT-ACTUAL
           MOVE 47112
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CAID TO UT-ACTUAL
           MOVE 0000000000
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Processing with a different bank/acc resets amount'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           INITIALIZE CURRENTBANK CURRENTACCOUNT
           MOVE 47112 TO CURRENTBANK
           MOVE 1111111111 TO CURRENTACCOUNT
           MOVE 10000 TO AMOUNT
           MOVE "4711112345678900005000000Testüberweisung"
               TO TRANSACTION
           PERFORM PROCESS-TRANSACTION
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE AMOUNT TO UT-ACTUAL
           MOVE 00050000.00
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Processing identical bank/acc accumulates amount'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           INITIALIZE CURRENTBANK CURRENTACCOUNT
           MOVE 47112 TO CURRENTBANK
           MOVE 1111111111 TO CURRENTACCOUNT
           MOVE 10000 TO CBSALDO
           MOVE 10000 TO CASALDO
           MOVE "4711211111111110005000000Testüberweisung"
               TO TRANSACTION
           PERFORM PROCESS-TRANSACTION
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CBSALDO TO UT-ACTUAL
           MOVE 00060000.00
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CASALDO TO UT-ACTUAL
           MOVE 00060000.00
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Processing identical bank but different acc'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           INITIALIZE CURRENTBANK CURRENTACCOUNT
           MOVE 47112 TO CURRENTBANK
           MOVE 1111111112 TO CURRENTACCOUNT
           MOVE 10000 TO CBSALDO
           MOVE 10000 TO CASALDO
           MOVE "4711211111111110005000000Testüberweisung"
               TO TRANSACTION
           PERFORM PROCESS-TRANSACTION
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CBSALDO TO UT-ACTUAL
           MOVE 00060000.00
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE CASALDO TO UT-ACTUAL
           MOVE 00050000.00
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
           ADD 1 TO UT-FILE-COUNT
           SET UT-FILE-IX TO UT-FILE-COUNT
           MOVE 'PROCESSING-OUT' TO UT-INTERNAL-FILENAME(UT-FILE-IX)
           SET UT-FIND-FILE-MOCK TO TRUE
           MOVE 'TRANSACTION-IN' TO UT-MOCK-FIND-FILENAME
           PERFORM UT-LOOKUP-MOCK
           PERFORM UT-LOOKUP-FILE
           MOVE 'TRANSACTION-F' TO UT-RECORD-FIELD-NAME(UT-FILE-IX)
           SET UT-FIND-FILE-MOCK TO TRUE
           MOVE 'PROCESSING-OUT' TO UT-MOCK-FIND-FILENAME
           PERFORM UT-LOOKUP-MOCK
           PERFORM UT-LOOKUP-FILE
           MOVE 'PROCESSING-OUT-F' TO UT-RECORD-FIELD-NAME(UT-FILE-IX)

           .
       UT-END.

           OPEN OUTPUT PROCESSING-OUT
           OPEN INPUT TRANSACTION-IN
              PERFORM UNTIL EOF
                 READ TRANSACTION-IN INTO TRANSACTION
                   AT END SET EOF TO TRUE
                   NOT AT END PERFORM PROCESS-TRANSACTION
                 END-READ
              END-PERFORM
           CLOSE TRANSACTION-IN
           PERFORM CLOSE-BANK
           CLOSE PROCESSING-OUT
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

           MOVE CBID TO BANKDESC(15:5)
           MOVE BANKDESC TO DESCRIPTION-F
           MOVE CBSALDO TO OUTSALDO-F
           WRITE PROCESSING-OUT-F FROM
               "================================"
           WRITE PROCESSING-OUT-F FROM PROCESSING-OUT-F


           WRITE PROCESSING-OUT-F FROM SPACE

           INITIALIZE CURRENTBANK


           .
       CLOSE-ACCOUNT SECTION.
           IF CAID EQUAL 0 THEN
               EXIT SECTION
           END-IF

           DISPLAY "Account " CAID ":  " CASALDO
           MOVE CAID TO ACCOUNTDESC(9:10)
           MOVE ACCOUNTDESC TO DESCRIPTION-F
           MOVE CASALDO TO OUTSALDO-F
           WRITE PROCESSING-OUT-F FROM PROCESSING-OUT-F

           INITIALIZE CURRENTACCOUNT

           .
