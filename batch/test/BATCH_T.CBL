           TESTSUITE 'Batch Testsuite'

           TESTCASE 'Reading a line leads to sensible values'
           MOVE "4711112345678900005000000Testüberweisung"
               TO TRANSACTION
           EXPECT BANKID TO BE 47111
           EXPECT ACCOUNTID TO BE 1234567890
           EXPECT AMOUNT TO BE 00050000.00
           EXPECT COMMENT TO BE "Testüberweisung".