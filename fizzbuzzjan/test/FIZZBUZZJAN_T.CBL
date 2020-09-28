           TESTSUITE 'Fizzbuzz Testsuite'

           TESTCASE 'Goal is default'
           PERFORM FIZZ
           EXPECT CURRENT TO BE 100
           
           TESTCASE 'Goal is 20'
           MOVE 020 TO GOAL
           PERFORM FIZZ
           EXPECT CURRENT TO BE 020.