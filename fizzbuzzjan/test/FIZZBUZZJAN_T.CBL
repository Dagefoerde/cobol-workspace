           TESTSUITE 'Fizzbuzz Testsuite'

           TESTCASE 'Goal is default'
           PERFORM FIZZ
           EXPECT CURRENT TO BE 100
           
           TESTCASE 'Goal is 20'
           MOVE 020 TO GOAL
           PERFORM FIZZ
           EXPECT CURRENT TO BE 020.
           
           TESTCASE '001 is literal'
           MOVE 001 TO CURRENT
           PERFORM CALC
           EXPECT CALC-RESULT TO BE '001'.
           
           TESTCASE '003 is fizz'
           MOVE 003 TO CURRENT
           PERFORM CALC
           EXPECT CALC-RESULT TO BE 'FIZZ'.
           
           TESTCASE '005 is buzz'
           MOVE 005 TO CURRENT
           PERFORM CALC
           EXPECT CALC-RESULT TO BE 'BUZZ'.
           
           TESTCASE '090 is fizzbuzz'
           MOVE 090 TO CURRENT
           PERFORM CALC
           EXPECT CALC-RESULT TO BE 'FIZZBUZZ'.