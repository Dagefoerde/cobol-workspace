           TESTSUITE 'Beispiel Testsuite'

           TESTCASE 'Beispiel Testcase'
           PERFORM FIZZ
           Display 'Das ist ein Testcase'
           EXPECT CURRENT TO BE 100.

           TESTCASE 'Alternativer Testcase'
           MOVE 20 TO GOAL
           PERFORM FIZZ
           Display 'Das ist ein Testcase'
           EXPECT CURRENT TO BE 20.