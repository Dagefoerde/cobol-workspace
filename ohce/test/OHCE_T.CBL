           TESTSUITE 'Test OHCE Kata'

           TESTCASE 'Reversing works (5 char)'
           MOVE 'ABCDE' TO ORIGINAL
           PERFORM DO-REVERSE
           EXPECT REVD TO BE "EDCBA"

           TESTCASE 'Reversing works (1 char)'
           MOVE 'A' TO ORIGINAL
           PERFORM DO-REVERSE
           EXPECT REVD TO BE "A"

           TESTCASE 'Reversing works (emptystring)'
           MOVE '' TO ORIGINAL
           PERFORM DO-REVERSE
           EXPECT REVD TO BE ""

           TESTCASE 'Reversing works with emoji'
           MOVE 'A😢' TO ORIGINAL
           PERFORM DO-REVERSE
           EXPECT REVD TO BE "😢A"

           TESTCASE 'Can discover a palindrome'
           MOVE 'ABBA' TO ORIGINAL
           PERFORM DO-REVERSE
           PERFORM DO-PALINDROME
           EXPECT ISPALINDROME TO BE TRUE

           TESTCASE 'Can discover a notpalindrome'
           MOVE 'ABAB' TO ORIGINAL
           PERFORM DO-REVERSE
           PERFORM DO-PALINDROME
           EXPECT ISPALINDROME TO BE FALSE