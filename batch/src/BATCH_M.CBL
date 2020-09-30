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
       01  TRANSACTION.
        05 BANKID       PIC 9(5).
        05 ACCOUNTID    PIC 9(10).
        05 AMOUNT       PIC Z(7)9V99.
        05 COMMENT      PIC X(55).
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'
           STOP RUN.