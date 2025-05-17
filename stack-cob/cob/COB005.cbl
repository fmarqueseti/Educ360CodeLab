      ******************************************************************
      * FILE NAME   : COB005                                           *
      * DATE        : 2025-05-12                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CALC FINAL BALANCE AFTER PAYMENTS                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB005.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       77 WS-SALDO-INICIAL     PIC  9(6)V9(2)              VALUE ZEROES.
       77 WS-VALOR-PAGO        PIC  9(6)V9(2)              VALUE 1.
       77 WS-VALORES-PAGOS     PIC  9(7)V9(2) USAGE COMP-3 VALUE ZEROES.
       77 WS-SALDO-FINAL       PIC S9(6)V9(2) USAGE COMP-3 VALUE ZEROES.
      *
       77 WS-SALDO-INICIAL-EDIT PIC   ZZZ.ZZ9,99           VALUE ZEROES.
       77 WS-VALORES-PAGOS-EDIT PIC Z.ZZZ.ZZ9,99           VALUE ZEROES.
       77 WS-SALDO-FINAL-EDIT   PIC   ZZZ.ZZ9,99-          VALUE ZEROES.
      *
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM 100-RECEIVE-DATA.
           PERFORM 300-PROCESS-DATA UNTIL WS-VALOR-PAGO EQUAL 0.
           PERFORM 400-PRINT-RESULTS.
           PERFORM 500-END-PROGRAM.
       000-MAIN-END. EXIT.
      *
       100-RECEIVE-DATA SECTION.
           ACCEPT WS-SALDO-INICIAL.
       100-RECEIVE-DATA-END. EXIT.
      *
       200-VALIDATE-DATA SECTION.
       200-VALIDATE-DATA-END. EXIT.
      *
       300-PROCESS-DATA SECTION.
           ACCEPT WS-VALOR-PAGO.
           ADD WS-VALOR-PAGO TO WS-VALORES-PAGOS.
       300-PROCESS-DATA-END. EXIT.
      *
       400-PRINT-RESULTS SECTION.
           SUBTRACT WS-VALORES-PAGOS FROM WS-SALDO-INICIAL
             GIVING WS-SALDO-FINAL.
      *
           MOVE WS-SALDO-INICIAL TO WS-SALDO-INICIAL-EDIT.
           MOVE WS-VALORES-PAGOS TO WS-VALORES-PAGOS-EDIT.
           MOVE WS-SALDO-FINAL   TO WS-SALDO-FINAL-EDIT.
      *
           DISPLAY "SALDO INICIAL   " WS-SALDO-INICIAL-EDIT.
           DISPLAY "PAGAMENTOS    "   WS-VALORES-PAGOS-EDIT.
           DISPLAY "SALDO FINAL     " WS-SALDO-FINAL-EDIT.
       400-PRINT-RESULTS-END. EXIT.      
      *
       500-END-PROGRAM SECTION.
           GOBACK.
       500-END-PROGRAM-END. EXIT.
      *
       END PROGRAM COB005.

