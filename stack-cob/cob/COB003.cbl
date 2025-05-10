      ******************************************************************
      * FILE NAME   : COB003                                           *
      * DATE        : 2025-05-05                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : FINAL SALARY CALCULATION WITH TAX RULE           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB003.
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
       77 WS-SAL-BRUTO       PIC  9(6)V9(2)              VALUE ZEROES.
       77 WS-IR              PIC  9(6)V9(2) USAGE COMP-3 VALUE ZEROES.
       77 WS-SAL-LIQ         PIC  9(6)V9(2) USAGE COMP-3 VALUE ZEROES.
      *
       77 WS-SAL-BRUTO-EDIT  PIC ZZZ.ZZ9,99              VALUE ZEROES.
       77 WS-IR-EDIT         PIC ZZZ.ZZ9,99              VALUE ZEROES.
       77 WS-SAL-LIQ-EDIT    PIC ZZZ.ZZ9,99              VALUE ZEROES.
      *
       PROCEDURE DIVISION.
       100-RECEIVE-DATA SECTION.
           ACCEPT WS-SAL-BRUTO.
      *
       200-VALIDATE-DATA SECTION.
           EXIT.
      *
       300-PROCESS-DATA SECTION.
           IF WS-SAL-BRUTO GREATER THAN OR EQUAL TO 1000
              COMPUTE WS-IR = WS-SAL-BRUTO * 0,15
           ELSE
              COMPUTE WS-IR = WS-SAL-BRUTO * 0,10
           END-IF.
      *
           SUBTRACT WS-IR FROM WS-SAL-BRUTO GIVING WS-SAL-LIQ.
      *
           MOVE WS-SAL-BRUTO TO WS-SAL-BRUTO-EDIT.
           MOVE WS-IR        TO WS-IR-EDIT.
           MOVE WS-SAL-LIQ   TO WS-SAL-LIQ-EDIT.
      *
       400-PRINT-RESULTS SECTION.
           DISPLAY "SALARIO BRUTO   = " WS-SAL-BRUTO-EDIT.
           DISPLAY "IR              = " WS-IR-EDIT.
           DISPLAY "SALARIO LIQUIDO = " WS-SAL-LIQ-EDIT.
      *
       500-END-PROGRAM SECTION.
           GOBACK.
      *
       END PROGRAM COB003.

