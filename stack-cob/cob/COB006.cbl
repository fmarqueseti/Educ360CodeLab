      ******************************************************************
      * FILE NAME   : COB006                                           *
      * DATE        : 2025-05-19                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CALC FINAL INVESTMENT VALUE AND DISPLAY DETAILS  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB006.
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
       01 WS-DDS-ENTRADA.
           05 WS-CAPITAL       PIC 9(04)V9(2)              VALUE ZEROES.
           05 WS-MESES         PIC 9(02)                   VALUE ZEROES.
           05 WS-TAXA          PIC 9(02)V9(2)              VALUE ZEROES.
      *
       77 WS-FINAL             PIC 9(08)V9(4) USAGE COMP-3 VALUE ZEROES.
      *
       01 WS-DDS-EDIT.
           05 WS-CAPITAL-EDIT  PIC Z.ZZ9,99                VALUE ZEROES.
           05 WS-MESES-EDIT    PIC Z9                      VALUE ZEROES.
           05 WS-TAXA-EDIT     PIC Z9,99                   VALUE ZEROES.
           05 WS-FINAL-EDIT    PIC ZZ.ZZZ.ZZ9,99           VALUE ZEROES.
      *
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM 100-RECEIVE-DATA.
           PERFORM 200-VALIDATE-DATA.
           PERFORM 300-PROCESS-DATA.
           PERFORM 400-PRINT-RESULTS.
           PERFORM 500-END-PROGRAM.
       000-MAIN-END. EXIT.
      *
       100-RECEIVE-DATA SECTION.
           ACCEPT WS-DDS-ENTRADA.
       100-RECEIVE-DATA-END. EXIT.
      *
       200-VALIDATE-DATA SECTION.
       200-VALIDATE-DATA-END. EXIT.
      *
       300-PROCESS-DATA SECTION.
      *                   M = C (1 + i)^t
           COMPUTE WS-FINAL = WS-CAPITAL * (1 + WS-TAXA / 100) ** 3.
       300-PROCESS-DATA-END. EXIT.
      *
       400-PRINT-RESULTS SECTION.
           MOVE WS-CAPITAL TO WS-CAPITAL-EDIT.
           MOVE WS-MESES   TO WS-MESES-EDIT.
           MOVE WS-TAXA    TO WS-TAXA-EDIT.
           MOVE WS-FINAL   TO WS-FINAL-EDIT.
      *
           DISPLAY 'CAPITAL           '       WS-CAPITAL-EDIT.
           DISPLAY 'MESES                   ' WS-MESES-EDIT.
           DISPLAY 'TAXA                 '    WS-TAXA-EDIT.
           DISPLAY 'CAPITALIZADO '            WS-FINAL-EDIT.
       400-PRINT-RESULTS-END. EXIT.
      *
       500-END-PROGRAM SECTION.
           GOBACK.
       500-END-PROGRAM-END. EXIT.
      *
       END PROGRAM COB006.


