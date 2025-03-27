      ******************************************************************
      * FILE NAME   : EXERC010                                         *
      * DATE        : 2025-03-22                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CALC FINAL INVESTMENT VALUE AND DISPLAY DETAILS  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERC010.
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
       01 WS-CAPITAL       PIC 9(04)V9(2)    VALUE ZEROES.
       01 WS-MESES         PIC 9(02)         VALUE ZEROES.
       01 WS-TAXA          PIC 9(02)V9(2)    VALUE ZEROES.
       01 WS-FINAL         PIC 9(08)V9(4)    VALUE ZEROES.
      *
       01 WS-I             PIC 9(02)         VALUE ZEROES.
       01 WS-CAPITAL-EDIT  PIC Z.ZZ9,99      VALUE ZEROES.
       01 WS-MESES-EDIT    PIC Z9            VALUE ZEROES.
       01 WS-TAXA-EDIT     PIC Z9,99         VALUE ZEROES.
       01 WS-FINAL-EDIT    PIC ZZ.ZZZ.ZZ9,99 VALUE ZEROES.
      *
       PROCEDURE DIVISION.
           ACCEPT WS-CAPITAL.
           ACCEPT WS-MESES.
           ACCEPT WS-TAXA.
      *
           MOVE WS-CAPITAL TO WS-FINAL.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MESES
              COMPUTE WS-FINAL = WS-FINAL + (WS-FINAL * WS-TAXA / 100)
           END-PERFORM.
      *
           MOVE WS-CAPITAL TO WS-CAPITAL-EDIT.
           MOVE WS-MESES   TO WS-MESES-EDIT.
           MOVE WS-TAXA    TO WS-TAXA-EDIT.
           MOVE WS-FINAL   TO WS-FINAL-EDIT.
      *
           DISPLAY 'CAPITAL      ' WS-CAPITAL-EDIT.
           DISPLAY 'MESES        ' WS-MESES-EDIT.
           DISPLAY 'TAXA         ' WS-TAXA-EDIT.
           DISPLAY 'CAPITALIZADO ' WS-FINAL-EDIT.
      *
           GOBACK.
