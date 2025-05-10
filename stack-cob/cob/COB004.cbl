      ******************************************************************
      * FILE NAME   : COB004                                           *
      * DATE        : 2025-05-05                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : TRAVEL COST CALCULATION WITH FUEL RATE           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB004.
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
       77 WS-PASSAGEIRO      PIC X(15)                  VALUE SPACES.
       77 WS-DESTINO         PIC X(10)                  VALUE SPACES.
       77 WS-VLR-COMBUSTIVEL PIC 9(1)V9(2)              VALUE ZEROES.
       77 WS-DISTANCIA       PIC 9(3)      USAGE COMP-3 VALUE ZEROES.
       77 WS-VLR-VIAGEM      PIC 9(4)V9(2) USAGE COMP-3 VALUE ZEROES.
      *
       77 WS-VLR-VIAGEM-EDIT PIC  Z.ZZ9,99 VALUE ZEROES.
      *
       PROCEDURE DIVISION.
       100-RECEIVE-DATA SECTION.
           ACCEPT WS-PASSAGEIRO.
           ACCEPT WS-DESTINO.
           ACCEPT WS-VLR-COMBUSTIVEL.
      *
       200-VALIDATE-DATA SECTION.
           EXIT.
      *
       300-PROCESS-DATA SECTION.
           EVALUATE WS-DESTINO
               WHEN 'PIRACICABA'
                   MOVE 220 TO WS-DISTANCIA
               WHEN 'CAMPINAS'
                   MOVE 180 TO WS-DISTANCIA
               WHEN 'JUNDIAI'
                   MOVE 120 TO WS-DISTANCIA
               WHEN OTHER
                   DISPLAY 'CIDADE ' WS-DESTINO ' NAO ATENDIDA.'
                   PERFORM 500-END-PROGRAM
           END-EVALUATE.
      *
           COMPUTE WS-VLR-VIAGEM =
                       (WS-DISTANCIA * WS-VLR-COMBUSTIVEL / 10) * 1,30
      *
           MOVE WS-VLR-VIAGEM TO WS-VLR-VIAGEM-EDIT.
      *
       400-PRINT-RESULTS SECTION.
           DISPLAY "PASSAGEIRO   = " WS-PASSAGEIRO.
           DISPLAY "DESTINO      = " WS-DESTINO.
           DISPLAY "VALOR VIAGEM = " WS-VLR-VIAGEM-EDIT.
      *
       500-END-PROGRAM SECTION.
           GOBACK.
      *
       END PROGRAM COB004.

