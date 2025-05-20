      ******************************************************************
      * FILE NAME   : COB007                                           *
      * DATE        : 2025-05-19                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : SUPERMARKET CHECKOUT SIMULATION                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB007.
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
           10 WS-DESCRICAO     PIC X(30)              VALUE 'X'.
           10 WS-VALOR         PIC 9(04)V9(2)         VALUE ZEROES.
           10 WS-QTD           PIC 9(02)V9(2)         VALUE ZEROES.
      *
       01  WS-ITENS.
           05 WS-PRODUTOS OCCURS 100 TIMES.
             10 WS-ITEM-DESC   PIC X(30)                   VALUE SPACES.
             10 WS-ITEM-VAL    PIC 9(04)V9(2) USAGE COMP-3 VALUE ZEROES.
             10 WS-ITEM-QTD    PIC 9(02)V9(2) USAGE COMP-3 VALUE ZEROES.
      *
       77 WS-QTD-ITENS         PIC 9(03)      USAGE COMP-3 VALUE 1.
       77 WS-CONTADOR          PIC 9(03)      USAGE COMP-3 VALUE ZEROES.
       77 WS-TOTAL             PIC 9(08)V9(2) USAGE COMP-3 VALUE ZEROES.
      *
       01 WS-TOTAL-EDIT        PIC ZZ.ZZZ.ZZ9,99           VALUE ZEROES.
      *
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM 100-RECEIVE-DATA 
              UNTIL WS-DESCRICAO EQUAL SPACES.
           PERFORM 200-VALIDATE-DATA.
           PERFORM 300-PROCESS-DATA WS-QTD-ITENS TIMES.
           PERFORM 400-PRINT-RESULTS.
           PERFORM 600-END-PROGRAM.
       000-MAIN-END. EXIT.

      *
       100-RECEIVE-DATA SECTION.
           ACCEPT WS-DDS-ENTRADA.
      *
           IF (WS-DESCRICAO NOT EQUAL SPACES)
               IF WS-QTD-ITENS EQUAL TO 1000
                   PERFORM 500-ROT-ABEND
               END-IF
      *
               MOVE WS-DESCRICAO TO WS-ITEM-DESC(WS-QTD-ITENS)
               MOVE WS-VALOR     TO WS-ITEM-VAL(WS-QTD-ITENS)
               MOVE WS-QTD       TO WS-ITEM-QTD(WS-QTD-ITENS)
           END-IF.
      *
           ADD 1 TO WS-QTD-ITENS.
      *
       100-RECEIVE-DATA-END. EXIT.
      *
       200-VALIDATE-DATA SECTION.
       200-VALIDATE-DATA-END. EXIT.
      *
       300-PROCESS-DATA SECTION.
           ADD 1 TO WS-CONTADOR.
           COMPUTE WS-TOTAL = WS-TOTAL + (WS-ITEM-VAL(WS-CONTADOR) * 
                                          WS-ITEM-QTD(WS-CONTADOR)).
       300-PROCESS-DATA-END. EXIT.
      *
       400-PRINT-RESULTS SECTION.
           SUBTRACT 1 FROM WS-QTD-ITENS.
           MOVE WS-TOTAL TO WS-TOTAL-EDIT.
      *
           DISPLAY '******************************'
           DISPLAY '********* RESULTADOS *********'
           DISPLAY '******************************'
           DISPLAY 'PRODUTOS COMPRADOS'
           DISPLAY '******************************'
      *
           PERFORM VARYING WS-CONTADOR
              FROM 1 BY 1 UNTIL WS-CONTADOR IS EQUAL TO WS-QTD-ITENS
                 DISPLAY WS-ITEM-DESC(WS-CONTADOR)
           END-PERFORM.
      *
           DISPLAY '******************************'
           DISPLAY 'TOTAL DA COMPRA ' WS-TOTAL-EDIT.
      *
       400-PRINT-RESULTS-END. EXIT.
      *
       500-ROT-ABEND SECTION.
           DISPLAY 'O QUANTITATIVO DE ITENS E MAIOR QUE O SUPORTADO.'
           MOVE 999 TO RETURN-CODE.
      *
           PERFORM 600-END-PROGRAM.
       500-ROT-ABEND-END. EXIT.

       600-END-PROGRAM SECTION.
           GOBACK.
       600-END-PROGRAM-END. EXIT.
      *
       END PROGRAM COB007.
