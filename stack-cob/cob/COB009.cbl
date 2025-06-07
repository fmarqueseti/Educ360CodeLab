      ******************************************************************
      * FILE NAME   : COB009                                           *
      * DATE        : 2025-06-03                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : RECORDING A RECORD ON A SEQUENTIAL FILE          *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB009.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-OUT  ASSIGN       TO SEQOUT
                           ORGANIZATION IS SEQUENTIAL
                           ACCESS MODE  IS SEQUENTIAL
                           FILE STATUS  IS WS-FILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-OUT.
       COPY 'BOOKS'.
      *
       WORKING-STORAGE SECTION.
       77  WS-FILE-STATUS           PIC 9(02).
       77  WS-REC-WRITE             PIC 9(03) USAGE COMP-3 VALUE ZEROES.
       77  WS-STATUS-READ           PIC X(10)              VALUE SPACES.
      *
       01  WS-INPUT-DATA.        *> LRECL 90
           05 WS-LINE01             PIC X(65).
           05 REDEFINES WS-LINE01.
               10 WS-ID             PIC X(05).
               10 WS-TITULO         PIC X(30).
               10 WS-AUTOR          PIC X(30).
           05 WS-LINE02             PIC X(24).
           05 REDEFINES WS-LINE02.
               10 WS-ANO-PUBL       PIC 9(04).
               10 WS-GENERO         PIC X(20).
               10 WS-STATUS         PIC X(01).
      *
       01  WS-ABEND.
           05 WS-ABEND-CODE         PIC 9(02)              VALUE ZEROES.
           05 WS-ABEND-MESSAGE      PIC X(30)              VALUE SPACES.
      *
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM 100-OPEN-DATA.
           PERFORM 200-VALIDATE-DATA.
           PERFORM 300-PROCESS-DATA UNTIL WS-ID EQUAL SPACES.
           PERFORM 400-PRINT-RESULTS.
           PERFORM 500-CLOSE-DATA.
           PERFORM 700-END-PROGRAM.
       000-MAIN-END. EXIT.
      *
       100-OPEN-DATA SECTION.
           OPEN OUTPUT SEQ-OUT.
           IF WS-FILE-STATUS NOT EQUAL 00
               MOVE WS-FILE-STATUS          TO WS-ABEND-CODE
               MOVE "ERRO AO ABRIR ARQUIVO" TO WS-ABEND-MESSAGE
               PERFORM 600-ROT-ABEND
           END-IF.
      *
           ACCEPT WS-LINE01.
           ACCEPT WS-LINE02.
      *
       100-OPEN-DATA-END. EXIT.
      *
       200-VALIDATE-DATA SECTION.
       200-VALIDATE-DATA-END. EXIT.
      *
       300-PROCESS-DATA SECTION.
           MOVE WS-INPUT-DATA TO SEQ-RECORD.
           WRITE SEQ-RECORD.
           ADD 1 TO WS-REC-WRITE.
      *
           ACCEPT WS-LINE01.
           ACCEPT WS-LINE02.
      *
       300-PROCESS-DATA-END. EXIT.
      *
       400-PRINT-RESULTS SECTION.
           DISPLAY ' '.
           DISPLAY '*******************************'.
           DISPLAY '********* ESTATISTICA *********'.
           DISPLAY '*******************************'.
           DISPLAY ' REGISTROS GRAVADOS ' WS-REC-WRITE '       *'.
           DISPLAY '*******************************'.
      *
       400-PRINT-RESULTS-END. EXIT.
      *
       500-CLOSE-DATA SECTION.
           CLOSE SEQ-OUT.
      *
           IF WS-FILE-STATUS NOT EQUAL 00
               MOVE WS-FILE-STATUS             TO WS-ABEND-CODE
               MOVE "ERRO AO FECHAR O ARQUIVO" TO WS-ABEND-MESSAGE
               PERFORM 600-ROT-ABEND
           END-IF.
      *
       500-CLOSE-DATA-END. EXIT.
      *
       600-ROT-ABEND SECTION.
           DISPLAY '****************************************'.
           DISPLAY '***************** ABEND ****************'.
           DISPLAY '****************************************'.
           DISPLAY '* CODIGO: ' WS-ABEND-CODE
                   '                           *'.
           DISPLAY '* MSG: ' WS-ABEND-MESSAGE '  *'.
           DISPLAY '****************************************'.
           MOVE WS-ABEND-CODE TO RETURN-CODE.
      *
           PERFORM 700-END-PROGRAM.
       600-ROT-ABEND-END. EXIT.

       700-END-PROGRAM SECTION.
           GOBACK.
       700-END-PROGRAM-END. EXIT.
      *
       END PROGRAM COB009.

