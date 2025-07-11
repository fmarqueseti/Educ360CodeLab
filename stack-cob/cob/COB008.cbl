      ******************************************************************
      * FILE NAME   : COB008                                           *
      * DATE        : 2025-05-26                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : READ AND PRINT SEQUENTIAL FILE RECORDS           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB008.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-IN   ASSIGN TO SEQIN
                           ORGANIZATION IS SEQUENTIAL
                           ACCESS MODE IS SEQUENTIAL
                           FILE STATUS IS WS-FILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-IN
             RECORDING MODE IS F.
       COPY 'BOOKS'.
      *
       WORKING-STORAGE SECTION.
       77  WS-FILE-STATUS            PIC 9(02).
       77  WS-REC-READ               PIC 9(03) VALUE ZEROES.
       77  WS-STATUS-READ            PIC X(10) VALUE SPACES.
      *
       01  WS-ABEND.
           05 WS-ABEND-CODE          PIC 9(02) VALUE ZEROES.
           05 WS-ABEND-MESSAGE       PIC X(30) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM 100-OPEN-DATA.
           PERFORM 200-VALIDATE-DATA.
           PERFORM 300-PROCESS-DATA UNTIL WS-FILE-STATUS EQUAL 10.
           PERFORM 400-PRINT-RESULTS.
           PERFORM 500-CLOSE-DATA.
           PERFORM 700-END-PROGRAM.
       000-MAIN-END. EXIT.
      *
       100-OPEN-DATA SECTION.
           OPEN INPUT SEQ-IN.
           IF WS-FILE-STATUS NOT EQUAL 00
               MOVE WS-FILE-STATUS          TO WS-ABEND-CODE
               MOVE "ERRO AO ABRIR ARQUIVO" TO WS-ABEND-MESSAGE
               PERFORM 600-ROT-ABEND
           END-IF.
           READ SEQ-IN.
      *
       100-OPEN-DATA-END. EXIT.
      *
       200-VALIDATE-DATA SECTION.
       200-VALIDATE-DATA-END. EXIT.
      *
       300-PROCESS-DATA SECTION.
           EVALUATE REC-STATUS
               WHEN 'L'   MOVE "LIDO"         TO WS-STATUS-READ
               WHEN 'N'   MOVE "NAO LIDO"     TO WS-STATUS-READ
               WHEN 'E'   MOVE "LENDO"        TO WS-STATUS-READ
               WHEN OTHER MOVE "DESCONHECIDO" TO WS-STATUS-READ
           END-EVALUATE.
      *
           IF WS-REC-READ EQUAL ZEROES
               DISPLAY '*************************'
                       '*************************'
               DISPLAY '********* LISTAGEM DO CAD'
                       'ASTRO DE LIVROS *********'
               DISPLAY '*************************'
                       '*************************'
           END-IF.
      *
           DISPLAY ' REGISTRO...: ' REC-ID.
           DISPLAY ' TITULO.....: ' REC-TITULO.
           DISPLAY ' AUTOR......: ' REC-AUTOR.
           DISPLAY ' ANO PUBLIC.: ' REC-ANO-PUBL.
           DISPLAY ' GENERO.....: ' REC-GENERO.
           DISPLAY ' STATUS.....: ' WS-STATUS-READ.
           DISPLAY '**************************************************'.
           ADD 1 TO WS-REC-READ.
      *
           READ SEQ-IN.
      *
       300-PROCESS-DATA-END. EXIT.
      *
       400-PRINT-RESULTS SECTION.
           DISPLAY ' '.
           DISPLAY '*******************************'.
           DISPLAY '********* ESTATISTICA *********'.
           DISPLAY '*******************************'.
           DISPLAY ' REGISTROS LIDOS ' WS-REC-READ '          *'.
           DISPLAY '*******************************'.
      *
       400-PRINT-RESULTS-END. EXIT.
      *
       500-CLOSE-DATA SECTION.
           CLOSE SEQ-IN.
       500-CLOSE-DATA-END. EXIT.
      *
       600-ROT-ABEND SECTION.
           DISPLAY '****************************************'.
           DISPLAY '***************** ABEND ****************'.
           DISPLAY '****************************************'.
           DISPLAY ' CODIGO: ' WS-ABEND-CODE
                   '                            *'.
           DISPLAY ' MSG: ' WS-ABEND-MESSAGE '   *'.
           DISPLAY '****************************************'.
           MOVE WS-ABEND-CODE TO RETURN-CODE.
      *
           PERFORM 700-END-PROGRAM.
       600-ROT-ABEND-END. EXIT.

       700-END-PROGRAM SECTION.
           GOBACK.
       700-END-PROGRAM-END. EXIT.
      *
       END PROGRAM COB008.

