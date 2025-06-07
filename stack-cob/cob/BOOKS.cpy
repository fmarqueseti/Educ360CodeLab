      ******************************************************************
      * FILE NAME   : BOOKS                                            *
      * DATE        : 2025-06-03                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : BOOKS RECORD COPY BOOK                           *
      * LRECL       : 100                                              *
      ******************************************************************
       01 SEQ-RECORD.
           05 REC-ID                 PIC X(05).
           05 REC-TITULO             PIC X(30).
           05 REC-AUTOR              PIC X(30).
           05 REC-ANO-PUBL           PIC 9(04).
           05 REC-GENERO             PIC X(20).
           05 REC-STATUS             PIC X(01).
           05 FILLER                 PIC X(10).

