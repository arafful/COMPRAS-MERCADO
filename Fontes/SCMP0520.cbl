      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 26/03/2024
      * Purpose: CADASTRO DE PRODUTOS - GERA CSV
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0520.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS SEQUENTIAL
                RECORD KEY     IS COD-PRODUTO
                FILE STATUS    IS WS-FS-PRODUTO.
      *
           SELECT SCMO0520     ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0520.csv"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\Produto.cpy".
      *
       FD SCMO0520.
       01 REGISTRO-CSV                         PIC X(75).
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-PRODUTO.
           05 WS-COD-PRODUTO                   PIC X(13).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
      *
       01 WS-REGISTRO-CSV.
           05 WS-CSV-COD-PRODUTO               PIC X(13).
           05 FILLER                           PIC X(01) VALUE ";".
           05 WS-CSV-DESC-PRODUTO              PIC X(50).
           05 FILLER                           PIC X(01) VALUE ";".
           05 WS-CSV-FK-COD-TIPO               PIC X(10).
      *
       77 WS-FS-PRODUTO                        PIC X(02).
           88 WS-FS-OK                         VALUE "00".
           88 WS-FS-NAO-EXISTE                 VALUE "35".
      *
       77 WS-FIM-DE-ARQUIVO                    PIC X(01).
           88 FLAG-EOF                         VALUE "S".
      *
       77 WS-QTD-REGISTROS-GRAVADOS            PIC 999 VALUE ZEROS.
      *
       77 WS-MENSAGEM                          PIC X(60) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
      *
       LINKAGE SECTION.
      *
       01 LK-COM-AREA.
           03 LK-MENSAGEM                      PIC X(20).
      *
       PROCEDURE DIVISION USING LK-COM-AREA.
      *
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA THRU P100-FIM.

           PERFORM P300-LISTA THRU P300-FIM UNTIL FLAG-EOF.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-OK            TO  TRUE.
           MOVE "N"                TO  WS-FIM-DE-ARQUIVO.
           MOVE ZEROS              TO  WS-QTD-REGISTROS-GRAVADOS.

           OPEN INPUT PRODUTO
      *
           IF NOT WS-FS-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO. FS: "
                       WS-FS-PRODUTO AT 1505
               ACCEPT WS-PROMPT AT 1501
               PERFORM P900-FIM
           END-IF.
      *
           OPEN OUTPUT SCMO0520.
      *
       P100-FIM.
      *
       P300-LISTA.
      *
           READ PRODUTO INTO WS-REG-PRODUTO
               AT END
                   MOVE "S"                    TO WS-FIM-DE-ARQUIVO
                   STRING  "ARQUIVO CSV GERADO COM "
                           WS-QTD-REGISTROS-GRAVADOS
                           " REGISTROS."   INTO    WS-MENSAGEM
                   DISPLAY WS-MENSAGEM     AT 1505
                   ACCEPT WS-PROMPT        AT 1501
               NOT AT END
                   ADD 1                   TO WS-QTD-REGISTROS-GRAVADOS
                   MOVE WS-COD-PRODUTO        TO WS-CSV-COD-PRODUTO
                   MOVE WS-DESC-PRODUTO       TO WS-CSV-DESC-PRODUTO
                   MOVE WS-FK-COD-TIPO        TO WS-CSV-FK-COD-TIPO
                   WRITE REGISTRO-CSV      FROM WS-REGISTRO-CSV
           END-READ.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE   PRODUTO
                   SCMO0520.
           GOBACK.
       END PROGRAM SCMP0520.
