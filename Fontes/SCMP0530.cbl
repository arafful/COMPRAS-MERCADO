      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 26/03/2024
      * Purpose: CADASTRO DE PREÇOS DE PRODUTOS - GERA CSV
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0530.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRC-PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRC-PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS SEQUENTIAL
                RECORD KEY     IS CHAVE-PRECO-PRODUTO
                FILE STATUS    IS WS-FS-PRC-PRODUTO.
      *
           SELECT SCMO0530     ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0530.csv"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD PRC-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\PrcProduto.cpy".
      *
       FD SCMO0530.
       01 REGISTRO-CSV                         PIC X(35).
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-PRC-PRODUTO.
           05 WS-CHAVE-PRECO-PRODUTO.
               10 WS-FK-COD-PRODUTO        PIC X(13).
               10 WS-DATA-PRECO            PIC X(08).
               10 WS-DATA-PRECO-DDMMAAAA REDEFINES WS-DATA-PRECO.
                   15 WS-ANO-PRECO         PIC 9(04).
                   15 WS-MES-PRECO         PIC 9(02).
                   15 WS-DIA-PRECO         PIC 9(02).
           05 WS-VLR-PRECO                 PIC 9(06)V99.
      *
       01 WS-REGISTRO-CSV.
           05 WS-CSV-FK-COD-PRODUTO        PIC X(13).
           05 FILLER                       PIC X(01) VALUE ";".
           05 WS-CSV-DATA-PRECO            PIC X(10).
           05 FILLER                       PIC X(01) VALUE ";".
           05 WS-CSV-VLR-PRECO             PIC Z(06).99.
      *
       77 WS-FS-PRC-PRODUTO                PIC X(02).
           88 WS-FS-OK                     VALUE "00".
           88 WS-FS-NAO-EXISTE             VALUE "35".
      *
       77 WS-FIM-DE-ARQUIVO                PIC X(01).
           88 FLAG-EOF                     VALUE "S".
      *
       77 WS-QTD-REGISTROS-GRAVADOS        PIC 999 VALUE ZEROS.
      *
       77 WS-MENSAGEM                      PIC X(60) VALUE SPACES.
       77 WS-PROMPT                        PIC X(01) VALUE SPACES.
      *
       LINKAGE SECTION.
      *
       01 LK-COM-AREA.
           03 LK-MENSAGEM                  PIC X(20).
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

           OPEN INPUT PRC-PRODUTO
      *
           IF NOT WS-FS-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO. FS: "
                       WS-FS-PRC-PRODUTO AT 1505
               ACCEPT WS-PROMPT AT 1501
               PERFORM P900-FIM
           END-IF.
      *
           OPEN OUTPUT SCMO0530.
      *
       P100-FIM.
      *
       P300-LISTA.
      *
           READ PRC-PRODUTO INTO WS-REG-PRC-PRODUTO
               AT END
                   MOVE "S"                    TO WS-FIM-DE-ARQUIVO
                   STRING  "ARQUIVO CSV GERADO COM "
                           WS-QTD-REGISTROS-GRAVADOS
                           " REGISTROS."   INTO    WS-MENSAGEM
                   DISPLAY WS-MENSAGEM     AT 1505
                   ACCEPT WS-PROMPT        AT 1501
               NOT AT END
                   ADD 1                   TO WS-QTD-REGISTROS-GRAVADOS
                   MOVE WS-FK-COD-PRODUTO  TO WS-CSV-FK-COD-PRODUTO
                   MOVE WS-VLR-PRECO       TO WS-CSV-VLR-PRECO
                   STRING  WS-DIA-PRECO "/"
                           WS-MES-PRECO "/"
                           WS-ANO-PRECO    INTO    WS-CSV-DATA-PRECO
                   WRITE REGISTRO-CSV      FROM WS-REGISTRO-CSV
           END-READ.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE   PRC-PRODUTO
                   SCMO0530.
           GOBACK.
       END PROGRAM SCMP0530.
