      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE PRODUTOS - INCLUSAO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0200.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SCMO0200 ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0200.txt"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL
                FILE STATUS    IS WS-FS-CARGAPRD.
      *
           SELECT CARGA-PRD ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRODUTO-CARGA.prn"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL
                FILE STATUS    IS WS-FS-CARGAPRD.
      *
           SELECT PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS RANDOM
                RECORD KEY     IS COD-PRODUTO
                FILE STATUS    IS WS-FS-PRODUTO.
      *
           SELECT TP-PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\TP-PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS RANDOM
                RECORD KEY     IS COD-TIPO
                FILE STATUS    IS WS-FS-TP-PRODUTO.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD SCMO0200.
       01  FD-REG-REPORT                       PIC X(80).
      *
       FD CARGA-PRD.
       01  FD-REGISTRO-CARGA-PRODUTO.
           05 FD-COD-PRODUTO-CARGA             PIC X(13).
           05 FD-DESC-PRODUTO-CARGA            PIC X(50).
           05 FD-TIPO-PRODUTO-CARGA            PIC X(10).
           05 FILLER                           PIC X(02).
      *
       FD PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\Produto.cpy".

       FD TP-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\TpProduto.cpy".
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-PRODUTO.
           05 WS-COD-PRODUTO                   PIC X(13).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
           05 FILLER                           PIC X(02).
      *
       01 WS-REG-TIPO-PRODUTO.
           05 WS-COD-TIPO                      PIC X(10).
           05 WS-DESC-TIPO                     PIC X(50).
      *
       01 WS-LKS-AREA.
           05 WS-LKS-CODIGO-DE-BARRAS.
               10 WS-LKS-CODIGO-PRODUTO        PIC X(012).
               10 WS-LKS-DIGITO-VERIFICADOR    PIC X(001).
           05 WS-LKS-RETORNO                   PIC 9(001).

      *
       01 WS-REPORT-CARGA.
           03 WS-LST-CAB-LINHA.
               05 FILLER   PIC X(80) VALUE ALL "=".
      *
           03 WS-LST-CAB-1.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(70) VALUE
                    "SCMO200 - RELATORIO DA CARGA DE PRODUTOS".
      *
           03 WS-LST-CAB-2.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE "CODIGO PRODUTO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(50) VALUE "DESCRICAO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE "TIPO".
      *
           03 WS-LST-CAB-3.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE ALL "-".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(50) VALUE ALL "-".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE ALL "-".
      *
           03 WS-LST-DET-1.
               05 FILLER                   PIC X(02) VALUE SPACES.
               05 WS-LISTA-COD-PRODUTO     PIC X(15) VALUE SPACES.
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-LISTA-DESC-PRODUTO    PIC X(50) VALUE SPACES.
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-LISTA-COD-TIPO        PIC X(10) VALUE SPACES.
      *
           03 WS-LST-DET-OK.
               05 FILLER                   PIC X(02) VALUE SPACES.
               05 FILLER                   PIC X(72) VALUE
                                       "REGISTRO GRAVADO OK.".
      *
           03 WS-LST-DET-ERRO.
               05 FILLER                   PIC X(02) VALUE SPACES.
               05 FILLER                   PIC X(06) VALUE "ERRO: ".
               05 WS-LISTA-ERRO-REPORT     PIC X(72) VALUE SPACES.
      *
           03 WS-LST-FINAL-0.
               05 FILLER                   PIC X(02) VALUE SPACES.
               05 FILLER                   PIC X(50) VALUE
                                       "NENHUM REGISTRO A LISTAR".
      *
           03 WS-LST-FINAL-GRAVADOS.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 FILLER               PIC X(20) VALUE
                                       "REGISTROS GRAVADOS: ".
               05 WS-LISTA-QTD-GRV     PIC 999 VALUE ZEROS.
      *
           03 WS-LST-FINAL-ERRADOS.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 FILLER               PIC X(20) VALUE
                                       "REGISTROS COM ERRO: ".
               05 WS-LISTA-QTD-ERR     PIC 999 VALUE ZEROS.
      *
           03 WS-LST-FINAL-TOTAL.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 FILLER               PIC X(20) VALUE
                                       "TOTAL DE REGISTROS: ".
               05 WS-LISTA-QTD-TOT     PIC 999 VALUE ZEROS.
      *
       77 WS-FS-CARGAPRD                       PIC X(02).
           88 WS-FS-CARGAPRD-OK                VALUE "00".
      *
       77 WS-FS-PRODUTO                        PIC X(02).
           88 WS-FS-PROD-OK                    VALUE "00".
           88 WS-FS-PROD-NAO-EXISTE            VALUE "35".
      *
       77 WS-FS-TP-PRODUTO                     PIC X(02).
           88 WS-FS-TP-OK                      VALUE "00".
           88 WS-FS-TP-NAO-EXISTE              VALUE "35".
      *
       77 WS-RESPOSTA-TELA                     PIC X(01).
           88 FLAG-SAIR                        VALUE "Q".
           88 FLAG-GRAVAR                      VALUE "S".
      *
       77 WS-MENSAGEM                          PIC X(50) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
       77 WS-VALIDA-PRODUTO                    PIC X(01) VALUE SPACES.
           88 FLAG-PRODUTO-VALIDO              VALUE "S".
       77 WS-ACHOU-TIPO-PRODUTO                PIC X(01) VALUE SPACES.
           88 FLAG-TP-PROD-VALIDO              VALUE "S".
      *
       LINKAGE SECTION.
      *
       01 LK-COM-AREA.
           03 LK-MENSAGEM                      PIC X(20).
      *
       SCREEN SECTION.
      *
       01 SS-CLEAR-SCREEN.
           05 BLANK SCREEN.
      *
       01 SS-INPUT-SCREEN.
           05 LINE 02 COL 05 VALUE "CADASTRO DE PRODUTOS".
           05 LINE 03 COL 05 VALUE "SMCP0200 - Carga de Produtos".
           05 LINE 04 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 06 COL 05 VALUE "CONFIRME A CARGA DO ARQUIVO".
           05 LINE 11 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 12 COL 05 VALUE
                           "<S> para confirmar ou <Q> para Sair. ".
           05 SS-RESPOSTA-TELA REVERSE-VIDEO PIC X(01)
                           USING WS-RESPOSTA-TELA.
           05 LINE 13 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
      *
       01  SS-LINHA-DE-MENSAGEM.
           05 SS-MENSAGEM              PIC X(50) USING WS-MENSAGEM
                                               LINE 14 COL 05.
      *
       01  SS-LIMPA-MENSAGEM.
           05 LINE 14 BLANK LINE.
      *
       PROCEDURE DIVISION USING LK-COM-AREA.
      *
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA THRU P100-FIM.

           PERFORM P300-PROCESSA THRU P300-FIM.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-PROD-OK           TO  TRUE.
           SET WS-FS-TP-OK             TO  TRUE.
           MOVE SPACES                 TO WS-RESPOSTA-TELA.

           PERFORM P105-ABRE-REPORT THRU P105-FIM.

           PERFORM P110-ABRE-CARGA THRU P110-FIM.

           PERFORM P120-ABRE-PRODUTO THRU P120-FIM.

           PERFORM P130-ABRE-TP-PRODUTO THRU P130-FIM.
      *
       P100-FIM.
      *
       P105-ABRE-REPORT.
      *
           OPEN OUTPUT SCMO0200.
      *
       P105-FIM.
      *
       P110-ABRE-CARGA.
      *
           OPEN INPUT CARGA-PRD
      *
           IF NOT WS-FS-CARGAPRD-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO DE CARGA FS: "
                       WS-FS-CARGAPRD    INTO WS-MENSAGEM
               DISPLAY SS-CLEAR-SCREEN
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
               PERFORM P900-FIM
           END-IF.
      *
       P110-FIM.
      *
       P120-ABRE-PRODUTO.
      *
           OPEN I-O PRODUTO

           IF WS-FS-PROD-NAO-EXISTE THEN
               OPEN OUTPUT PRODUTO
           END-IF.
      *
           IF NOT WS-FS-PROD-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO PRODUTO FS: "
                       WS-FS-PRODUTO    INTO WS-MENSAGEM
               DISPLAY SS-CLEAR-SCREEN
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
               PERFORM P900-FIM
           END-IF.
      *
           P120-FIM.
      *
       P130-ABRE-TP-PRODUTO.
      *
           OPEN INPUT TP-PRODUTO

           IF NOT WS-FS-TP-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO TP-PRODUTO FS: "
                       WS-FS-TP-PRODUTO    INTO WS-MENSAGEM
               DISPLAY SS-CLEAR-SCREEN
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
               PERFORM P900-FIM
           END-IF.
      *
       P130-FIM.
      *
       P300-PROCESSA.
      *
           MOVE SPACES             TO WS-COD-PRODUTO.
           MOVE SPACES             TO WS-DESC-PRODUTO.
           MOVE SPACES             TO WS-FK-COD-TIPO.
           MOVE SPACES             TO WS-DESC-TIPO.
           MOVE SPACES             TO WS-RESPOSTA-TELA.
           MOVE SPACES             TO WS-VALIDA-PRODUTO.
           MOVE SPACES             TO WS-ACHOU-TIPO-PRODUTO.
      *
           DISPLAY SS-CLEAR-SCREEN.
           DISPLAY SS-INPUT-SCREEN.
           ACCEPT  SS-INPUT-SCREEN.
      *
           IF FLAG-GRAVAR THEN
               PERFORM P510-INICIALIZA-RELATORIO THRU P510-FIM

               PERFORM P400-PROCESSA-CARGA THRU P400-FIM
                                           UNTIL FLAG-SAIR

               PERFORM P520-FINALIZA-RELATORIO THRU P520-FIM
           END-IF.
      *
       P300-FIM.
      *
       P400-PROCESSA-CARGA.

           READ CARGA-PRD  INTO    WS-REG-PRODUTO
               NOT AT END
                   PERFORM P405-VALIDA-PRODUTO THRU P405-FIM
      *
                   IF FLAG-PRODUTO-VALIDO
                       PERFORM P410-VALIDA-TIPO-PRODUTO THRU P410-FIM
      *
                       IF FLAG-TP-PROD-VALIDO
                           PERFORM P420-GRAVA-PRODUTO THRU P420-FIM
                       END-IF
                   END-IF
               AT END
                   SET FLAG-SAIR       TO  TRUE
           END-READ.
      *
       P400-FIM.
      *
       P405-VALIDA-PRODUTO.
      *
           IF WS-COD-PRODUTO IS EQUAL TO SPACES THEN
               MOVE SPACES                     TO WS-VALIDA-PRODUTO
               MOVE WS-COD-PRODUTO             TO WS-LISTA-COD-PRODUTO
               MOVE WS-DESC-PRODUTO            TO WS-LISTA-DESC-PRODUTO
               MOVE WS-FK-COD-TIPO             TO WS-LISTA-COD-TIPO
               MOVE "CODIGO DE PRODUTO EM BRANCO"
                                               TO WS-LISTA-ERRO-REPORT
               ADD 1                           TO WS-LISTA-QTD-ERR
               ADD 1                           TO WS-LISTA-QTD-TOT

               WRITE FD-REG-REPORT     FROM    WS-LST-DET-1
               WRITE FD-REG-REPORT     FROM    WS-LST-DET-ERRO
           ELSE
               IF WS-COD-PRODUTO IS NUMERIC
                   MOVE WS-COD-PRODUTO TO WS-LKS-CODIGO-DE-BARRAS
                   CALL "SCMP0902" USING WS-LKS-AREA
                   EVALUATE WS-LKS-RETORNO
                       WHEN    0
                           SET FLAG-PRODUTO-VALIDO TO TRUE
                       WHEN    1
                           MOVE SPACES             TO WS-VALIDA-PRODUTO
                           MOVE WS-COD-PRODUTO  TO WS-LISTA-COD-PRODUTO
                           MOVE WS-DESC-PRODUTO TO WS-LISTA-DESC-PRODUTO
                           MOVE WS-FK-COD-TIPO  TO WS-LISTA-COD-TIPO
                           MOVE "CODIGO DE PRODUTO INVALIDO"
                                                TO WS-LISTA-ERRO-REPORT
                           ADD 1                TO WS-LISTA-QTD-ERR
                           ADD 1                TO WS-LISTA-QTD-TOT

                           WRITE FD-REG-REPORT  FROM    WS-LST-DET-1
                           WRITE FD-REG-REPORT  FROM    WS-LST-DET-ERRO
                       WHEN    2
                           MOVE SPACES             TO WS-VALIDA-PRODUTO
                           MOVE WS-COD-PRODUTO  TO WS-LISTA-COD-PRODUTO
                           MOVE WS-DESC-PRODUTO TO WS-LISTA-DESC-PRODUTO
                           MOVE WS-FK-COD-TIPO  TO WS-LISTA-COD-TIPO
                           MOVE "DIGITO VERIFICADOR DO CODIGO DE PRODUTO
      -                         " INVALIDO."    TO WS-LISTA-ERRO-REPORT
                           ADD 1                TO WS-LISTA-QTD-ERR
                           ADD 1                TO WS-LISTA-QTD-TOT

                           WRITE FD-REG-REPORT  FROM    WS-LST-DET-1
                           WRITE FD-REG-REPORT  FROM    WS-LST-DET-ERRO
                   END-EVALUATE
               ELSE
                   SET FLAG-PRODUTO-VALIDO         TO  TRUE
               END-IF
           END-IF.

           IF FLAG-PRODUTO-VALIDO THEN
               MOVE WS-COD-PRODUTO TO COD-PRODUTO
               READ PRODUTO
                   KEY IS COD-PRODUTO
                       INVALID KEY
                           SET FLAG-PRODUTO-VALIDO TO TRUE
                       NOT INVALID KEY
                           MOVE SPACES          TO WS-VALIDA-PRODUTO
                           MOVE WS-COD-PRODUTO  TO WS-LISTA-COD-PRODUTO
                           MOVE WS-DESC-PRODUTO TO WS-LISTA-DESC-PRODUTO
                           MOVE WS-FK-COD-TIPO  TO WS-LISTA-COD-TIPO
                           MOVE "CODIGO DE PRODUTO JA CADASTRADO."
                                                TO WS-LISTA-ERRO-REPORT
                           ADD 1                TO WS-LISTA-QTD-ERR
                           ADD 1                TO WS-LISTA-QTD-TOT

                           WRITE FD-REG-REPORT  FROM    WS-LST-DET-1
                           WRITE FD-REG-REPORT  FROM    WS-LST-DET-ERRO
               END-READ
           END-IF.
      *
       P405-FIM.
      *
       P410-VALIDA-TIPO-PRODUTO.
      *
           MOVE SPACES                     TO WS-ACHOU-TIPO-PRODUTO.
           MOVE WS-FK-COD-TIPO             TO COD-TIPO.

           READ TP-PRODUTO INTO WS-REG-TIPO-PRODUTO
               KEY IS COD-TIPO
                   NOT INVALID KEY
                       SET FLAG-TP-PROD-VALIDO TO TRUE
                   INVALID KEY
                       MOVE WS-COD-PRODUTO      TO WS-LISTA-COD-PRODUTO
                       MOVE WS-DESC-PRODUTO     TO WS-LISTA-DESC-PRODUTO
                       MOVE WS-FK-COD-TIPO      TO WS-LISTA-COD-TIPO
                       MOVE "TIPO DE PRODUTO NAO CADASTRADO."
                                                TO WS-LISTA-ERRO-REPORT
                       ADD 1                TO WS-LISTA-QTD-ERR
                       ADD 1                TO WS-LISTA-QTD-TOT

                       WRITE FD-REG-REPORT  FROM    WS-LST-DET-1
                       WRITE FD-REG-REPORT  FROM    WS-LST-DET-ERRO
           END-READ.
      *
       P410-FIM.
      *
       P420-GRAVA-PRODUTO.
      *
           MOVE WS-COD-PRODUTO                     TO COD-PRODUTO.
           MOVE WS-DESC-PRODUTO                    TO DESC-PRODUTO.
           MOVE WS-FK-COD-TIPO                     TO FK-COD-TIPO.

           WRITE   REG-PRODUTO.

           IF WS-FS-PROD-OK THEN
               MOVE WS-COD-PRODUTO      TO WS-LISTA-COD-PRODUTO
               MOVE WS-DESC-PRODUTO     TO WS-LISTA-DESC-PRODUTO
               MOVE WS-FK-COD-TIPO      TO WS-LISTA-COD-TIPO
               ADD 1                    TO WS-LISTA-QTD-GRV
               ADD 1                    TO WS-LISTA-QTD-TOT

               WRITE FD-REG-REPORT  FROM   WS-LST-DET-1
               WRITE FD-REG-REPORT  FROM   WS-LST-DET-OK
           ELSE
               MOVE WS-COD-PRODUTO      TO WS-LISTA-COD-PRODUTO
               MOVE WS-DESC-PRODUTO     TO WS-LISTA-DESC-PRODUTO
               MOVE WS-FK-COD-TIPO      TO WS-LISTA-COD-TIPO
               STRING "GRAVACAO DE PRODUTO - "
                      "FILE STATUS: " WS-FS-PRODUTO
                                      INTO WS-LISTA-ERRO-REPORT
               ADD 1                    TO WS-LISTA-QTD-ERR
               ADD 1                    TO WS-LISTA-QTD-TOT

               WRITE FD-REG-REPORT  FROM   WS-LST-DET-1
               WRITE FD-REG-REPORT  FROM   WS-LST-DET-ERRO
           END-IF.
      *
       P420-FIM.
      *
       P510-INICIALIZA-RELATORIO.
      *
           MOVE ZEROS                  TO  WS-LISTA-QTD-GRV
                                           WS-LISTA-QTD-ERR
                                           WS-LISTA-QTD-TOT.
      *
           WRITE FD-REG-REPORT    FROM WS-LST-CAB-LINHA.
           WRITE FD-REG-REPORT    FROM WS-LST-CAB-1.
           WRITE FD-REG-REPORT    FROM WS-LST-CAB-LINHA.
           WRITE FD-REG-REPORT    FROM WS-LST-CAB-2.
           WRITE FD-REG-REPORT    FROM WS-LST-CAB-3.
      *
       P510-FIM.
      *
       P520-FINALIZA-RELATORIO.
      *
           IF WS-LISTA-QTD-TOT EQUAL ZEROS THEN
               WRITE FD-REG-REPORT     FROM    WS-LST-FINAL-0
           ELSE
               WRITE FD-REG-REPORT     FROM    WS-LST-FINAL-ERRADOS
               WRITE FD-REG-REPORT     FROM    WS-LST-FINAL-GRAVADOS
               WRITE FD-REG-REPORT     FROM    WS-LST-FINAL-TOTAL
           END-IF.
      *
       P520-FIM.
      *
       P900-FIM.
           CLOSE   TP-PRODUTO
                   PRODUTO
                   CARGA-PRD
                   SCMO0200.
           GOBACK.
       END PROGRAM SCMP0200.
