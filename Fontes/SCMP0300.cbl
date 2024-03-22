      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 22/03/2024
      * Purpose: CADASTRO DE PRECOS DE PRODUTOS - CARGA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0300.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SCMO0300 ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0300.txt"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL
                FILE STATUS    IS WS-FS-CARGAPRC.
      *
           SELECT CARGA-PRC ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRECO-PRODUTO-CARGA.prn"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL
                FILE STATUS    IS WS-FS-CARGAPRC.
      *
           SELECT PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS RANDOM
                RECORD KEY     IS COD-PRODUTO
                FILE STATUS    IS WS-FS-PRODUTO.
      *
           SELECT PRC-PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRC-PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS RANDOM
                RECORD KEY     IS CHAVE-PRECO-PRODUTO
                FILE STATUS    IS WS-FS-PRC-PRODUTO.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD SCMO0300.
       01  FD-REG-REPORT                       PIC X(80).
      *
       FD CARGA-PRC.
       01  FD-REGISTRO-CARGA-PRECO.
           05 FD-COD-PROD-PRECO-CARGA          PIC X(13).
           05 FD-DATA-PRECO-CARGA              PIC X(10).
           05 FD-VALOR-PRECO-CARGA             PIC X(08).
      *
       FD PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\Produto.cpy".

       FD PRC-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\PrcProduto.cpy".
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REGISTRO-CARGA-PRECO.
           05 WS-COD-PROD-PRECO-CARGA          PIC X(13).
           05 WS-DATA-PRECO-CARGA              PIC X(10).
           05 WS-VALOR-PRECO-CARGA             PIC X(08).
      *
       01 WS-VALIDA-DATA-PRECO.
           05 WS-VALIDA-DD-DATA-PRECO-CARGA    PIC X(02).
           05 FILLER                           PIC X(01).
           05 WS-VALIDA-MM-DATA-PRECO-CARGA    PIC X(02).
           05 FILLER                           PIC X(01).
           05 WS-VALIDA-AAAA-DATA-PRECO-CARGA  PIC X(04).
      *
       01 WS-REG-PRODUTO.
           05 WS-COD-PRODUTO                   PIC X(13).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
           05 FILLER                           PIC X(02).
      *
       01 WS-REG-PRECO-PRODUTO.
           05 WS-CHAVE-PRECO-PRODUTO.
               10 WS-FK-COD-PRODUTO            PIC X(13).
               10 WS-DATA-PRECO                PIC X(08).
               10 WS-DATA-PRECO-DDMMAAAA REDEFINES WS-DATA-PRECO.
                   15 WS-ANO-PRECO             PIC 9(04).
                   15 WS-MES-PRECO             PIC 9(02).
                   15 WS-DIA-PRECO             PIC 9(02).
           05 WS-VLR-PRECO                     PIC 9(06)V99.
      *
       01 WS-LKS-AREA-DT.
           05 WS-LKS-DATA.
               10 WS-LKS-DD                    PIC 99.
               10 FILLER                       PIC X VALUE "/".
               10 WS-LKS-MM                    PIC 99.
               10 FILLER                       PIC X VALUE "/".
               10 WS-LKS-AAAA                  PIC 9999.
           05 WS-LKS-RETORNO-DT                PIC 9.
      *
       01 WS-LKS-AREA-CB.
           05 WS-LKS-CODIGO-DE-BARRAS.
               10 WS-LKS-CODIGO-PRODUTO        PIC X(012).
               10 WS-LKS-DIGITO-VERIFICADOR    PIC X(001).
           05 WS-LKS-RETORNO-CB                PIC 9(001).

      *
       01 WS-REPORT-CARGA.
           03 WS-LST-CAB-LINHA.
               05 FILLER   PIC X(80) VALUE ALL "=".
      *
           03 WS-LST-CAB-1.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(70) VALUE
                    "SCMO300 - RELATORIO DA CARGA DE PRODUTOS".
      *
           03 WS-LST-CAB-2.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE "CODIGO PRODUTO".
               05 FILLER   PIC X(03) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE "DATA DA COMPRA".
               05 FILLER   PIC X(03) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE "PRECO".
      *
           03 WS-LST-CAB-3.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE ALL "-".
               05 FILLER   PIC X(03) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE ALL "-".
               05 FILLER   PIC X(03) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE ALL "-".
      *
           03 WS-LST-DET-1.
               05 FILLER                   PIC X(02) VALUE SPACES.
               05 WS-LISTA-COD-PRODUTO     PIC X(15) VALUE SPACES.
               05 FILLER                   PIC X(03) VALUE SPACES.
               05 WS-LISTA-DATA-PRECO      PIC X(15) VALUE SPACES.
               05 FILLER                   PIC X(03) VALUE SPACES.
               05 WS-LISTA-VALOR-PRECO     PIC X(10) VALUE SPACES.
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
       77 WS-FS-CARGAPRC                       PIC X(02).
           88 WS-FS-CARGAPRC-OK                VALUE "00".
      *
       77 WS-FS-PRODUTO                        PIC X(02).
           88 WS-FS-PROD-OK                    VALUE "00".
           88 WS-FS-PROD-NAO-EXISTE            VALUE "35".
      *
       77 WS-FS-PRC-PRODUTO                    PIC X(02).
           88 WS-FS-PRC-OK                     VALUE "00".
           88 WS-FS-PRC-NAO-EXISTE             VALUE "35".
      *
       77 WS-RESPOSTA-TELA                     PIC X(01).
           88 FLAG-SAIR                        VALUE "Q".
           88 FLAG-GRAVAR                      VALUE "S".
      *
       77 WS-MENSAGEM                          PIC X(50) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
       77 WS-VALIDA-REGISTRO                   PIC X(01) VALUE SPACES.
           88 FLAG-REGISTRO-OK                 VALUE "S".
           88 FLAG-REGISTRO-COM-ERRO           VALUE SPACES.
       77 WS-VALIDA-PRODUTO                    PIC X(01) VALUE SPACES.
           88 FLAG-PRODUTO-VALIDO              VALUE "S".
       77 WS-VALIDA-DATA                       PIC X(01) VALUE SPACES.
           88 FLAG-DATA-VALIDA                 VALUE "S".
       77 WS-VALIDA-PRECO-PRODUTO              PIC X(01) VALUE SPACES.
           88 FLAG-VLR-PROD-VALIDO             VALUE "S".
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
           05 LINE 02 COL 05 VALUE "CADASTRO DE PRECOS DE PRODUTOS".
           05 LINE 03 COL 05 VALUE
                               "SMCP0300 - Carga de Precos de Produtos".
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
           SET WS-FS-PRC-OK             TO  TRUE.
           MOVE SPACES                 TO WS-RESPOSTA-TELA.

           PERFORM P105-ABRE-REPORT THRU P105-FIM.

           PERFORM P110-ABRE-CARGA THRU P110-FIM.

           PERFORM P120-ABRE-PRODUTO THRU P120-FIM.

           PERFORM P130-ABRE-PRC-PRODUTO THRU P130-FIM.
      *
       P100-FIM.
      *
       P105-ABRE-REPORT.
      *
           OPEN OUTPUT SCMO0300.
      *
       P105-FIM.
      *
       P110-ABRE-CARGA.
      *
           OPEN INPUT CARGA-PRC
      *
           IF NOT WS-FS-CARGAPRC-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO DE CARGA FS: "
                       WS-FS-CARGAPRC    INTO WS-MENSAGEM
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
           OPEN INPUT PRODUTO

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
       P130-ABRE-PRC-PRODUTO.
      *
           OPEN I-O PRC-PRODUTO

           IF WS-FS-PRC-NAO-EXISTE THEN
               OPEN OUTPUT PRC-PRODUTO
           END-IF.

           IF NOT WS-FS-PRC-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO PRC-PRODUTO FS: "
                       WS-FS-PRC-PRODUTO    INTO WS-MENSAGEM
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
           MOVE SPACES             TO WS-RESPOSTA-TELA.
           MOVE SPACES             TO WS-VALIDA-PRODUTO.
           MOVE SPACES             TO WS-VALIDA-DATA.
           MOVE SPACES             TO WS-VALIDA-PRECO-PRODUTO.
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

           READ CARGA-PRC  INTO    WS-REGISTRO-CARGA-PRECO
               NOT AT END
                   SET FLAG-REGISTRO-OK  TO  TRUE
      *
                   PERFORM P405-VALIDA-PRODUTO         THRU P405-FIM
      *
                   PERFORM P407-VALIDA-DATA-PRECO      THRU P407-FIM
      *
                   PERFORM P410-VALIDA-PRECO-PRODUTO   THRU P410-FIM
      *
                   IF FLAG-REGISTRO-OK
                       PERFORM P420-GRAVA-PRECO        THRU P420-FIM
                   END-IF
      *
                   PERFORM P505-GRAVA-RPT THRU P505-FIM
      *
               AT END
                   SET FLAG-SAIR       TO  TRUE
           END-READ.
      *
       P400-FIM.
      *
       P405-VALIDA-PRODUTO.
      *
           IF WS-COD-PROD-PRECO-CARGA IS EQUAL TO SPACES THEN
               MOVE "CODIGO DE PRODUTO EM BRANCO"
                                               TO WS-LISTA-ERRO-REPORT
      *        *=========<< ERRO >>==========*
               SET FLAG-REGISTRO-COM-ERRO      TO TRUE
               PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *        *=============================*
           ELSE
               IF WS-COD-PROD-PRECO-CARGA IS NUMERIC
                   MOVE WS-COD-PROD-PRECO-CARGA
                                           TO WS-LKS-CODIGO-DE-BARRAS
                   CALL "SCMP0902" USING WS-LKS-AREA-CB
                   EVALUATE WS-LKS-RETORNO-CB
                       WHEN    0
                           SET FLAG-PRODUTO-VALIDO TO TRUE
                       WHEN    1
                           MOVE "CODIGO DE PRODUTO INVALIDO"
                                               TO WS-LISTA-ERRO-REPORT
      *                    *=========<< ERRO >>==========*
                           SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                           PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                    *=============================*
                       WHEN    2
                           MOVE "DIGITO VERIFICADOR DO CODIGO DE PRODUTO
      -                         " INVALIDO."   TO WS-LISTA-ERRO-REPORT
      *                    *=========<< ERRO >>==========*
                           SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                           PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                    *=============================*
                   END-EVALUATE
               ELSE
                   SET FLAG-PRODUTO-VALIDO         TO  TRUE
               END-IF
           END-IF.

           IF FLAG-PRODUTO-VALIDO THEN
               MOVE WS-COD-PROD-PRECO-CARGA TO COD-PRODUTO
               READ PRODUTO
                   KEY IS COD-PRODUTO
                       NOT INVALID KEY
                           SET FLAG-PRODUTO-VALIDO TO TRUE
                       INVALID KEY
                           MOVE "CODIGO DE PRODUTO NAO CADASTRADO."
                                                TO WS-LISTA-ERRO-REPORT
      *                    *=========<< ERRO >>==========*
                           SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                           PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                    *=============================*
               END-READ
           END-IF.
      *
       P405-FIM.
      *
       P407-VALIDA-DATA-PRECO.
      *
           MOVE SPACES                         TO WS-VALIDA-DATA.

           MOVE WS-DATA-PRECO-CARGA            TO WS-VALIDA-DATA-PRECO.

           IF WS-VALIDA-AAAA-DATA-PRECO-CARGA  NOT NUMERIC THEN
               MOVE "ANO INVALIDO."            TO WS-LISTA-ERRO-REPORT
      *        *=========<< ERRO >>==========*
               SET FLAG-REGISTRO-COM-ERRO      TO TRUE
               PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *        *=============================*
           ELSE
               IF WS-VALIDA-MM-DATA-PRECO-CARGA    NOT NUMERIC THEN
                   MOVE "MES INVALIDO."        TO WS-LISTA-ERRO-REPORT
      *            *=========<< ERRO >>==========*
                   SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                   PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *            *=============================*
               ELSE
                   IF WS-VALIDA-DD-DATA-PRECO-CARGA NOT NUMERIC THEN
                       MOVE "DIA INVALIDO."    TO WS-LISTA-ERRO-REPORT
      *                *=========<< ERRO >>==========*
                       SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                       PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                *=============================*
                   ELSE
                       MOVE WS-VALIDA-DATA-PRECO TO WS-LKS-DATA
                       CALL "SCMP0901" USING WS-LKS-AREA-DT
                       EVALUATE WS-LKS-RETORNO-DT
                           WHEN    ZERO
                               SET FLAG-DATA-VALIDA    TO TRUE
                           WHEN    1
                               MOVE "DATA INVALIDA."
                                               TO WS-LISTA-ERRO-REPORT
      *                        *=========<< ERRO >>==========*
                               SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                               PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                        *=============================*
                           WHEN    2
                               MOVE "DATA INVALIDA."
                                               TO WS-LISTA-ERRO-REPORT
      *                        *=========<< ERRO >>==========*
                               SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                               PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                        *=============================*
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF.
      *
       P407-FIM.
      *
       P410-VALIDA-PRECO-PRODUTO.
      *
           IF WS-VALOR-PRECO-CARGA NOT NUMERIC
               MOVE "PRECO DE PRODUTO NAO NUMERICO."
                                               TO WS-LISTA-ERRO-REPORT
      *        *=========<< ERRO >>==========*
               SET FLAG-REGISTRO-COM-ERRO      TO TRUE
               PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *        *=============================*
           ELSE
               MOVE SPACES                 TO WS-VALIDA-PRECO-PRODUTO

               MOVE WS-COD-PROD-PRECO-CARGA
                                           TO FK-COD-PRODUTO
               MOVE WS-VALIDA-DD-DATA-PRECO-CARGA
                                           TO DIA-PRECO

               MOVE WS-VALIDA-MM-DATA-PRECO-CARGA
                                           TO MES-PRECO

               MOVE WS-VALIDA-AAAA-DATA-PRECO-CARGA
                                           TO ANO-PRECO

               READ PRC-PRODUTO
                   KEY IS CHAVE-PRECO-PRODUTO
                       INVALID KEY
                           SET FLAG-VLR-PROD-VALIDO TO TRUE
                       NOT INVALID KEY
                           STRING              "PRECO DE PRODUTO "
                                               "JA CADATRADO PARA "
                                               "ESSA DATA."
                                             INTO WS-LISTA-ERRO-REPORT
      *                    *=========<< ERRO >>==========*
                           SET FLAG-REGISTRO-COM-ERRO      TO TRUE
                           PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *                    *=============================*
               END-READ
           END-IF.
      *
       P410-FIM.
      *
       P420-GRAVA-PRECO.
      *
           MOVE WS-COD-PROD-PRECO-CARGA            TO WS-FK-COD-PRODUTO.
           MOVE WS-VALIDA-DD-DATA-PRECO-CARGA      TO WS-DIA-PRECO.
           MOVE WS-VALIDA-MM-DATA-PRECO-CARGA      TO WS-MES-PRECO.
           MOVE WS-VALIDA-AAAA-DATA-PRECO-CARGA    TO WS-ANO-PRECO.
           MOVE WS-VALOR-PRECO-CARGA               TO WS-VLR-PRECO.

           WRITE   REG-PRECO-PRODUTO   FROM    WS-REG-PRECO-PRODUTO.

           IF NOT WS-FS-PRC-OK THEN
               STRING "GRAVACAO DE PRECO PRODUTO - "
                      "FILE STATUS: " WS-FS-PRC-PRODUTO
                                      INTO WS-LISTA-ERRO-REPORT
      *        *=========<< ERRO >>==========*
               SET FLAG-REGISTRO-COM-ERRO      TO TRUE
               PERFORM P500-GRAVA-RPT-ERRO THRU P500-FIM
      *        *=============================*
           END-IF.
      *
       P420-FIM.
      *
       P500-GRAVA-RPT-ERRO.
      *
           WRITE FD-REG-REPORT  FROM   WS-LST-DET-ERRO.
      *
       P500-FIM.
      *
       P505-GRAVA-RPT.
      *
           MOVE WS-COD-PROD-PRECO-CARGA    TO WS-LISTA-COD-PRODUTO.
           MOVE WS-DATA-PRECO-CARGA        TO WS-LISTA-DATA-PRECO.
           MOVE WS-VALOR-PRECO-CARGA       TO WS-LISTA-VALOR-PRECO.
           ADD 1                           TO WS-LISTA-QTD-TOT.

           IF FLAG-REGISTRO-OK
               ADD 1                       TO WS-LISTA-QTD-GRV
               WRITE FD-REG-REPORT       FROM WS-LST-DET-OK
           ELSE
               ADD 1                       TO WS-LISTA-QTD-ERR
           END-IF.

           WRITE FD-REG-REPORT     FROM    WS-LST-DET-1.
      *
       P505-FIM.
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
           CLOSE   PRC-PRODUTO
                   PRODUTO
                   CARGA-PRC
                   SCMO0300.
           GOBACK.
       END PROGRAM SCMP0300.
