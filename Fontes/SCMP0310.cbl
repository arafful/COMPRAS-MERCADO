      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE PRODUTOS - INCLUSAO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0310.
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
                ACCESS         IS RANDOM
                RECORD KEY     IS CHAVE-PRECO-PRODUTO
                FILE STATUS    IS WS-FS-PRC-PRODUTO.
      *
           SELECT PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS RANDOM
                RECORD KEY     IS COD-PRODUTO
                FILE STATUS    IS WS-FS-PRODUTO.
      *
       DATA DIVISION.
       FILE SECTION.
       FD PRC-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\PrcProduto.cpy".
      *
       FD PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\Produto.cpy".
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-PRECO-PRODUTO.
           05 WS-CHAVE-PRECO-PRODUTO.
               10 WS-FK-COD-PRODUTO            PIC X(14).
               10 DATA-PRECO                   PIC X(08).
               10 DATA-PRECO-DDMMAAAA REDEFINES DATA-PRECO.
                   15 WS-ANO-PRECO             PIC 9(04).
                   15 WS-MES-PRECO             PIC 9(02).
                   15 WS-DIA-PRECO             PIC 9(02).
           05 WS-VLR-PRECO                     PIC 9(12)V99.
      *
       01 WS-REG-PRODUTO.
           05 WS-COD-PRODUTO                   PIC X(14).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
      *
       01 WS-LKS-AREA.
           05 WS-LKS-DATA.
               10 WS-LKS-DD                    PIC 99.
               10 FILLER                       PIC X VALUE "/".
               10 WS-LKS-MM                    PIC 99.
               10 FILLER                       PIC X VALUE "/".
               10 WS-LKS-AAAA                  PIC 9999.
           05 WS-LKS-RETORNO                   PIC 9.
      *
       01 WS-DATA-PRECO-TELA.
           05 WS-DD-PRECO-TELA                 PIC XX.
           05 FILLER                           PIC X VALUE "/".
           05 WS-MM-PRECO-TELA                 PIC XX.
           05 FILLER                           PIC X VALUE "/".
           05 WS-AAAA-PRECO-TELA               PIC XXXX.
      *
       77 WS-FS-PRC-PRODUTO                    PIC X(02).
           88 WS-FS-PRC-PROD-OK                VALUE "00".
           88 WS-FS-PRC-PROD-NAO-EXISTE        VALUE "35".
      *
       77 WS-FS-PRODUTO                        PIC X(02).
           88 WS-FS-PROD-OK                    VALUE "00".
           88 WS-FS-PROD-NAO-EXISTE            VALUE "35".
      *
       77 WS-RESPOSTA-TELA                     PIC X(01).
           88 FLAG-SAIR                        VALUE "Q".
           88 FLAG-GRAVAR                      VALUE "S".
      *
       77 WS-MENSAGEM                          PIC X(50) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
       77 WS-VALIDA-PRODUTO                    PIC X(01) VALUE SPACES.
           88 FLAG-PRODUTO-VALIDO              VALUE "S".
       77 WS-VALIDA-DATA-PRECO                 PIC X(01) VALUE SPACES.
           88 FLAG-DATA-PRECO-VALIDO           VALUE "S".
       77 WS-VALIDA-VALOR-PRECO                PIC X(01) VALUE SPACES.
           88 FLAG-VALOR-PRECO-VALIDO          VALUE "S".
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
           05 LINE 03 COL 05 VALUE "SMCP0310 - Inclusao".
           05 LINE 04 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 06 COL 05 VALUE "Codigo do Porduto...: ".
           05 SS-COD-TIPO REVERSE-VIDEO PIC X(14)
                           USING WS-COD-PRODUTO.
           05 LINE 08 COL 05 VALUE "Data (DD/MM/AAAA)...: ".
           05 SS-DATA-PRECO-TELA REVERSE-VIDEO PIC X(10)
                           USING WS-DATA-PRECO-TELA.
           05 LINE 10 COL 05 VALUE "Preco de Produto....: ".
           05 LINE 10 COL 27 REVERSE-VIDEO USING WS-VLR-PRECO.
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

           PERFORM P300-CADASTRA THRU P300-FIM UNTIL FLAG-SAIR.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-PRC-PROD-OK       TO  TRUE.
           SET WS-FS-PROD-OK           TO  TRUE.
           MOVE SPACES                 TO WS-RESPOSTA-TELA.

           PERFORM P120-ABRE-PRC-PRODUTO THRU P120-FIM.

           PERFORM P130-ABRE-PRODUTO THRU P130-FIM.
      *
       P100-FIM.
      *
       P120-ABRE-PRC-PRODUTO.
      *
           OPEN I-O PRC-PRODUTO

           IF WS-FS-PRC-PROD-NAO-EXISTE THEN
               OPEN OUTPUT PRC-PRODUTO
           END-IF.
      *
           IF NOT WS-FS-PRC-PROD-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO PRC-PRODUTO FS: "
                       WS-FS-PRC-PRODUTO    INTO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1501
               PERFORM P900-FIM
           END-IF.
      *
           P120-FIM.
      *
       P130-ABRE-PRODUTO.
      *
           OPEN INPUT PRODUTO

           IF NOT WS-FS-PROD-OK THEN
               STRING "ERRO NA ABERTURA DO ARQUIVO PRODUTO FS: "
                       WS-FS-PRODUTO    INTO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1501
               PERFORM P900-FIM
           END-IF.
      *
       P130-FIM.
      *
       P300-CADASTRA.
      *
           MOVE SPACES             TO WS-COD-PRODUTO.
           MOVE SPACES             TO WS-DESC-PRODUTO.
           MOVE ZEROS              TO WS-DD-PRECO-TELA.
           MOVE ZEROS              TO WS-MM-PRECO-TELA.
           MOVE ZEROS              TO WS-AAAA-PRECO-TELA.
           MOVE ZEROS              TO WS-VLR-PRECO.
           MOVE SPACES             TO WS-RESPOSTA-TELA.
      *
           DISPLAY SS-CLEAR-SCREEN.
           DISPLAY SS-INPUT-SCREEN.
           ACCEPT  SS-INPUT-SCREEN.
      *
           IF FLAG-GRAVAR
               PERFORM P410-VALIDA-PRODUTO THRU P410-FIM
      *
               IF FLAG-PRODUTO-VALIDO THEN
                   PERFORM P420-VALIDA-DATA-PRECO THRU P420-FIM
      *
                   IF FLAG-DATA-PRECO-VALIDO THEN
                       PERFORM P430-VALIDA-VALOR-PRECO THRU P430-FIM
      *
                       IF FLAG-VALOR-PRECO-VALIDO THEN
                           PERFORM P320-GRAVA-PRECO-PRODUTO
                                                   THRU P320-FIM
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
       P300-FIM.
      *
       P320-GRAVA-PRECO-PRODUTO.
      *
           MOVE WS-COD-PRODUTO             TO WS-FK-COD-PRODUTO
           MOVE WS-DD-PRECO-TELA           TO WS-DIA-PRECO
           MOVE WS-MM-PRECO-TELA           TO WS-MES-PRECO
           MOVE WS-AAAA-PRECO-TELA         TO WS-ANO-PRECO

           WRITE   REG-PRECO-PRODUTO   FROM    WS-REG-PRECO-PRODUTO

           IF WS-FS-PRC-PROD-OK THEN
               MOVE "PRECO DE PRODUTO CADATRADO OK"
                                           TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
           ELSE
               IF WS-FS-PRC-PRODUTO = "22" THEN
                   MOVE "PRECO DE PRODUTO JA CADATRADO"
                                           TO WS-MENSAGEM
                   DISPLAY SS-LINHA-DE-MENSAGEM
                   ACCEPT WS-PROMPT AT 1401
                   DISPLAY SS-LIMPA-MENSAGEM
               ELSE
                   STRING "ERRO NA GRAVACAO DO ARQUIVO - FS: "
                   WS-FS-PRC-PRODUTO           INTO WS-MENSAGEM
                   DISPLAY SS-LINHA-DE-MENSAGEM
                   ACCEPT WS-PROMPT AT 1401
                   DISPLAY SS-LIMPA-MENSAGEM
               END-IF
           END-IF.
      *
       P320-FIM.
      *
       P410-VALIDA-PRODUTO.
      *
           MOVE SPACES                     TO WS-VALIDA-PRODUTO.
           MOVE WS-COD-PRODUTO             TO COD-PRODUTO.

           READ PRODUTO INTO WS-REG-PRODUTO
               KEY IS COD-PRODUTO
                   INVALID KEY
                       MOVE "CODIGO DE PRODUTO NAO CADASTRADO"
                                                   TO WS-MENSAGEM
                       DISPLAY SS-LINHA-DE-MENSAGEM
                       ACCEPT WS-PROMPT AT 1401
                       DISPLAY SS-LIMPA-MENSAGEM
                   NOT INVALID KEY
                       SET FLAG-PRODUTO-VALIDO TO TRUE
                       DISPLAY WS-DESC-PRODUTO        AT 0642
           END-READ.
      *
       P410-FIM.
      *
       P420-VALIDA-DATA-PRECO.
      *
           MOVE SPACES                         TO WS-VALIDA-DATA-PRECO.

           IF WS-AAAA-PRECO-TELA       NOT NUMERIC THEN
               MOVE "ANO INVALIDO"             TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
           ELSE
               IF WS-MM-PRECO-TELA     NOT NUMERIC THEN
                   MOVE "MES INVALIDO"             TO WS-MENSAGEM
                   DISPLAY SS-LINHA-DE-MENSAGEM
                   ACCEPT WS-PROMPT AT 1401
                   DISPLAY SS-LIMPA-MENSAGEM
               ELSE
                   IF WS-DD-PRECO-TELA NOT NUMERIC THEN
                       MOVE "DIA INVALIDO"             TO WS-MENSAGEM
                       DISPLAY SS-LINHA-DE-MENSAGEM
                       ACCEPT WS-PROMPT AT 1401
                       DISPLAY SS-LIMPA-MENSAGEM
                   ELSE
                       MOVE WS-DATA-PRECO-TELA         TO WS-LKS-DATA
                       CALL "SCMP0901" USING WS-LKS-AREA
                       EVALUATE WS-LKS-RETORNO
                           WHEN    ZERO
                               SET FLAG-DATA-PRECO-VALIDO
                                                       TO TRUE
                           WHEN    1
                               MOVE "DATA INVALIDA"    TO WS-MENSAGEM
                               DISPLAY SS-LINHA-DE-MENSAGEM
                               ACCEPT WS-PROMPT AT 1401
                               DISPLAY SS-LIMPA-MENSAGEM
                           WHEN    2
                               MOVE "DATA INVALIDA"    TO WS-MENSAGEM
                               DISPLAY SS-LINHA-DE-MENSAGEM
                               ACCEPT WS-PROMPT AT 1401
                               DISPLAY SS-LIMPA-MENSAGEM
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF.
      *
       P420-FIM.
      *
       P430-VALIDA-VALOR-PRECO.
      *
           MOVE SPACES                     TO WS-VALIDA-VALOR-PRECO.

           IF WS-VLR-PRECO <= ZERO THEN
               MOVE "VALOR DO PRODUTO INVALIDO"        TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
           ELSE
               SET FLAG-VALOR-PRECO-VALIDO     TO TRUE
           END-IF.
      *
           P430-FIM.
      *
       P900-FIM.
           CLOSE   PRODUTO
                   PRC-PRODUTO.
           GOBACK.
       END PROGRAM SCMP0310.
