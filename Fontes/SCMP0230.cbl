      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE PRODUTOS - ALTERACAO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0230.
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
           05 WS-COD-PRODUTO                   PIC X(14).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
      *
       01 WS-REG-TIPO-PRODUTO.
           05 WS-COD-TIPO                      PIC X(10).
           05 WS-DESC-TIPO                     PIC X(50).
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
           88 FLAG-CONTINUAR                   VALUE "S".
      *
       77 WS-MENSAGEM                          PIC X(50) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
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
           05 LINE 03 COL 05 VALUE "SMCP0230 - Alteracao".
           05 LINE 04 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 06 COL 05 VALUE "Codigo do Porduto...: ".
           05 LINE 08 COL 05 VALUE "Descricao do Produto: ".
           05 SS-DESC-TIPO REVERSE-VIDEO PIC X(50)
                           USING WS-DESC-PRODUTO.
           05 LINE 10 COL 05 VALUE "Tipo de Produto.....: ".
           05 SS-FK-COD-TIPO REVERSE-VIDEO PIC X(10)
                           USING WS-FK-COD-TIPO.
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

           PERFORM P300-PROCESSA THRU P300-FIM UNTIL FLAG-SAIR.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-PROD-OK           TO  TRUE.
           SET WS-FS-TP-OK             TO  TRUE.
           MOVE SPACES                 TO WS-RESPOSTA-TELA.

           PERFORM P120-ABRE-PRODUTO THRU P120-FIM.

           PERFORM P130-ABRE-TP-PRODUTO THRU P130-FIM.
      *
       P100-FIM.
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
           MOVE SPACES             TO WS-ACHOU-TIPO-PRODUTO.
      *
           DISPLAY SS-CLEAR-SCREEN.
           DISPLAY SS-INPUT-SCREEN.

           ACCEPT WS-COD-PRODUTO   REVERSE-VIDEO   AT 0627.

           IF WS-COD-PRODUTO EQUAL SPACES THEN
               MOVE "Q"            TO  WS-RESPOSTA-TELA
           ELSE
               PERFORM P310-PROCESSAR-DADOS    THRU    P310-FIM
           END-IF.
      *
       P300-FIM.
      *
       P310-PROCESSAR-DADOS.
      *
           MOVE WS-COD-PRODUTO     TO  COD-PRODUTO.

           READ PRODUTO    INTO    WS-REG-PRODUTO
               KEY IS COD-PRODUTO
                   INVALID KEY
                       MOVE "PRODUTO NAO EXISTE"
                                   TO WS-MENSAGEM
                       DISPLAY SS-LINHA-DE-MENSAGEM
                       ACCEPT WS-PROMPT AT 1401
                       DISPLAY SS-LIMPA-MENSAGEM
                   NOT INVALID KEY
                       MOVE    WS-FK-COD-TIPO  TO FK-COD-TIPO
                       PERFORM P410-VALIDA-TIPO-PRODUTO THRU P410-FIM
                       IF FLAG-TP-PROD-VALIDO THEN
                           MOVE SPACE          TO WS-RESPOSTA-TELA
                           ACCEPT  SS-INPUT-SCREEN
                           IF FLAG-CONTINUAR THEN
                               PERFORM P400-ATUALIZAR-PRODUTO
                                  THRU P400-FIM
                           END-IF
                       END-IF
               END-READ.
      *
       P310-FIM.
      *
       P400-ATUALIZAR-PRODUTO.
      *
           REWRITE   REG-PRODUTO         FROM    WS-REG-PRODUTO.

           IF WS-FS-PROD-OK THEN
               DISPLAY WS-DESC-TIPO        AT 1038
               MOVE "PRODUTO ALTERADO OK"         TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
           ELSE
               STRING "ERRO NA GRAVACAO DO ARQUIVO - FS: "
               WS-FS-PRODUTO                   INTO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1401
               DISPLAY SS-LIMPA-MENSAGEM
           END-IF.
      *
       P400-FIM.
      *
       P410-VALIDA-TIPO-PRODUTO.
      *
           MOVE SPACES                     TO WS-ACHOU-TIPO-PRODUTO.
           MOVE WS-FK-COD-TIPO             TO COD-TIPO.

           READ TP-PRODUTO INTO WS-REG-TIPO-PRODUTO
               KEY IS COD-TIPO
                   INVALID KEY
                       MOVE "TIPO DE PRODUTO NAO CADASTRADO"
                                                   TO WS-MENSAGEM
                       DISPLAY SS-LINHA-DE-MENSAGEM
                       ACCEPT WS-PROMPT AT 1401
                       DISPLAY SS-LIMPA-MENSAGEM
                   NOT INVALID KEY
                       DISPLAY WS-DESC-TIPO        AT 1038
                       SET FLAG-TP-PROD-VALIDO TO TRUE
           END-READ.
      *
       P410-FIM.
      *
       P900-FIM.
           CLOSE   TP-PRODUTO
                   PRODUTO.
           GOBACK.
       END PROGRAM SCMP0230.
