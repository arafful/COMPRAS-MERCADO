      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0101.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       FD TP-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\TpProduto.cpy".

       WORKING-STORAGE SECTION.
      *
       01 WS-REG-TIPO-PRODUTO.
           05 WS-COD-TIPO                      PIC X(10).
           05 WS-DESC-TIPO                     PIC X(50).
      *
       77 WS-FS-TP-PRODUTO                     PIC 9(02).
           88 WS-FS-OK                         VALUE ZEROS.
           88 WS-FS-NAO-EXISTE                 VALUE 35.
      *
       77 WS-RESPOSTA-TELA                     PIC X(01).
           88 FLAG-SAIR                        VALUE "Q".
           88 FLAG-GRAVAR                      VALUE "S".
      *
       77 WS-MENSAGEM                          PIC X(30) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
      *
       SCREEN SECTION.
      *
       01 SS-CLEAR-SCREEN.
           05 BLANK SCREEN.
      *
       01 SS-INPUT-SCREEN.
           05 LINE 02 COL 05 VALUE "CADASTRO DE TIPOS DE PRODUTOS".
           05 LINE 03 COL 05 VALUE "SMCP0101 - Inclusao".
           05 LINE 04 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 06 COL 05 VALUE "Tipo Porduto..: ".
           05 SS-COD-TIPO REVERSE-VIDEO PIC X(10)
                           USING WS-COD-TIPO.
           05 LINE 08 COL 05 VALUE "Desc Produto..: ".
           05 SS-DESC-TIPO REVERSE-VIDEO PIC X(50)
                           USING WS-DESC-TIPO.
           05 LINE 10 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 11 COL 05 VALUE
                           "<S> para confirmar ou <Q> para Sair. ".
           05 SS-RESPOSTA-TELA REVERSE-VIDEO PIC X(01)
                           USING WS-RESPOSTA-TELA.
           05 LINE 12 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
      *
       01  SS-LINHA-DE-MENSAGEM.
           05 SS-MENSAGEM              PIC X(30) USING WS-MENSAGEM
                                               LINE 13 COL 05.
      *
       01  SS-LIMPA-MENSAGEM.
           05 LINE 13 BLANK LINE.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA THRU P100-FIM.

           PERFORM P300-CADASTRA THRU P300-FIM UNTIL FLAG-SAIR.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-OK           TO  TRUE.

           OPEN I-O TP-PRODUTO

           IF WS-FS-NAO-EXISTE THEN
               OPEN OUTPUT TP-PRODUTO
           END-IF.
      *
           IF NOT WS-FS-OK THEN
               MOVE "ERRO NA ABERTURA DO ARQUIVO"
                                           TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT AT 1301
               DISPLAY SS-LIMPA-MENSAGEM
               PERFORM P900-FIM
           END-IF.
      *
       P100-FIM.
      *
       P300-CADASTRA.
      *
           MOVE SPACES                         TO WS-COD-TIPO.
           MOVE SPACES                         TO WS-DESC-TIPO.
           MOVE SPACES                         TO WS-RESPOSTA-TELA.
      *
           DISPLAY SS-CLEAR-SCREEN.
           DISPLAY SS-INPUT-SCREEN.
           ACCEPT  SS-INPUT-SCREEN.
      *
           IF FLAG-GRAVAR THEN
               IF WS-COD-TIPO  EQUAL   SPACES THEN
                   MOVE "CODIGO DE TIPO INVALIDO." TO WS-MENSAGEM
                   DISPLAY SS-LINHA-DE-MENSAGEM
                   ACCEPT WS-PROMPT AT 1301
                   DISPLAY SS-LIMPA-MENSAGEM
               ELSE
                   MOVE WS-COD-TIPO                    TO COD-TIPO
                   MOVE WS-DESC-TIPO                   TO DESC-TIPO

                   WRITE   REG-TIPO-PRODUTO
                   IF NOT WS-FS-OK
                       IF WS-FS-TP-PRODUTO = 22 THEN
                           MOVE "TIPO DE PRODUTO JÁ CADATRADO"
                                                       TO WS-MENSAGEM
                           DISPLAY SS-LINHA-DE-MENSAGEM
                           ACCEPT WS-PROMPT AT 1301
                           DISPLAY SS-LIMPA-MENSAGEM
                       ELSE
                           MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                                       TO WS-MENSAGEM
                           DISPLAY SS-LINHA-DE-MENSAGEM
                           ACCEPT WS-PROMPT AT 1301
                           DISPLAY SS-LIMPA-MENSAGEM
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE TP-PRODUTO.
           GOBACK.
       END PROGRAM SCMP0101.
