      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS - ALTERAÇÃO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM0103.
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
           88 FLAG-CONTINUAR                   VALUE "S".
      *
       77 WS-MENSAGEM                          PIC X(30) VALUE SPACES.
       77 WS-PROMPT                            PIC X(01) VALUE SPACES.
      *
       SCREEN SECTION.
      *
       01 SS-CABECALHO-TELA.
           05 VALUE ".================================================."
                   BLANK SCREEN                LINE 01 COL 10.
           05 VALUE "|"                        LINE 02 COL 10.
           05 VALUE "CADASTRO DE TIPOS DE PRODUTOS"
                                               LINE 02 COL 20.
           05 VALUE "|"                        LINE 02 COL 59.
           05 VALUE "+------------------------------------------------+"
                                               LINE 03 COL 10.
           05 VALUE "|"                        LINE 04 COL 10.
           05 VALUE "ALTERACAO"
                                               LINE 04 COL 31.
           05 VALUE "|"                        LINE 04 COL 59.
           05 VALUE "+================================================+"
                                               LINE 05 COL 10.
      *
       01  SS-TELA-DE-DADOS.
           05 VALUE "|"                        LINE 06 COL 10.
           05 VALUE "|"                        LINE 06 COL 59.
           05 VALUE "|"                        LINE 07 COL 10.
           05 VALUE "Tipo produto..:"          LINE 07 COL 12.
           05 VALUE "|"                        LINE 07 COL 59.
           05 VALUE "|"                        LINE 08 COL 10.
           05 VALUE "|"                        LINE 08 COL 59.
           05 VALUE "|"                        LINE 09 COL 10.
           05 VALUE "Descricao tipo:"          LINE 09 COL 12.
           05 VALUE "|"                        LINE 09 COL 59.
           05 VALUE "|"                        LINE 10 COL 10.
           05 VALUE "|"                        LINE 10 COL 59.
           05 VALUE "+================================================+"
                                               LINE 11 COL 10.
           05 VALUE "DIGITE <S> PARA CONFIRMAR / <Q> PARA SAIR [ ]"
                                               LINE 12 COL 12.
      *
       01  SS-TELA-ALTERACAO.
      *     05 LINE 15 COL 1 VALUE "TESTE".
           05 SS-DESC-TIPO PIC X(50)
               LINE 9 COL 28
               USING WS-DESC-TIPO.
      *
           05 SS-CONFIRMACAO-OPERACAO PIC X(01)
               LINE 12 COL 55.
      *
       01  SS-LINHA-DE-MENSAGEM.
           05 SS-MENSAGEM              PIC X(30) USING WS-MENSAGEM
                                               LINE 14 COL 12.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA THRU P100-FIM.

           PERFORM P300-PROCESSA THRU P300-FIM UNTIL FLAG-SAIR.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-OK           TO  TRUE.

           OPEN I-O TP-PRODUTO
      *
           IF NOT WS-FS-OK THEN
               MOVE "ERRO NA ABERTURA DO ARQUIVO"
                                           TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT LINE 14 COL 50
               PERFORM P900-FIM
           END-IF.
      *
       P100-FIM.
      *
       P300-PROCESSA.
      *
           MOVE SPACES                         TO WS-COD-TIPO.
           MOVE SPACES                         TO WS-DESC-TIPO.
      *
           DISPLAY SS-CABECALHO-TELA.
           DISPLAY SS-TELA-DE-DADOS.
      *     DISPLAY SS-CONFIRMACAO-ENTRADA.
      *
           ACCEPT WS-COD-TIPO      LINE 07 COL 28.
           ACCEPT WS-RESPOSTA-TELA LINE 12 COL 55.
      *
           IF FLAG-CONTINUAR THEN
               MOVE WS-COD-TIPO                    TO COD-TIPO
      *
               READ TP-PRODUTO  INTO    WS-REG-TIPO-PRODUTO
                   KEY IS COD-TIPO
                       INVALID KEY
                           MOVE "TIPO DE PRODUTO NÃO EXISTE"
                                           TO WS-MENSAGEM
                           DISPLAY SS-LINHA-DE-MENSAGEM
                           ACCEPT WS-PROMPT LINE 14 COL 50
                       NOT INVALID KEY
                           MOVE SPACE              TO WS-RESPOSTA-TELA
      *----------------------------------------------------------------
                           ACCEPT SS-TELA-ALTERACAO
      *----------------------------------------------------------------
                           IF FLAG-CONTINUAR THEN
                               MOVE SS-DESC-TIPO   TO WS-DESC-TIPO
                               PERFORM P400-ATUALIZAR THRU P400-FIM
                           END-IF
               END-READ
           END-IF.
      *
       P300-FIM.
      *
       P400-ATUALIZAR.
      *
           MOVE WS-REG-TIPO-PRODUTO TO REG-TIPO-PRODUTO.

           REWRITE REG-TIPO-PRODUTO.

           IF WS-FS-TP-PRODUTO NOT EQUAL ZEROS THEN
               MOVE "ERRO NA ALTERACAO DO REGISTRO"
                                           TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT LINE 14 COL 50
           ELSE
               MOVE "REGISTRO ATUALIZADO OK"
                                           TO WS-MENSAGEM
               DISPLAY SS-LINHA-DE-MENSAGEM
               ACCEPT WS-PROMPT LINE 14 COL 50
           END-IF.
      *
       P400-FIM.
      *
       P900-FIM.
           CLOSE TP-PRODUTO.
           GOBACK.
       END PROGRAM SCM0103.
