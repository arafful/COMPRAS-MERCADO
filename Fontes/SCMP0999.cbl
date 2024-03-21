      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 19/10/2023
      * Purpose: MENU PRINCIPAL DO SISTEMA DE COMPRAS DE MERCADO
      * Alterações: 99/99/9999 - XXXXXXXX<autor>XXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *             99/99/9999 - XXXXXXXX<autor>XXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0000.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT PRC-PRODUTO ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\PRC-PRODUTO.dat"
                ORGANIZATION   IS INDEXED
                ACCESS         IS SEQUENTIAL
                RECORD KEY     IS CHAVE-PRECO-PRODUTO
                FILE STATUS    IS WS-FS-PRC-PRODUTO.
      *

       DATA DIVISION.
       FILE SECTION.
       FD PRC-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\PrcProduto.cpy".
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-PRECO-PRODUTO.
           05 WS-CHAVE-PRECO-PRODUTO.
               10 WS-FK-COD-PRODUTO            PIC X(14).
               10 DATA-PRECO                   PIC X(08).
               10 WS-DATA-PRECO-DDMMAAAA REDEFINES DATA-PRECO.
                   15 WS-ANO-PRECO             PIC 9(04).
                   15 WS-MES-PRECO             PIC 9(02).
                   15 WS-DIA-PRECO             PIC 9(02).
           05 WS-VLR-PRECO                     PIC 9(12)V99.

           01 WS-COM-AREA.
           03 WS-MENSAGEM                      PIC X(20).
      *
       77 WS-PROMPT                            PIC X.
       77 WS-FS-PRC-PRODUTO                    PIC X(02).
           88 WS-FS-PRC-PROD-OK                VALUE "00".
           88 WS-FS-PRC-PROD-NAO-EXISTE        VALUE "35".
      *
       77 WS-EXIT                              PIC X(01).
           88 EXIT-OK                          VALUE "S" FALSE "N".
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SET EXIT-OK                         TO FALSE.

           OPEN INPUT PRC-PRODUTO.

           DISPLAY "COD PRODUTO  "
                   "DATA PR "
                   "VALOR PRECO "
           DISPLAY "-------------"
                   "--------"
                   "------------"

           PERFORM UNTIL EXIT-OK

               READ PRC-PRODUTO INTO   WS-REG-PRECO-PRODUTO
                   AT END
                       SET EXIT-OK             TO TRUE
                   NOT AT END
                       DISPLAY         WS-FK-COD-PRODUTO
                       DISPLAY         WS-DATA-PRECO-DDMMAAAA
                       DISPLAY         WS-VLR-PRECO
           END-PERFORM.

           ACCEPT WS-PROMPT.

           GOBACK.
       END PROGRAM SCMP0000.
