      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS - LISTAGEM
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM0102.
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
                ACCESS         IS SEQUENTIAL
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
       77 WS-FIM-DE-ARQUIVO                    PIC X(01).
           88 FLAG-EOF                         VALUE "S".
      *
       01 WS-LISTA-TELA.
           03 WS-LST-CAB-1.
               05 FILLER   PIC X(05) VALUE SPACES.
               05 FILLER   PIC X(70) VALUE ALL "=".
               05 FILLER   PIC X(05) VALUE SPACES.
      *
           03 WS-LST-CAB-2.
               05 FILLER   PIC X(05) VALUE SPACES.
               05 FILLER   PIC X(29) VALUE
                                       "LISTAGEM DE TIPOS DE PRODUTOS".
      *
           03 WS-LST-CAB-3.
               05 FILLER   PIC X(05) VALUE SPACES.
               05 FILLER   PIC X(70) VALUE ALL "=".
               05 FILLER   PIC X(05) VALUE SPACES.
      *
           03 WS-LST-CAB-4.
               05 FILLER   PIC X(05) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE "CODIGO".
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(50) VALUE "DESCRICAO".
      *
           03 WS-LST-CAB-5.
               05 FILLER   PIC X(05) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE ALL "-".
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(50) VALUE ALL "-".
      *
           03 WS-LST-DET-1.
               05 FILLER               PIC X(05) VALUE SPACES.
               05 WS-LISTA-CODIGO      PIC X(10) VALUE SPACES.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 WS-LISTA-DESCRICAO   PIC X(50) VALUE SPACES.
      *
           03 WS-LST-FINAL-0.
               05 FILLER               PIC X(05) VALUE SPACES.
               05 FILLER               PIC X(50) VALUE
                                       "NENHUM REGISTRO A LISTAR".
      *
           03 WS-LST-FINAL-1.
               05 FILLER               PIC X(05) VALUE SPACES.
               05 FILLER               PIC X(05) VALUE SPACES.
               05 FILLER               PIC X(20) VALUE
                                       "REGISTROS LISTADOS: ".
               05 WS-LISTA-QTD-REG     PIC 999 VALUE ZEROS.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA THRU P100-FIM.

           PERFORM P300-LISTA THRU P300-FIM UNTIL FLAG-EOF.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-OK            TO  TRUE.
           MOVE "N"                TO  WS-FIM-DE-ARQUIVO.

           OPEN INPUT TP-PRODUTO
      *
           IF NOT WS-FS-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO. FS: "
                       WS-FS-TP-PRODUTO
               PERFORM P900-FIM
           END-IF.
      *
           DISPLAY WS-LST-CAB-1.
           DISPLAY WS-LST-CAB-2.
           DISPLAY WS-LST-CAB-3.
           DISPLAY WS-LST-CAB-4.
           DISPLAY WS-LST-CAB-5.
      *
       P100-FIM.
      *
       P300-LISTA.
      *
           READ TP-PRODUTO INTO WS-REG-TIPO-PRODUTO
               AT END
                   MOVE "S"                    TO WS-FIM-DE-ARQUIVO
                   IF WS-LISTA-QTD-REG = ZERO THEN
                       DISPLAY WS-LST-FINAL-0
                   ELSE
                       DISPLAY " "
                       DISPLAY WS-LST-FINAL-1
                   END-IF
               NOT AT END
                   ADD 1                       TO WS-LISTA-QTD-REG
                   MOVE WS-COD-TIPO            TO WS-LISTA-CODIGO
                   MOVE WS-DESC-TIPO           TO WS-LISTA-DESCRICAO
                   DISPLAY WS-LST-DET-1
           END-READ.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE TP-PRODUTO.
           GOBACK.
       END PROGRAM SCM0102.
