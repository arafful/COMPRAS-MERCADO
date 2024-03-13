      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 13/03/2024
      * Purpose: CADASTRO DE PRODUTOS - LISTAGEM
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0220.
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
           SELECT SCMO0220     ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0220.txt"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\Produto.cpy".
      *
       FD SCMO0220.
       01 REG-REPORT                           PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-PRODUTO.
           05 WS-COD-PRODUTO                   PIC X(14).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
      *
       77 WS-FS-PRODUTO                        PIC X(02).
           88 WS-FS-PROD-OK                    VALUE "00".
      *
       77 WS-FIM-DE-ARQUIVO                    PIC X(01).
           88 FLAG-EOF                         VALUE "S".

       77  WS-PROMPT                           PIC X(01).
       77  WS-MAX-REG                          PIC 9(03) VALUE 200.
       77  WS-IND-TAB                          PIC 9(03).
      *
       01  TABELA-PRODUTOS.
           05 TAB-PRODUTOS     OCCURS  200 TIMES.
               10 TAB-COD-TIPO                 PIC X(10).
               10 TAB-COD-PRODUTO              PIC X(14).
               10 TAB-DESC-PRODUTO             PIC X(50).
      *
       01 WS-LISTA-TELA.
           03 WS-LST-CAB-LINHA.
               05 FILLER   PIC X(80) VALUE ALL "=".
      *
           03 WS-LST-CAB-1.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(70) VALUE
                                       "SCMO220 - LISTAGEM DE PRODUTOS".
      *
           03 WS-LST-CAB-2.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE "TIPO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE "CODIGO PRODUTO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(50) VALUE "DESCRICAO".
      *
           03 WS-LST-CAB-3.
               05 FILLER   PIC X(02) VALUE SPACES.
               05 FILLER   PIC X(10) VALUE ALL "-".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(15) VALUE ALL "-".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(50) VALUE ALL "-".
      *
           03 WS-LST-DET-1.
               05 FILLER                   PIC X(02) VALUE SPACES.
               05 WS-LISTA-COD-TIPO        PIC X(10) VALUE SPACES.
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-LISTA-COD-PRODUTO     PIC X(15) VALUE SPACES.
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-LISTA-DESC-PRODUTO    PIC X(50) VALUE SPACES.
      *
           03 WS-LST-FINAL-0.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 FILLER               PIC X(50) VALUE
                                       "NENHUM REGISTRO A LISTAR".
      *
           03 WS-LST-FINAL-1.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 FILLER               PIC X(20) VALUE
                                       "REGISTROS LISTADOS: ".
               05 WS-LISTA-QTD-REG     PIC 999 VALUE ZEROS.
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

           PERFORM P200-CARREGA-TABELA THRU P200-FIM UNTIL FLAG-EOF.

           PERFORM P250-ORDENA-TABELA THRU P250-FIM.

           PERFORM P300-LISTA THRU P300-FIM.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-PROD-OK       TO  TRUE.
           MOVE "N"                TO  WS-FIM-DE-ARQUIVO.
           MOVE ZERO               TO  WS-IND-TAB

           OPEN INPUT PRODUTO
      *
           IF NOT WS-FS-PROD-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO. FS: "
                       WS-FS-PRODUTO AT 1505
                       ACCEPT WS-PROMPT AT 1501
               PERFORM P900-FIM
           END-IF.
      *
           OPEN OUTPUT SCMO0220.
      *
           WRITE REG-REPORT    FROM WS-LST-CAB-LINHA.
           WRITE REG-REPORT    FROM WS-LST-CAB-1.
           WRITE REG-REPORT    FROM WS-LST-CAB-LINHA.
           WRITE REG-REPORT    FROM WS-LST-CAB-2.
           WRITE REG-REPORT    FROM WS-LST-CAB-3.
      *
       P100-FIM.
      *
       P200-CARREGA-TABELA.
      *
           READ PRODUTO INTO WS-REG-PRODUTO
               AT END
                   MOVE "S"                    TO WS-FIM-DE-ARQUIVO
               NOT AT END
                   ADD 1                       TO WS-IND-TAB
                   IF WS-IND-TAB > 200 THEN
                       DISPLAY
                       "*** LIMITE DE TABELA INTERNA ULTRAPASSADO ***"
                       AT 1505
                       PERFORM P900-FIM
                   ELSE
                       MOVE WS-FK-COD-TIPO
                                       TO TAB-COD-TIPO(WS-IND-TAB)
                       MOVE WS-COD-PRODUTO
                                       TO TAB-COD-PRODUTO(WS-IND-TAB)
                       MOVE WS-DESC-PRODUTO
                                       TO TAB-DESC-PRODUTO(WS-IND-TAB)
                   END-IF
           END-READ.
      *
       P200-FIM.
      *
       P250-ORDENA-TABELA.
      *
           SORT TAB-PRODUTOS ON ASCENDING KEY TAB-COD-TIPO.
      *
       P250-FIM.
      *
       P300-LISTA.
      *
           MOVE ZEROS                          TO WS-IND-TAB.
      *
           PERFORM UNTIL WS-IND-TAB = WS-MAX-REG
      *
               ADD 1                           TO  WS-IND-TAB

               IF TAB-COD-TIPO(WS-IND-TAB) NOT EQUAL SPACES
                   ADD 1                       TO  WS-LISTA-QTD-REG
                   MOVE TAB-COD-TIPO(WS-IND-TAB)
                                               TO  WS-LISTA-COD-TIPO
                   MOVE TAB-COD-PRODUTO(WS-IND-TAB)
                                               TO  WS-LISTA-COD-PRODUTO
                   MOVE TAB-DESC-PRODUTO(WS-IND-TAB)
                                               TO  WS-LISTA-DESC-PRODUTO
      *
                   WRITE REG-REPORT            FROM WS-LST-DET-1
               END-IF
           END-PERFORM.

           IF WS-LISTA-QTD-REG = ZERO THEN
               WRITE REG-REPORT        FROM WS-LST-FINAL-0
           ELSE
               WRITE REG-REPORT        FROM WS-LST-FINAL-1
           END-IF.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE   PRODUTO
                   SCMO0220.
           GOBACK.
       END PROGRAM SCMP0220.
