      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS - LISTAGEM
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0102.
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
           SELECT SCMO0102     ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0102.txt"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD TP-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\TpProduto.cpy".
      *
       FD SCMO0102.
       01 REG-REPORT                           PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-REG-TIPO-PRODUTO.
           05 WS-COD-TIPO                      PIC X(10).
           05 WS-DESC-TIPO                     PIC X(50).
      *
       77 WS-FS-TP-PRODUTO                     PIC X(02).
           88 WS-FS-OK                         VALUE "00".
           88 WS-FS-NAO-EXISTE                 VALUE "35".
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
       LINKAGE SECTION.
      *
       01 LK-COM-AREA.
           03 LK-MENSAGEM                      PIC X(20).
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

           OPEN INPUT TP-PRODUTO
      *
           IF NOT WS-FS-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO. FS: "
                       WS-FS-TP-PRODUTO
               PERFORM P900-FIM
           END-IF.
      *
           OPEN OUTPUT SCMO0102.
      *
           WRITE REG-REPORT    FROM WS-LST-CAB-1.
           WRITE REG-REPORT    FROM WS-LST-CAB-2.
           WRITE REG-REPORT    FROM WS-LST-CAB-3.
           WRITE REG-REPORT    FROM WS-LST-CAB-4.
           WRITE REG-REPORT    FROM WS-LST-CAB-5.
      *
       P100-FIM.
      *
       P300-LISTA.
      *
           READ TP-PRODUTO INTO WS-REG-TIPO-PRODUTO
               AT END
                   MOVE "S"                    TO WS-FIM-DE-ARQUIVO
                   IF WS-LISTA-QTD-REG = ZERO THEN
                       WRITE REG-REPORT        FROM WS-LST-FINAL-0
                   ELSE
                       WRITE REG-REPORT        FROM WS-LST-FINAL-1
                   END-IF
               NOT AT END
                   ADD 1                       TO WS-LISTA-QTD-REG
                   MOVE WS-COD-TIPO            TO WS-LISTA-CODIGO
                   MOVE WS-DESC-TIPO           TO WS-LISTA-DESCRICAO
                   WRITE REG-REPORT            FROM WS-LST-DET-1
           END-READ.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE   TP-PRODUTO
                   SCMO0102.
           GOBACK.
       END PROGRAM SCMP0102.
