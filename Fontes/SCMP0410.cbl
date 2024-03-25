      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: RELATORIO DE LISTA DE COMPRAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0410.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
                ACCESS         IS SEQUENTIAL
                RECORD KEY     IS CHAVE-PRECO-PRODUTO
                FILE STATUS    IS WS-FS-PRC-PRODUTO.
      *
           SELECT SCMO0410     ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SCMO0410.txt"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL.
      *
           SELECT SORT-REGISTRO     ASSIGN TO
               "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRAS
      -        "-MERCADO\Arquivos\SORT-TMP.txt"
                ORGANIZATION   IS LINE SEQUENTIAL
                ACCESS         IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\Produto.cpy".
      *
       FD PRC-PRODUTO.
           COPY "F:\Meus Docs - Disco Rigido\Desenv\Meus Projetos\COMPRA
      -         "S-MERCADO\Copybooks\PrcProduto.cpy".
      *
       FD SCMO0410.
       01 REG-REPORT                           PIC X(100).
      *
       SD SORT-REGISTRO.
       01 REGISTRO-SORT.
           05 SD-TIPO-PRODUTO                  PIC X(10).
           05 SD-COD-PRODUTO                   PIC X(13).
           05 SD-DATA-COMPRA                   PIC X(08).
           05 SD-DESC-PRODUTO                  PIC X(30).
           05 SD-PRECO-PRODUTO                 PIC 9(06)V99.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-REG-PRODUTO.
           05 WS-COD-PRODUTO                   PIC X(13).
           05 WS-DESC-PRODUTO                  PIC X(50).
           05 WS-FK-COD-TIPO                   PIC X(10).
      *
       01  WS-REG-PRECO-PRODUTO.
           05 WS-CHAVE-PRECO-PRODUTO.
               10 WS-FK-COD-PRODUTO            PIC X(13).
               10 WS-DATA-PRECO                PIC X(08).
               10 WS-DATA-PRECO-DDMMAAAA REDEFINES WS-DATA-PRECO.
                   15 WS-ANO-PRECO             PIC 9(04).
                   15 WS-MES-PRECO             PIC 9(02).
                   15 WS-DIA-PRECO             PIC 9(02).
           05 WS-VLR-PRECO                     PIC 9(06)V99.
      *
       01 WS-REGISTRO-SORT.
           05 WS-SD-TIPO-PRODUTO               PIC X(10).
           05 WS-SD-COD-PRODUTO                PIC X(13).
           05 WS-SD-DATA-COMPRA                PIC X(08).
           05 WS-SD-DESC-PRODUTO               PIC X(30).
           05 WS-SD-PRECO-PRODUTO              PIC 9(06)V99.
      *
       77 WS-FS-PRODUTO                     PIC X(02).
           88 WS-FS-PRD-OK                     VALUE "00".
           88 WS-FS-PRD-NAO-EXISTE             VALUE "35".
      *
       77 WS-FS-PRC-PRODUTO                     PIC X(02).
           88 WS-FS-PRC-OK                         VALUE "00".
           88 WS-FS-PRC-NAO-EXISTE                 VALUE "35".
      *
       01 WS-CONTROLA-QUEBRA.
           05 WS-TP-PRD-ANT                    PIC X(10).
           05 WS-CD-PRD-ANT                    PIC X(13).
           05 WS-DT-CMP-ANT                    PIC X(08).
      *
       01 WS-EDITA-DATA.
           05 WS-EDITA-AAAA                    PIC X(04).
           05 WS-EDITA-MM                      PIC X(02).
           05 WS-EDITA-DD                      PIC X(02).
      *
       01 WS-DATA-CORRENTE.
           05 WS-AAAA-CORRENTE                 PIC 9(04).
           05 WS-MM-CORRENTE                   PIC 9(02).
           05 WS-DD-CORRENTE                   PIC 9(02).
      *
       77 WS-FLAG-IMPRIME                     PIC X(01).
           88 88-NAO-IMPRIME                   VALUE "N".
           88 88-IMPRIME                       VALUE "S".
      *
       77 WS-FIM-DE-ARQUIVO                    PIC X(01).
           88 FLAG-EOF                         VALUE "S".
      *
       77 WS-PROMPT                            PIC X(01).
      *
       01 WS-RELATORIO.
           03 WS-LST-CAB-1.
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(83) VALUE ALL "=".
               05 FILLER   PIC X(01) VALUE SPACES.
      *
           03 WS-LST-CAB-2.
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(11) VALUE "SMCO0410 - ".
               05 FILLER   PIC X(16) VALUE
                                       "LISTA DE COMPRAS".
               05 FILLER   PIC X(37) VALUE SPACES.
               05 FILLER   PIC X(09) VALUE "EMISSAO: ".
               05 WS-CAB-DT-SIS
                           PIC X(10) VALUE SPACES.
      *
           03 WS-LST-CAB-3.
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(83) VALUE ALL "=".
               05 FILLER   PIC X(01) VALUE SPACES.
      *
           03 WS-LST-CAB-4.
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(13) VALUE "TIPO PRODUTO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(13) VALUE "PRODUTO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(30) VALUE "DESCRICAO".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(11) VALUE "DATA COMPRA".
               05 FILLER   PIC X(08) VALUE SPACES.
               05 FILLER   PIC X(05) VALUE "PRECO".
      *
           03 WS-LST-CAB-5.
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(13) VALUE ALL "=".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(13) VALUE ALL "=".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(30) VALUE ALL "=".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(11) VALUE ALL "=".
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(12) VALUE ALL "=".
      *
           03 WS-LST-LINHA.
               05 FILLER   PIC X(01) VALUE SPACES.
               05 FILLER   PIC X(83) VALUE ALL "-".
               05 FILLER   PIC X(01) VALUE SPACES.
      *
           03 WS-DET-REPORT.
               05 FILLER               PIC X(01) VALUE SPACES.
               05 WS-DET-PRD           PIC X(13) VALUE SPACES.
               05 FILLER               PIC X(01) VALUE SPACES.
               05 WS-DET-COD-PRD       PIC X(13) VALUE SPACES.
               05 FILLER               PIC X(01) VALUE SPACES.
               05 WS-DET-DSC-PRD       PIC X(30) VALUE SPACES.
               05 FILLER               PIC X(02) VALUE SPACES.
               05 WS-DET-DT-PRC        PIC X(12) VALUE SPACES.
               05 FILLER               PIC X(03) VALUE " R$".
               05 WS-DET-VLR-PRC       PIC Z(5).99.
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

           PERFORM P300-LISTA THRU P300-FIM.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           SET WS-FS-PRD-OK        TO  TRUE.
           SET WS-FS-PRC-OK        TO  TRUE.
           MOVE "N"                TO  WS-FIM-DE-ARQUIVO.

           PERFORM P120-ABRE-PRODUTO THRU P120-FIM.

           PERFORM P130-ABRE-PRECO-PRODUTO THRU P130-FIM.

           PERFORM P140-ABRE-RELATORIO-SAIDA THRU P140-FIM.
      *
       P100-FIM.
      *
       P120-ABRE-PRODUTO.
      *
           OPEN INPUT PRODUTO
      *
           IF NOT WS-FS-PRD-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO PRODUTO. FS: "
                       WS-FS-PRODUTO       AT 1505
               ACCEPT WS-PROMPT            AT 1501
               PERFORM P900-FIM
           END-IF.
      *
       P120-FIM.
      *
       P130-ABRE-PRECO-PRODUTO.
      *
           OPEN INPUT PRC-PRODUTO
      *
           IF NOT WS-FS-PRC-OK THEN
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO PRECO PRODUTO. FS: "
                       WS-FS-PRC-PRODUTO   AT 1505
               ACCEPT WS-PROMPT            AT 1501
               PERFORM P900-FIM
           END-IF.
      *
       P130-FIM.
      *
       P140-ABRE-RELATORIO-SAIDA.
      *
           OPEN OUTPUT SCMO0410.
      *
       P140-FIM.
      *
       P300-LISTA.
      *
           SORT SORT-REGISTRO
                   ON ASCENDING    KEY SD-TIPO-PRODUTO
                   ON ASCENDING    KEY SD-COD-PRODUTO
                   ON DESCENDING   KEY SD-DATA-COMPRA
               INPUT   PROCEDURE IS P400-PROCESSA-ENTRADA
                               THRU P400-FIM
               OUTPUT  PROCEDURE IS P500-PROCESSA-SAIDA
                               THRU P500-FIM.
      *
       P300-FIM.
      *
       P400-PROCESSA-ENTRADA.
      *
           PERFORM UNTIL FLAG-EOF
      *
               READ PRC-PRODUTO INTO WS-REG-PRECO-PRODUTO
                   AT END
                       SET FLAG-EOF    TO TRUE
               NOT AT END
                   PERFORM P410-ACESSA-PRODUTO
                           THRU P410-FIM
                   PERFORM P420-GRAVA-SORT
                           THRU P420-FIM
               END-READ
           END-PERFORM.
      *
       P400-FIM.
      *
       P410-ACESSA-PRODUTO.
      *
           MOVE WS-FK-COD-PRODUTO      TO COD-PRODUTO

           READ PRODUTO        INTO    WS-REG-PRODUTO
                KEY  IS  COD-PRODUTO
                   INVALID KEY
                       MOVE "** PRODUTO NÃO ENCONTRADO **"
                                       TO WS-DESC-PRODUTO
                       MOVE "**********"
                                       TO WS-FK-COD-TIPO
           END-READ.
      *
       P410-FIM.
      *
       P420-GRAVA-SORT.
      *
           MOVE WS-FK-COD-TIPO         TO SD-TIPO-PRODUTO.
           MOVE WS-FK-COD-PRODUTO      TO SD-COD-PRODUTO.
           MOVE WS-DATA-PRECO          TO SD-DATA-COMPRA.
           MOVE WS-DESC-PRODUTO        TO SD-DESC-PRODUTO.
           MOVE WS-VLR-PRECO           TO SD-PRECO-PRODUTO.
      *
           RELEASE REGISTRO-SORT.
      *
       P420-FIM.
      *
       P500-PROCESSA-SAIDA.
      *
           PERFORM P510-INICIALIZA-REPORT THRU P510-FIM.
      *
           PERFORM UNTIL FLAG-EOF
      *
               RETURN SORT-REGISTRO INTO WS-REGISTRO-SORT
                   AT END
                       SET FLAG-EOF    TO  TRUE
                   NOT AT END
                       PERFORM P520-GERA-REPORT
                                       THRU P520-FIM
               END-RETURN
           END-PERFORM.
      *
           PERFORM P590-FINALIZA-REPORT THRU P590-FIM.
      *
       P500-FIM.
      *
       P510-INICIALIZA-REPORT.
      *
           MOVE SPACES     TO  WS-TP-PRD-ANT
                               WS-CD-PRD-ANT
                               WS-DT-CMP-ANT
                               WS-FIM-DE-ARQUIVO.
      *
           MOVE ZERO       TO  WS-LISTA-QTD-REG.
      *
           PERFORM P530-DATA-DO-SISTEMA THRU P530-FIM.
      *
           WRITE REG-REPORT    FROM WS-LST-CAB-1.
           WRITE REG-REPORT    FROM WS-LST-CAB-2.
           WRITE REG-REPORT    FROM WS-LST-CAB-3.
           WRITE REG-REPORT    FROM WS-LST-CAB-4.
           WRITE REG-REPORT    FROM WS-LST-CAB-5.
      *
       P510-FIM.
      *
       P520-GERA-REPORT.

           SET 88-IMPRIME              TO  TRUE.
           MOVE WS-SD-TIPO-PRODUTO     TO  WS-DET-PRD.
           MOVE WS-SD-COD-PRODUTO      TO  WS-DET-COD-PRD
                                           WS-DET-COD-PRD.
      *
           MOVE WS-SD-DESC-PRODUTO     TO  WS-DET-DSC-PRD.
      *
           MOVE WS-SD-DATA-COMPRA      TO  WS-EDITA-DATA.
      *
           STRING  WS-EDITA-DD "/"
                   WS-EDITA-MM "/"
                   WS-EDITA-AAAA       INTO    WS-DET-DT-PRC.
      *
           MOVE WS-SD-PRECO-PRODUTO    TO  WS-DET-VLR-PRC.
      *
           IF  WS-SD-TIPO-PRODUTO = WS-TP-PRD-ANT  AND
               WS-SD-COD-PRODUTO = WS-CD-PRD-ANT
      *
               SET 88-NAO-IMPRIME    TO  TRUE
           ELSE
               IF WS-SD-TIPO-PRODUTO = WS-TP-PRD-ANT
                   MOVE SPACES         TO  WS-DET-PRD
               END-IF
           END-IF.
      *
           IF 88-IMPRIME
               WRITE REG-REPORT FROM WS-DET-REPORT
               ADD 1                       TO  WS-LISTA-QTD-REG
           END-IF.
      *
           MOVE WS-SD-TIPO-PRODUTO     TO  WS-TP-PRD-ANT.
           MOVE WS-SD-COD-PRODUTO      TO  WS-CD-PRD-ANT.
           MOVE WS-SD-DATA-COMPRA      TO  WS-DT-CMP-ANT.
      *
       P520-FIM.
      *
       P530-DATA-DO-SISTEMA.
      *
           ACCEPT  WS-DATA-CORRENTE FROM DATE YYYYMMDD.

           STRING  WS-DD-CORRENTE "/"
                   WS-MM-CORRENTE "/"
                   WS-AAAA-CORRENTE    INTO    WS-CAB-DT-SIS.
      *
       P530-FIM.
      *
       P590-FINALIZA-REPORT.

           IF WS-LISTA-QTD-REG = ZERO THEN
               WRITE REG-REPORT        FROM WS-LST-FINAL-0
           ELSE
               WRITE REG-REPORT        FROM WS-LST-LINHA
               WRITE REG-REPORT        FROM WS-LST-FINAL-1
           END-IF.

       P590-FIM.
      *
       P900-FIM.
           CLOSE   PRODUTO
                   PRC-PRODUTO
                   SCMO0410.
           GOBACK.
       END PROGRAM SCMP0410.
