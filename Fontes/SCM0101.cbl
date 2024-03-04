      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM0101.
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
           05 WS-COD-TIPO                PIC 9(02).
           05 WS-DESC-TIPO               PIC X(50).

       01 WS-REGISTRO-CONTATO                  PIC X(22) VALUE SPACES.
       01 FILLER REDEFINES WS-REGISTRO-CONTATO.
           03 WS-ID-CONTATO                    PIC 9(02).
           03 WS-NM-CONTATO                    PIC X(20).
      *
       77 WS-FS-TP-PRODUTO                     PIC 9(02).
           88 WS-FS-OK                         VALUE ZEROS.
           88 WS-FS-NAO-EXISTE                 VALUE 35.
      *
       77 WS-EOF                               PIC X(01).
           88 EOF-OK                           VALUE "S" FALSE "N".
      *
       77 WS-EXIT                              PIC X(01).
           88 EXIT-OK                          VALUE "S" FALSE "N".
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA THRU P100-FIM.

           PERFORM P300-CADASTRA THRU P300-FIM UNTIL EXIT-OK.

           PERFORM P900-FIM.

       P100-INICIALIZA.

           DISPLAY "*** CADASTRO DE CONTATOS ***".
           DISPLAY " ".

           SET WS-FS-OK           TO  TRUE.

           OPEN I-O TP-PRODUTO

           IF WS-FS-NAO-EXISTE THEN
               OPEN OUTPUT TP-PRODUTO
           END-IF.
      *
           IF NOT WS-FS-OK THEN
               DISPLAY "** ERRO NA ABERTURA DO ARQUIVO TP-PRODUTO ****"
               DISPLAY "FILE STATUS: " WS-FS-TP-PRODUTO
               DISPLAY "**********************************************"
               PERFORM P900-FIM
           END-IF.
      *
       P100-FIM.
      *
       P300-CADASTRA.

           DISPLAY "PARA CADASTRAR UM TIPO PRODUTO, INFORME:".
           DISPLAY "Um numero para o identificador: ".
           ACCEPT WS-COD-TIPO.
           DISPLAY "DESCRIÇÃO.............: ".
           ACCEPT WS-DESC-TIPO.
      *
           MOVE WS-COD-TIPO                    TO COD-TIPO.
           MOVE WS-DESC-TIPO                   TO DESC-TIPO.

           WRITE   REG-TIPO-PRODUTO.
           IF NOT WS-FS-OK
               DISPLAY "***************************************"
               DISPLAY "ERRO NA GRAVACAO DO REGISTRO DE CONTATO"
               DISPLAY "FILE STATUS: " WS-FS-TP-PRODUTO
               DISPLAY "REGISTRO...: " WS-REG-TIPO-PRODUTO
               DISPLAY "***************************************"
               PERFORM P900-FIM
           END-IF.
      *
           DISPLAY " ".
           DISPLAY "Tipo Produto cadastrado com sucesso!".
           DISPLAY " ".

           DISPLAY
           "Tecle <S> para encerrar ou qualquer tecla para continuar: ".
           ACCEPT WS-EXIT.
      *
       P300-FIM.
      *
       P900-FIM.
           CLOSE TP-PRODUTO.
           GOBACK.
       END PROGRAM SCM0101.
