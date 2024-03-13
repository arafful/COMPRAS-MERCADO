      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 19/10/2023
      * Purpose: MENU DO CADASTRO DE PRODUTOS
      * Alterações: 99/99/9999 - XXXXXXXX<autor>XXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *             99/99/9999 - XXXXXXXX<autor>XXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCMP0200.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *
       01 WS-COM-AREA.
           03 WS-MENSAGEM                      PIC X(20).
      *
       77 WS-OPCAO-MENU                        PIC X(01).
       77 WS-PROMPT                            PIC X(01).
      *
       77 WS-EXIT                              PIC X(01).
           88 EXIT-OK                          VALUE "S" FALSE "N".
      *
       LINKAGE SECTION.
      *
       01 LK-COM-AREA.
           03 LK-MENSAGEM                      PIC X(20).
      *
       SCREEN SECTION.
           01 SS-CLEAR-SCREEN.
           05 BLANK SCREEN.
      *
       01 SS-MENU-SCREEN.
           05 LINE 02 COL 05 VALUE "SISTEMA DE COMPRAS DE MERCADO".
           05 LINE 03 COL 05 VALUE
                   "SMCP0200 - Menu do Cadastro de Produtos".
           05 LINE 04 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 06 COL 05 VALUE
                             "<1> - INCLUSAO DE PRODUTOS".
           05 LINE 07 COL 05 VALUE
                             "<2> - RELATORIO DE PRODUTOS".
           05 LINE 08 COL 05 VALUE
                             "<3> - ALTERACAO DE PRODUTOS".
           05 LINE 09 COL 05 VALUE
                             "<4> - EXCLUSAO DE PRODUTOS".
           05 LINE 10 COL 05 VALUE
                             "<Q> - RETORNAR MENU PRINCIPAL".
           05 LINE 12 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 13 COL 05 VALUE
                           "DIGITE A OPCAO DESEJADA: ".
           05 SS-OPCAO-MENU REVERSE-VIDEO PIC X(01)
                           USING WS-OPCAO-MENU.
           05 LINE 14 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
      *
       PROCEDURE DIVISION USING LK-COM-AREA.
      *
       MAIN-PROCEDURE.

           SET EXIT-OK                         TO FALSE.
      *
           PERFORM UNTIL EXIT-OK
               MOVE SPACES                     TO WS-OPCAO-MENU
      *
               DISPLAY SS-CLEAR-SCREEN
               DISPLAY SS-MENU-SCREEN
               ACCEPT  SS-MENU-SCREEN
      *
               EVALUATE WS-OPCAO-MENU
                   WHEN "1"
                       CALL "SCMP0210" USING WS-COM-AREA
                   WHEN "2"
                       DISPLAY "RELATORIO EM EXECUCAO. AGUARDE."
                                           AT 1505
                       CALL "SCMP0220" USING WS-COM-AREA
                       DISPLAY "RELATORIO CONCLUIDO            "
                                           AT 1505
                       ACCEPT WS-PROMPT    AT 1537
                   WHEN "3"
      *                 CALL "SCMP0230" USING WS-COM-AREA
                       DISPLAY "ROTINA NAO DISPONIVEL" AT 1505
                       ACCEPT WS-PROMPT AT 1527
                   WHEN "4"
      *                 CALL "SCMP0240" USING WS-COM-AREA
                       DISPLAY "ROTINA NAO DISPONIVEL" AT 1505
                       ACCEPT WS-PROMPT AT 1527
                   WHEN "Q"
                       SET EXIT-OK             TO TRUE
                   WHEN "q"
                       SET EXIT-OK             TO TRUE
                   WHEN OTHER
                       SET EXIT-OK             TO FALSE
               END-EVALUATE
           END-PERFORM.

           GOBACK.
       END PROGRAM SCMP0200.
