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
       SCREEN SECTION.
       01 SS-CLEAR-SCREEN.
           05 BLANK SCREEN.
      *
       01 SS-MAIN-MANU-SCREEN.
           05 LINE 02 COL 05 VALUE "SISTEMA DE COMPRAS DE MERCADO".
           05 LINE 03 COL 05 VALUE "SMCP0000 - Menu Principal".
           05 LINE 04 COL 05 VALUE
           "------------------------------------------------------------
      -    "--------------".
           05 LINE 06 COL 05 VALUE
                             "<1> - CADASTRO DE TIPOS DE PRODUTOS".
           05 LINE 07 COL 05 VALUE
                             "<2> - CARGA DE PRODUTOS".
           05 LINE 08 COL 05 VALUE
                             "<3> - CARGA DE PRECOS DE PRODUTOS".
           05 LINE 09 COL 05 VALUE
                             "<4> - RELATORIOS".
           05 LINE 10 COL 05 VALUE
                             "<Q> - FINALIZAR".
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
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SET EXIT-OK                         TO FALSE.

           PERFORM UNTIL EXIT-OK
               INITIALIZE                          WS-OPCAO-MENU
      *
               DISPLAY SS-CLEAR-SCREEN
               DISPLAY SS-MAIN-MANU-SCREEN
               ACCEPT  SS-MAIN-MANU-SCREEN
      *
               EVALUATE WS-OPCAO-MENU
                   WHEN "1"
                       CALL "SCMP0100" USING WS-COM-AREA
                   WHEN "2"
                       CALL "SCMP0200" USING WS-COM-AREA
                   WHEN "3"
      *                CALL "SCMP0300" USING WS-COM-AREA
                       DISPLAY "ROTINA NAO DISPONIVEL" AT 1505
                       ACCEPT WS-PROMPT AT 1527
                   WHEN "4"
      *                CALL "SCMP0400" USING WS-COM-AREA
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
       END PROGRAM SCMP0000.
