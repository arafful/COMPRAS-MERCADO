      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS - LISTAGEM
      ******************************************************************
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.    SCMP0901.
      *               BRASILIA, 17 DE MAIO DE 2006
      * VERIFICAR SE A DATA EH VALIDA SEM USAR O BANCO DE DADOS
      * Anos entre 1901 e 2099
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  WS-AUXILIARES.
           05  WS-CALCULO-BISEXTO.
               10  WS-QUOCIENTE          PIC 9(004).
               10  WS-RESTO              PIC 9(004).
                   88  RESTO-ZERO        VALUE 0000.
                   88  RESTO-DIFE        VALUE 0001 THRU 9999.
           05  WS-DATA                   PIC X(010).
           05  WS-DATA-R                 REDEFINES WS-DATA.
               10  WS-DIA                PIC 9(002).
                   88  WS-DIA-29         VALUE 01 THRU 29.
                   88  WS-DIA-28         VALUE 01 THRU 28.
                   88  WS-DIA-30         VALUE 01 THRU 30.
                   88  WS-DIA-31         VALUE 01 THRU 31.
               10  PONTO-001             PIC X(001).
               10  WS-MES                PIC 9(002).
                   88  WS-MES-VALIDO     VALUE 01 THRU 12.
                   88  WS-MES-28         VALUE 02.
                   88  WS-MES-30         VALUE 04 06 09 11.
                   88  WS-MES-31         VALUE 01 03 05 07 08 10 12.
               10  PONTO-002             PIC X(001).
               10  WS-ANO                PIC 9(004).
                   88 ANO-VALIDO         VALUE 1901 THRU 2099.
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  LKS-PARAMETRO.
           05 LKS-DATA                   PIC X(010).
           05 LKS-RETORNO                PIC 9(001).
      *-----------------------------------------------------------------
      * LKS-DATA    = FORMATO DD/MM/AAAA OU DD.MM.AAAA OU DD MM AAAA
      * LKS-RETORNO = 0 - A data informada está correta
      * LKS-RETORNO = 1 - A data informada está incorreta
      * LKS-RETORNO = 2 - O ano ou o mes informado é invalido
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING LKS-PARAMETRO.
      *-----------------------------------------------------------------
           MOVE LKS-DATA TO WS-DATA.
           DIVIDE WS-ANO BY 4 GIVING WS-QUOCIENTE REMAINDER WS-RESTO
           EVALUATE TRUE
               WHEN ANO-VALIDO AND WS-MES-VALIDO
                    EVALUATE TRUE
                        WHEN RESTO-ZERO AND WS-MES-28 AND WS-DIA-29
                        WHEN RESTO-DIFE AND WS-MES-28 AND WS-DIA-28
                        WHEN WS-MES-30  AND WS-DIA-30
                        WHEN WS-MES-31  AND WS-DIA-31
                             MOVE 0 TO LKS-RETORNO
                        WHEN OTHER
                             MOVE 1 TO LKS-RETORNO
                    END-EVALUATE
               WHEN OTHER
                    MOVE 2 TO LKS-RETORNO
           END-EVALUATE
           GOBACK.
       END PROGRAM SCMP0901.
