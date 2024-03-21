      ******************************************************************
      * Author: ANDRE RAFFUL
      * Date: 04/03/2024
      * Purpose: CADASTRO DE TIPOS DE PRODUTOS - LISTAGEM
      ******************************************************************
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.    SCMP0902.
      * VALIDA DIGITO VERIFICADOR CODIGO DE BARRAS PARAO EAN-13
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  WS-AUXILIARES.
           05 WS-CODIGO-DE-BARRAS.
               10 WS-CODIGO-PRODUTO.
                 15 WS-DIGITO-1          PIC 9(001).
                 15 WS-DIGITO-2          PIC 9(001).
                 15 WS-DIGITO-3          PIC 9(001).
                 15 WS-DIGITO-4          PIC 9(001).
                 15 WS-DIGITO-5          PIC 9(001).
                 15 WS-DIGITO-6          PIC 9(001).
                 15 WS-DIGITO-7          PIC 9(001).
                 15 WS-DIGITO-8          PIC 9(001).
                 15 WS-DIGITO-9          PIC 9(001).
                 15 WS-DIGITO-10         PIC 9(001).
                 15 WS-DIGITO-11         PIC 9(001).
                 15 WS-DIGITO-12         PIC 9(001).
               10 WS-DIGITO-VERIFICADOR  PIC 9(001).
           05  WS-CALCULO-DIGITO.
               10  WS-SOMA               PIC 9(004).
               10  WS-QUOCIENTE          PIC 9(004).
               10  WS-DIGITO-CALCULADO   PIC 9(001).
               10  WS-RESTO              PIC 9(004).
                   88  RESTO-ZERO        VALUE 0000.
                   88  RESTO-DIFE        VALUE 0001 THRU 9999.
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  LKS-PARAMETRO.
           05 LKS-CODIGO-DE-BARRAS.
               10 LKS-CODIGO-PRODUTO     PIC X(012).
               10 LKS-DIGITO-VERIFICADOR PIC X(001).
           05 LKS-RETORNO                PIC 9(001).
      *-----------------------------------------------------------------
      * LKS-DATA    = FORMATO DD/MM/AAAA OU DD.MM.AAAA OU DD MM AAAA
      * LKS-RETORNO = 0 - Código de barras correto
      * LKS-RETORNO = 1 - Código de barras inválido
      * LKS-RETORNO = 2 - Dígito verificador inválido
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING LKS-PARAMETRO.
      *-----------------------------------------------------------------
           IF LKS-CODIGO-DE-BARRAS IS NOT NUMERIC
               MOVE 1                          TO LKS-RETORNO
           ELSE
               MOVE LKS-CODIGO-DE-BARRAS       TO WS-CODIGO-DE-BARRAS
               MOVE ZEROS                      TO WS-SOMA
      *----<< SOMANDO DIGITOS PARES E MULTIPLICANDO POR 4 >>------------
               ADD WS-DIGITO-2                 TO WS-SOMA
               ADD WS-DIGITO-4                 TO WS-SOMA
               ADD WS-DIGITO-6                 TO WS-SOMA
               ADD WS-DIGITO-8                 TO WS-SOMA
               ADD WS-DIGITO-10                TO WS-SOMA
               ADD WS-DIGITO-12                TO WS-SOMA
               COMPUTE WS-SOMA EQUAL   WS-SOMA * 3
      *----<< SOMANDO DIGITOS IMPARES E MULTIPLICANDO >>----------------
               ADD WS-DIGITO-1                 TO WS-SOMA
               ADD WS-DIGITO-3                 TO WS-SOMA
               ADD WS-DIGITO-5                 TO WS-SOMA
               ADD WS-DIGITO-7                 TO WS-SOMA
               ADD WS-DIGITO-9                 TO WS-SOMA
               ADD WS-DIGITO-11                TO WS-SOMA
      *----<< CALCULANDO O DIGITO VERIFICADOR >>------------------------
               DIVIDE WS-SOMA BY 10    GIVING      WS-QUOCIENTE
                                       REMAINDER   WS-RESTO
               IF RESTO-ZERO THEN
                   MOVE ZERO                   TO WS-DIGITO-CALCULADO
               ELSE
                   COMPUTE  WS-DIGITO-CALCULADO = 10 - WS-RESTO
               END-IF
           END-IF.

           IF WS-DIGITO-CALCULADO EQUAL WS-DIGITO-VERIFICADOR
               MOVE ZERO                       TO  LKS-RETORNO
           ELSE
               MOVE 2                          TO  LKS-RETORNO
           END-IF.

               GOBACK.
       END PROGRAM SCMP0902.
