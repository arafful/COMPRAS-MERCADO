       01  REG-PRECO-PRODUTO.
           05 FK-COD-PRODUTO            PIC 9(14)
           05 DATA-PRECO                PIC X(10).
           05 DATA-PRECO-DDMMAAAA REDEFINES DATA-PRECO.
               10 DIA-PRECO             PIC 99.
               10 FILLER                PIC X VALUE "/".
               10 MES-PRECO             PIC 99.
               10 FILLER                PIC X VALUE "/".
               10 ANO-PRECO             PIC 9999.
           05 VLR-PRECO                 PIC 9(12)V99.