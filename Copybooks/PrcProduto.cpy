       01  REG-PRECO-PRODUTO.
           05 FK-COD-PRODUTO            PIC x(14)
           05 DATA-PRECO                PIC X(08).
           05 DATA-PRECO-DDMMAAAA REDEFINES DATA-PRECO.
               10 ANO-PRECO             PIC 9(04).
               10 MES-PRECO             PIC 9(02).
               10 DIA-PRECO             PIC 9(02).
           05 VLR-PRECO                 PIC 9(12)V99.