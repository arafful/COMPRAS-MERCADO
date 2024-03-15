       01  REG-PRECO-PRODUTO.
           05 CHAVE-PRECO-PRODUTO.
               10 FK-COD-PRODUTO       PIC X(14).
               10 DATA-PRECO           PIC X(08).
               10 DATA-PRECO-DDMMAAAA REDEFINES DATA-PRECO.
                   15 ANO-PRECO        PIC 9(04).
                   15 MES-PRECO        PIC 9(02).
                   15 DIA-PRECO        PIC 9(02).
           05 VLR-PRECO                PIC 9(12)V99.
