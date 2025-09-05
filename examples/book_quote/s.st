b1 ? [Title];
b1 ! [Quote];
b2 ? { ok:
        b1 ! { ok:   b1 ! [Addr]; b1 ? [Addr]; end },
       quit:
        b1 ! { quit: end } }