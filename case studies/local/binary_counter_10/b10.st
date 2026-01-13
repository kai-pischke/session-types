rec t0.
  b9?{ tok0 : c!{ tok0 : t0 },
       tok1 : c!{ tok0 :
              (rec t1.
                 b9?{ tok0 : c!{ tok0 : t1 },
                      tok1 : c!{ tok1 : t0 } }) } }
