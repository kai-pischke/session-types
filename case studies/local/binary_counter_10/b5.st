rec t0.
  b4?{ tok0 : b6!{ tok0 : t0 },
       tok1 : b6!{ tok0 :
              (rec t1.
                 b4?{ tok0 : b6!{ tok0 : t1 },
                      tok1 : b6!{ tok1 : t0 } }) } }
