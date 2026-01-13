rec t0.
  b1?{ tok0 : b3!{ tok0 : t0 },
       tok1 : b3!{ tok0 :
              (rec t1.
                 b1?{ tok0 : b3!{ tok0 : t1 },
                      tok1 : b3!{ tok1 : t0 } }) } }
