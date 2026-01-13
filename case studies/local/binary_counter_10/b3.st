rec t0.
  b2?{ tok0 : b4!{ tok0 : t0 },
       tok1 : b4!{ tok0 :
              (rec t1.
                 b2?{ tok0 : b4!{ tok0 : t1 },
                      tok1 : b4!{ tok1 : t0 } }) } }
