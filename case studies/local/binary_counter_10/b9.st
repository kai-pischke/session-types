rec t0.
  b8?{ tok0 : b10!{ tok0 : t0 },
       tok1 : b10!{ tok0 :
              (rec t1.
                 b8?{ tok0 : b10!{ tok0 : t1 },
                      tok1 : b10!{ tok1 : t0 } }) } }
