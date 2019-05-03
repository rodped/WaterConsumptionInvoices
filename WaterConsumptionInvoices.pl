consumption(idclient(1000), name('Pedro'), address('Braganca'), npersons(1), cubicmeters0_5(3), cubicmeters6_15(0), month(1), year(2019)).

show_invoice(C, I) :- consumption(_, name(C), _, npersons(NP), cubicmeters0_5(W1), cubicmeters6_15(W2), _, _), I is (1.5*NP+0.5*W1+1*W2).
