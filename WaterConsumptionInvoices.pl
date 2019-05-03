:- dynamic consumption/8:
:- dynamic invoice/4:

consumption(idclient(1000), name('Pedro'), address('Braganca'), npersons(1), cubicmeters0_5(3), cubicmeters6_15(0), month(1), year(2019)).

invoice(idclient(1000), water_bill(3), month(1), year(2019)).

show_invoice(C, I) :- consumption(_, name(C), _, npersons(NP), cubicmeters0_5(W1), cubicmeters6_15(W2), _, _), I is (1.5*NP+0.5*W1+1*W2).

/*Insert the consumption of one client in one month*/
insert_consumption :-
    write('Client Id:'), read(A),
    write('Name:'), read(B),
    write('Address:'), read(C),
    write('Number of Persons:'), read(D),
    write('CubicMeters (0 to 5):'), read(E),
    write('CubicMeters (6 to 15):'), read(F),
    write('Month:'), read(G),
    write('Year:'), read(H), !,
    assert(consumption(idclient(A), name(B), address(C), npersons(D), cubicmeters0_5(E), cubicmeters6_15(F), month(G), year(H))).

/*Create the invoice of a client*/
create_invoice(Client, Month) :-
    write('Name:'), read(A),
    consumption(idclient(A), name(B), _, npersons(D), cubicmeters0_5(E), cubicmeters6_15(F), month(G), year(H)),
    B=Client, G=Month,
    WaterBill is (1.5*D+0.5*E+1.0+F),
    assert(invoice(idclient(A), water_bill(WaterBill), month(G), year(H))).

/*Calculate the average consumption for one client in one year*/
calculate_average() :-
    write('Name:'), read(Name),
    write('Year:'), read(Year),
    findall(T, (invoice(_, water_bill(T), _, year(B)), T=Name, B=Year), S),
    sum_up(S,WaterTotal),
    WaterAverage is (WaterTotal/12),
    write(WaterAverage).

sum_up([],0).
sum_up([X|Y],N) :- sum_up(Y, N1), N is N1+X.


/*Start the program*/
start :- menu.

/*Shows the menu*/
menu :-
    write('\n---------------------------------------------------------------------\n'),
    write('-- 1. Insert the consumption of one client in one month            --\n'),
    write('-- 2. Create the invoice of a client                               --\n'),
    write('-- 3. Calculate the average consumption for one client in one year --\n'),
    write('-- 0. Exit                                                         --\n'),
    write('---------------------------------------------------------------------\n\n'),

    read(A),
    choice(A).

/*Executes the selected option*/
choice(A) :- (
    A==1, insert_consumption, nl, menu;
    A==2, create_invoice(C, M), nl, menu;
    A==3, calculate_average(), nl, menu;
    A==0, save
).



save :-	tell('fil.txt'),
	listing,
	told.
