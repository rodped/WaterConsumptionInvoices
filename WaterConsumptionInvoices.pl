:- dynamic consumption/8.
:- dynamic invoice/4.

consumption(idclient(1000), name('Pedro'), address('Braganca'), npersons(1), cubicmeters0_5(3), cubicmeters6_15(0), month(1), year(2019)).

invoice(idclient(1000), water_bill(3), month(1), year(2019)).

show_consumption(C, Y) :-
    consumption(_, name(C), _, npersons(NP), cubicmeters0_5(W1), cubicmeters6_15(W2), _, year(Y)), I is (1.5*NP+0.5*W1+1*W2), write(I), nl, fail.
show_invoice(C, Y) :-
    consumption(idclient(I), name(C), _, _, _, _, _, _),
    invoice(idclient(I), water_bill(W), _, year(Y)), write(W), nl, fail.


/*Auxiliary*/
sum_up([],0).
sum_up([X|Y],N) :- sum_up(Y, N1), N is N1+X.

current_year(Y) :-
    get_time(T),
    stamp_date_time(T, date(Y, _, _, _, _, _, _, _, _), 'UTC').

clients_consumption() :-
    consumption(idclient(A), name(B), address(C), npersons(D), cubicmeters0_5(E), cubicmeters6_15(F), month(G), year(H)),
    write('Client Id:'), write(A),
    write(' Name:'), write(B),
    write(' Address:'), write(C),
    write(' Number of Persons:'), write(D),
    write(' CubicMeters (0 to 5):'), write(E),
    write(' CubicMeters (6 to 15):'), write(F),
    write(' Month:'), write(G),
    write(' Year:'), write(H), nl, fail.

monthly_payment() :-
   /* write('Name:'), read(Name),
    write('Year:'), read(Year),*/
    Year is 2019,
    findall(T, (invoice(_, water_bill(T), _, year(B)), T='Pedro', B=Year), S),
    write_monthly_payment('Pedro', S).

write_monthly_payment(Name, Month) :-
    write('"'), write(Name), write('"'),
    write('->'),
    write('"'), write(Month), write('"'),
    write(';').


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
create_invoice() :-
    write('Name:'), read(Name),
    write('Month:'), read(Month),
    write('Year:'), read(Year),
    consumption(idclient(A), name(B), _, npersons(D), cubicmeters0_5(E), cubicmeters6_15(F), month(G), year(H)),
    B=Name, G=Month, H=Year,
    WaterBill is (1.5*D+0.5*E+1.0+F),
    assert(invoice(idclient(A), water_bill(WaterBill), month(Month), year(Year))).

/*Calculate the average consumption for one client in one year*/
calculate_average() :-
    write('Name:'), read(Name),
    write('Year:'), read(Year),
    findall(T, (invoice(_, water_bill(T), _, year(B)), T=Name, B=Year), S),
    sum_up(S, WaterTotal),
    WaterAverage is (WaterTotal/12),
    write('Water Average: '), write(WaterAverage).

/*Calculate the total payment for one client at the end of one year*/
calculate_total() :-
    write('Name:'), read(Name),
    write('Year:'), read(Year),
    findall(T, (invoice(_, water_bill(T), _, year(B)), T=Name, B=Year), S),
    sum_up(S, WaterTotal),
    write('Water Total: '), write(WaterTotal).

/*Total number of cubicmeters consummated over 5 cubicmeters of all clients in the last year*/
over_5cubicmeters() :-
    current_year(Year),
    not(clients_consumption()),
    findall(W, (consumption(_, _, _, _, _, cubicmeters6_15(W), _, year(Y)), Y=Year), S),
    sum_up(S, WaterTotal),
    write('Water Total: '), write(WaterTotal).

/*Generate a graph connecting the monthly payment of a client during one year*/
graph_payment() :-
    tell(fich),
    write('Digraph {'),
    monthly_payment(),
    write('}'),
    told.

/*Start the program*/
start :- menu.

/*Shows the menu*/
menu :-
    write('\n---------------------------------------------------------------------\n'),
    write('-- 1. Insert the consumption of one client in one month            --\n'),
    write('-- 2. Create the invoice of a client                               --\n'),
    write('-- 3. Average consumption for one client in one year               --\n'),
    write('-- 4. Total payment for one client at the end of one year          --\n'),
    write('-- 5. Total of over 5 cubicmeters of all clients in the last year  --\n'),
    write('-- 6. Graph of a monthly payment of a client during one year       --\n'),
    write('-- 0. Exit and save                                                --\n'),
    write('---------------------------------------------------------------------\n\n'),

    read(A),
    choice(A).

/*Executes the selected option*/
choice(A) :-
    (A==1, insert_consumption, nl, menu;
    A==2, create_invoice(), nl, menu;
    A==3, calculate_average(), nl, menu;
    A==4, calculate_total(), nl, menu;
    A==5, over_5cubicmeters(), nl, menu;
    A==6, graph_payment(), nl, menu;
    A==0, save).

/*Save*/
save :-
    tell('fil.txt'),
    listing,
    told.
