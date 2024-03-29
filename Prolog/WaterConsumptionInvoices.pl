:- dynamic consumption/8.
:- dynamic invoice/4.
:- dynamic temp/2.

/*Some information for test*/
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(1.3), month(1), year(2018)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(3), cubicmeters6_15(0), month(1), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(4), cubicmeters6_15(0), month(2), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(1), month(3), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(2), month(4), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(3), cubicmeters6_15(0), month(5), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(3), month(6), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(1), cubicmeters6_15(0), month(7), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(2), cubicmeters0_5(4), cubicmeters6_15(0), month(8), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(2), cubicmeters6_15(0), month(9), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(3), cubicmeters6_15(0), month(10), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(1), cubicmeters0_5(5), cubicmeters6_15(1), month(11), year(2019)).
consumption(idclient(1000), name('John'), address('Lisbon'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(3), month(12), year(2019)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(1), cubicmeters0_5(1.6), cubicmeters6_15(0), month(1), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(1), cubicmeters0_5(2), cubicmeters6_15(0), month(2), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(1), cubicmeters0_5(3.4), cubicmeters6_15(0), month(3), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(1), cubicmeters0_5(1.8), cubicmeters6_15(0), month(4), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(4.9), cubicmeters6_15(0), month(5), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(1.2), month(6), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(3.8), cubicmeters6_15(0), month(7), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(1), cubicmeters0_5(1.7), cubicmeters6_15(0), month(8), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(0.2), month(9), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(4.8), cubicmeters6_15(0), month(10), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(4.7), cubicmeters6_15(0), month(11), year(2018)).
consumption(idclient(1100), name('Steve'), address('New York'), npersons(2), cubicmeters0_5(5), cubicmeters6_15(1.2), month(12), year(2018)).
consumption(idclient(900), name('Anna'), address('Los Angeles'), npersons(4), cubicmeters0_5(5), cubicmeters6_15(1), month(3), year(2018)).
consumption(idclient(900), name('Anna'), address('Los Angeles'), npersons(4), cubicmeters0_5(3), cubicmeters6_15(0), month(2), year(2018)).
consumption(idclient(900), name('Anna'), address('Los Angeles'), npersons(4), cubicmeters0_5(2), cubicmeters6_15(0), month(1), year(2018)).


invoice(idclient(1000), water_bill(6.8), month(1), year(2018)).
invoice(idclient(1000), water_bill(3), month(1), year(2019)).
invoice(idclient(1000), water_bill(3.5), month(2), year(2019)).
invoice(idclient(1000), water_bill(6.5), month(3), year(2019)).
invoice(idclient(1000), water_bill(7.5), month(4), year(2019)).
invoice(idclient(1000), water_bill(3), month(5), year(2019)).
invoice(idclient(1000), water_bill(8.5), month(6), year(2019)).
invoice(idclient(1000), water_bill(2), month(7), year(2019)).
invoice(idclient(1000), water_bill(5), month(8), year(2019)).
invoice(idclient(1000), water_bill(2.5), month(9), year(2019)).
invoice(idclient(1000), water_bill(3), month(10), year(2019)).
invoice(idclient(1000), water_bill(5), month(11), year(2019)).
invoice(idclient(1000), water_bill(8.5), month(12), year(2019)).
invoice(idclient(1100), water_bill(2.3), month(1), year(2018)).
invoice(idclient(1100), water_bill(2.5), month(2), year(2018)).
invoice(idclient(1100), water_bill(3.2), month(3), year(2018)).
invoice(idclient(1100), water_bill(2.4), month(4), year(2018)).
invoice(idclient(1100), water_bill(5.45), month(5), year(2018)).
invoice(idclient(1100), water_bill(6.7), month(6), year(2018)).
invoice(idclient(1100), water_bill(4.9), month(7), year(2018)).
invoice(idclient(1100), water_bill(2.35), month(8), year(2018)).
invoice(idclient(1100), water_bill(5.7), month(9), year(2018)).
invoice(idclient(1100), water_bill(5.4), month(10), year(2018)).
invoice(idclient(1100), water_bill(5.35), month(11), year(2018)).
invoice(idclient(1100), water_bill(6.7), month(12), year(2018)).
invoice(idclient(900), water_bill(9.5), month(3), year(2018)).
invoice(idclient(900), water_bill(7.5), month(2), year(2018)).
invoice(idclient(900), water_bill(7), month(1), year(2018)).



/*Auxiliary*/

/*Create Latex file*/
latex_file(ClientId, Name, Address, SanitationfeeQuantity, SanitationfeeTotal, CubicmetersZeroQuantity, CubicmetersZeroTotal, CubicmetersSixQuantity, CubicmetersSixTotal, WaterBill, Month, Year) :-
    tell('WaterInvoice.txt'),
    write('\\newcommand{\\clientId}{'), write(ClientId), write('}\n'),
    write('\\newcommand{\\toname}{'), write(Name), write('}\n'),
    write('\\newcommand{\\toaddress}{'), write(Address), write('}\n'),
    write('\\newcommand{\\sanitationfeeQuantity}{'), write(SanitationfeeQuantity), write('}\n'),
    write('\\newcommand{\\sanitationfeeTotal}{'), write(SanitationfeeTotal), write('}\n'),
    write('\\newcommand{\\cubicmetersZeroQuantity}{'), write(CubicmetersZeroQuantity), write('}\n'),
    write('\\newcommand{\\cubicmetersZeroTotal}{'), write(CubicmetersZeroTotal), write('}\n'),
    write('\\newcommand{\\cubicmetersSixQuantity}{'), write(CubicmetersSixQuantity), write('}\n'),
    write('\\newcommand{\\cubicmetersSixTotal}{'), write(CubicmetersSixTotal), write('}\n'),
    write('\\newcommand{\\waterBill}{'), write(WaterBill), write('}\n'),
    write('\\newcommand{\\clientmonth}{'), write(Month), write('}\n'),
    write('\\newcommand{\\clientyear}{'), write(Year), write('}\n'),
    static_text_latex(),
    told.

/*Static text for latex file*/
static_text_latex() :-
    write('\n\\documentclass{scrlttr2}\n\n'),
    write('\\usepackage[utf8]{inputenc}\n'),
    write('\\usepackage[T1]{fontenc}\n'),
    write('\\usepackage{textcomp}\n'),
    write('\\usepackage{lmodern}\n'),
    write('\\usepackage{color}\n'),
    write('\\usepackage{spreadtab}\n'),
    write('\\usepackage{tabularx}\n'),
    write('\\usepackage{booktabs}\n'),
    write('\\usepackage{datetime}\n\n'),

    write('\\newcommand{\\taxID}{Tax Number: 617498954}\n'),
    write('\\renewcommand{\\familydefault}{\\sfdefault}\n\n'),

    write('\\setkomavar{subject}{Water Bill of \\monthname[\\clientmonth] \\clientyear }\n'),
    write('\\setkomavar{invoice}{\\clientyear\\clientmonth-01}\n'),
    write('\\setkomavar{date}[Date of invoice]{\\today}\n'),
    write('\\setkomavar{fromaddress}{3792 Pleasant Hill Road\\\\Musterhausen}\n'),
    write('\\setkomavar{backaddressseparator}{·}\n'),
    write('\\setkomavar{fromemail}[\\Letter~]{\\color[gray]{.3}info@watersmile.com}\n'),
    write('\\setkomavar{frommobilephone}{\\color[gray]{.3}+1\\,562\\,232\\,9486}\n'),
    write('\\setkomavar{frombank}{IBAN: 125\\,4485\\,1026\\,7843\\,4513\\,11\\\\BIC: AQDQ\\,XX\\,AQ\\,PTO\\\\meineBank}\n'),
    write('\\setkomavar{firstfoot}{\n'),
      write('  \\rule[3pt]{\\textwidth}{.4pt} \\\\ \n'),
      write('  \\parbox[t]{\\textwidth}{\\footnotesize\n'),
        write('    \\begin{tabular}[t]{@{}l}\n'),
          write('      \\usekomavar{frombank}\n'),
        write('    \\end{tabular}\n'),
        write('    \\hfill\n'),
        write('    \\begin{tabular}[t]{l@{}}\n'),
          write('      \\taxID\n'),
        write('    \\end{tabular}\n'),
      write('  }\n'),
    write('}\n\n'),

    write('\\KOMAoptions{\n'),
      write('  firsthead=true,\n'),
      write('  fromalign=right,\n'),
      write('  fromrule=false,\n'),
      write('  fromemail=true,\n'),
      write('  frommobilephone=true,\n'),
      write('  symbolicnames=true,\n'),
      write('  numericaldate=true,\n'),
      write('  firstfoot=true,\n'),
      write('  parskip=full\n'),
    write('}\n\n'),

    write('\\makeatletter\n'),
    write('\\@addtoplength{sigbeforevskip}{-1.5em}\n'),
    write('\\@addtoplength{firstfootvpos}{-2em}\n'),
    write('\\makeatother\n\n'),

    write('\\begin{document}\n'),
    write('\\begin{letter}{Nº: \\clientId\\\\Name: \\toname\\\\Addres: \\toaddress}\n'),
    write('\\opening{Water Consumption}\n\n'),

    write('\\STsetdecimalsep{{,}}\n'),
    write('\\STautoround*{2}\n'),
    write('\\begin{spreadtab}{{tabularx}{\\textwidth}{Xrrr}}\n'),
    write('\\toprule\n'),
    write('@\\multicolumn{1}{l}{Description} &\n'),
    write('@\\multicolumn{1}{l}{Price Unit} &\n'),
    write('@\\multicolumn{1}{l}{Quantity} &\n'),
    write('@\\multicolumn{1}{l}{Total}       \\\\\n'),
    write('\\midrule\n'),
    write('@Sanitation fee                                     &\n'),
    write(':={1.50}\\, & \\sanitationfeeQuantity & :=\\sanitationfeeTotal\\, \\\\ \n'),
    write('@CubicMeters (0 to 5):&\n'),
    write(':={0.50}\\, & \\cubicmetersZeroQuantity & :=\\cubicmetersZeroTotal\\, \\\\ \n'),
    write('@CubicMeters (6 to 15):&\n'),
    write(':={1.00}\\, & \\cubicmetersSixQuantity & :=\\cubicmetersSixTotal\\, \\\\ \n'),
    write('\\midrule \\midrule\n'),
    write('\\multicolumn{4}{r}{Total :=\\waterBill\\,}\\\\ \n'),
    write('\\bottomrule\n'),
    write('\\end{spreadtab}\n'),

    write('\\closing{}\n'),
    write('\\end{letter}\n'),
    write('\\end{document}').

/*Sum elements of an list*/
sum_up([],0).
sum_up([X|Y],N) :- sum_up(Y, N1), N is N1+X.

/*Construct a temp predicate*/
solutions(B, A, _) :-
	call(A),
	assert(temp(B)),
	fail.
solutions(_, _, A) :-
	construct([], A).
construct(B, C) :-
	retract(temp(A)),
	construct([A|B], C).
construct(C, C).


/*Get the current year*/
current_year(Y) :-
    get_time(T),
    stamp_date_time(T, date(Y, _, _, _, _, _, _, _, _), 'UTC').

/*Convert months numeric to text*/
month_text(M, MT) :-
    M == 1, MT = 'Jan';
    M == 2, MT = 'Feb';
    M == 3, MT = 'Mar';
    M == 4, MT = 'Apr';
    M == 5, MT = 'May';
    M == 6, MT = 'Jun';
    M == 7, MT = 'Jul';
    M == 8, MT = 'Aug';
    M == 9, MT = 'Sep';
    M == 10, MT = 'Oct';
    M == 11, MT = 'Nov';
    M == 12, MT = 'Dec'.

/*Write the consumptions of all clients*/
clients_consumption(H) :-
    solutions(G-[A,B,F,G,H],consumption(idclient(A), name(B), _, _, _, cubicmeters6_15(F), month(G), year(H)),List1),
    keysort(List1,List2),
    pairs_values(List2, List3),
    print_listA(List3).

/*Print list*/
print_listA([]).
print_listA([A|B]) :-
    nth0(3, A, Month), month_text(Month, MT), write('\n\t'), write(MT),
    nth0(0, A, ClientId), write('\t'), write(ClientId),
    nth0(1, A, Name), write('\t'), write(Name),
    nth0(2, A, Cubicmeters6_15), write('\t'), write(Cubicmeters6_15),
    print_listA(B).


/*Write the consumptions of one client in one year*/
client_consumption(A, H) :-
    solutions(G-[A,E,F,G,H],consumption(idclient(A), _, _, _, cubicmeters0_5(E), cubicmeters6_15(F), month(G), year(H)),List1),
    keysort(List1,List2),
    pairs_values(List2, List3),
    print_listB(List3).

/*Print list*/
print_listB([]).
print_listB([A|B]) :-
    nth0(3, A, Month), month_text(Month, MT), write('\n\t'), write(MT),
    nth0(1, A, Cubicmeters0_5), write('\t'), write(Cubicmeters0_5),
    nth0(2, A, Cubicmeters6_15), write('\t'), write(Cubicmeters6_15),
    print_listB(B).

/*Write the invoices of one client in one year*/
client_invoice(A, D) :-
    solutions(C-[A,B,C,D], invoice(idclient(A), water_bill(B), month(C), year(D)),List1),
    keysort(List1,List2),
    pairs_values(List2, List3),
    print_listC(List3).

/*Print list*/
print_listC([]).
print_listC([A|B]) :-
    nth0(2, A, Month), month_text(Month, MT), write('\n\t'), write(MT),
    nth0(1, A, WaterBill), write('\t'), write(WaterBill), write('€'),
    print_listC(B).

/*Print list*/
print_listD([]).
print_listD([A|B]) :-
    nth0(1, A, WaterBill), Size is (WaterBill*0.2),
    nth0(2, A, Month), month_text(Month, MT), write('  '), write(Month), write('[shape=circle, label="", style=filled, color="#006699", fillcolor="#cceeff", fixedsize=true, width='), write(Size), write(', height='), write(Size), write('];\n'),
    write('  '), write(Month), write(' -> "'), write(MT), write(' - '), write(WaterBill), write('€";\n'),
    print_listD(B).



/*Create the Graph with payment of a client*/
write_monthly_payment(WaterBill, Months, Year, IdClient, Name) :-
    tell('Graph.txt'),
    write('Digraph {\n'),
    write('  labelloc="t";\n  node[shape=plaintext];\n  edge[arrowhead=none];\n'),
    write('  graph[fontsize=20, label="Monthly Payment of '), write(Year),
    write('\\nId: '), write(IdClient), write(', Client: '), write(Name), write('"];\n'),
    /*sort(Months, MonthsSorted), sort(WaterBill, WaterBillSorted),*/
    draw_list_months(Months, WaterBill),
    write('}'),
    told.

/*Draw list of monthy payment of all months*/
draw_list_months([], []).
draw_list_months([A|B], [C|D]) :-
    draw_months(A, C),
    draw_list_months(B, D).

/*Draw the payment of one month*/
draw_months(Months, S) :-
    month_text(Months, MonthText),
    Size is (S*0.2),
    write('  '), write(Months), write('[shape=circle, label="", style=filled, color="#006699", fillcolor="#cceeff", fixedsize=true, width='), write(Size), write(', height='), write(Size), write('];\n'),
   write('  '), write(Months), write(' -> "'), write(MonthText), write(' - '), write(S), write('";\n').

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
create_invoice_latex :-
    write('Id of client:'), read(IdClient),
    write('Month:'), read(Month),
    write('Year:'), read(Year),
    consumption(idclient(A), name(B), address(C), npersons(D), cubicmeters0_5(E), cubicmeters6_15(F), month(G), year(H)),
    A=IdClient, G=Month, H=Year,
    SFT is (D*1.5), C0_5T is (0.5*E), C6_15T is (1.0*F),
    WaterBill is (SFT+C0_5T+C6_15T),
    assert(invoice(idclient(A), water_bill(WaterBill), month(G), year(H))),
    latex_file(A, B, C, D, SFT, E, C0_5T, F, C6_15T, WaterBill, G, H).

/*Calculate the average consumption for one client in one year*/
calculate_average :-
    write('Id of Client:'), read(IdClient),
    write('Year:'), read(Year),
    consumption(idclient(IdClient), name(Name), _, _, _, _, _, _),
    write('\n\tWater Consumption of '), write(Year), write(', '), write(Name),
    write('\n---------------------------------------------------------------------\n'),
    write('\tMonth\t[0,5]m³\t[6,15]m³\n'),
    client_consumption(IdClient, Year),
    write('\n---------------------------------------------------------------------\n'),
    findall(W0_5, (consumption(idclient(C), _, _, _, cubicmeters0_5(W0_5), _, _, year(Y)), C=IdClient, Y=Year), S0),
    findall(W6_15, (consumption(idclient(C), _, _, _, _, cubicmeters6_15(W6_15), _, year(Y)), C=IdClient, Y=Year), S1),
    sum_up(S0, W0_5), sum_up(S1, W6_15), WaterTotal is (W0_5+W6_15),
    WaterAverage is (WaterTotal/12),
    write('\tWater Average: '), write(WaterAverage).

/*Calculate the total payment for one client at the end of one year*/
calculate_total :-
    write('Id of client:'), read(IdClient),
    write('Year:'), read(Year),
    consumption(idclient(IdClient), name(Name), _, _, _, _, _, _),
    write('\n\tWater Bill of '), write(Year), write(', '), write(Name),
    write('\n---------------------------------------------------------------------\n'),
    write('\tMonth\tWater Bill\n'),
    client_invoice(IdClient, Year),
    write('\n---------------------------------------------------------------------\n'),
    findall(T, (invoice(idclient(A), water_bill(T), _, year(B)), A=IdClient, B=Year), S),
    sum_up(S, WaterTotal),
    write('\tWater Bill: '), write(WaterTotal), write('€').

/*Total number of cubicmeters consummated over 5 cubicmeters of all clients in the last year*/
over_5cubicmeters :-
    current_year(Year), /*LastYear is (Year-1),*/
    Last_Year is (Year-1),
    write('\n\tOver 5 cubicmeters of '), write(Last_Year),
    write('\n---------------------------------------------------------------------\n'),
    write('\tMonth\tId\tName\t[6,15]m³\n'),
    clients_consumption(Last_Year),
    write('\n---------------------------------------------------------------------\n'),
    findall(W, (consumption(_, _, _, _, _, cubicmeters6_15(W), _, year(Y)), Y=Last_Year), S),
    sum_up(S, WaterTotal),
    write('\tWater Total: '), write(WaterTotal), write('m³').

/*Generate a graph connecting the monthly payment of a client during one year*/
graph_payment :-
    write('Id of client:'), read(A),
    write('Year:'), read(D),
    consumption(idclient(A), name(Name), _, _, _, _, _, _),
    solutions(C-[A,B,C,D], invoice(idclient(A), water_bill(B), month(C), year(D)),List1),
    keysort(List1,List2),
    pairs_values(List2, List3),
    tell('Graph.txt'),
    write('Digraph {\n'),
    write('  labelloc="t";\n  node[shape=plaintext];\n  edge[arrowhead=none];\n'),
    write('  graph[fontsize=20, label="Monthly Payment of '), write(D),
    write('\\nId: '), write(A), write(', Client: '), write(Name), write('"];\n'),
    print_listD(List3),
    write('}'),
    told.

/*Save*/
save :-
    tell('WaterConsumptionInvoices.txt'),
    listing,
    told.


/*Start the program*/
start :- menu.

/*Shows the menu*/
menu :-
    write('\n---------------------------------------------------------------------\n'),
    write('-- 1. Insert the consumption of one client in one month            --\n'),
    write('-- 2. Create the invoice of a client in Latex                      --\n'),
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
    A==2, create_invoice_latex, nl, menu;
    A==3, calculate_average, nl, menu;
    A==4, calculate_total, nl, menu;
    A==5, over_5cubicmeters, nl, menu;
    A==6, graph_payment, nl, menu;
    A==0, save).
