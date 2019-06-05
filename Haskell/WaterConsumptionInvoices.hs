type Idclient = Int
type Name = String
type Address = String
type Npersons = Int
type Cubicmeters0_5 = Float
type Cubicmeters6_15 = Float
type Month = Int
type Year = Int
type Water_bill = Float

type Consumption = (Idclient, Name, Address, Npersons, Cubicmeters0_5, Cubicmeters6_15, Month, Year)
type Invoice = (Idclient, Water_bill, Month, Year)

type ListConsumption = [Consumption]
type ListInvoice = [Invoice]

listConsumption :: ListConsumption
listConsumption = []
listInvoice :: ListInvoice
listInvoice = []

gerListWaterConsumption :: [[String]] -> ListConsumption
gerListWaterConsumption [] = []
gerListWaterConsumption ([idclient, name, address, npersons, cubicmeters0_5, cubicmeters6_15, month, year]: xs) = (read(idclient)::Int, name, address, read(npersons)::Int, read(cubicmeters0_5)::Float, read(cubicmeters6_15)::Float, read(month)::Int, read(year)::Int): (gerListWaterConsumption xs)
gerListWaterInvoice :: [[String]] -> ListInvoice
gerListWaterInvoice [] = []
gerListWaterInvoice ([idclient, water_bill, month, year]: xs) = (read(idclient)::Int, read(water_bill)::Float, read(month)::Int, read(year)::Int): (gerListWaterInvoice xs)

loadWaterConsumption :: IO ListConsumption
loadWaterConsumption = do s <- readFile "WaterConsumption.txt" 
                          return (gerListWaterConsumption (map words (lines s)))
loadWaterInvoice :: IO ListInvoice
loadWaterInvoice = do s <- readFile "WaterInvoice.txt" 
                      return (gerListWaterInvoice (map words (lines s)))

printListWaterConsumption [] = ""
printListWaterConsumption ((idclient, name, address, npersons, cubicmeters0_5, cubicmeters6_15, month, year): xs) = idclient ++ "\t" ++ name ++ "\t" ++ address ++ "\t" ++ npersons ++ "\t" ++ cubicmeters0_5 ++ "\t" ++ cubicmeters6_15 ++ "\t" ++ month ++ "\t" ++ year ++ "\n" ++ (printListWaterConsumption xs)
printListWaterInvoice [] = ""
printListWaterInvoice ((idclient, water_bill, month, year): xs) = idclient ++ "\t" ++ water_bill ++ "\t" ++ month ++ "\t" ++ year ++ "\n" ++ (printListWaterInvoice xs)

-- Find a water consumption
findWaterConsumption :: (Idclient, Month, Year) -> ListConsumption -> IO()
findWaterConsumption (idclient, month, year) [] = error "ERROR" 
findWaterConsumption (idclient, month, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && month == g && year == h) = do let sft = (1.5* (fromIntegral d))
                                                      let c0_5t = (0.5*e) 
                                                      let c6_15t = (1.0*f) 
                                                      let water_bill = (sft + c0_5t + c6_15t) 
                                                      insertInvoice idclient water_bill g h
                                                      latexFile a b c d sft e c0_5t f c6_15t water_bill g h
    | otherwise = findWaterConsumption (idclient, month, year) xs

insertInvoice :: Idclient -> Water_bill -> Month -> Year -> IO()
insertInvoice idclient water_bill month year = do appendFile "WaterInvoice.txt" (show(idclient) ++ "\t" ++ show(water_bill) ++ "\t" ++ show(month) ++ "\t" ++ show(year) ++ "\n")

-- Create Latex file
latexFile :: Idclient -> Name -> Address -> Npersons -> Float -> Float -> Float -> Float -> Float -> Float -> Month -> Year -> IO()
latexFile a b c d sft e c0_5t f c6_15t water_bill g h = do writeFile "WaterInvoiceLatex.txt" ("\\newcommand{\\clientId}{" ++ show(a) ++ "}\n" ++ 
                                                            "\\newcommand{\\toname}{" ++ b ++ "}\n" ++ 
                                                            "\\newcommand{\\toaddress}{" ++ c ++ "}\n" ++ 
                                                            "\\newcommand{\\sanitationfeeQuantity}{" ++ show(d) ++ "}\n" ++ 
                                                            "\\newcommand{\\sanitationfeeTotal}{" ++ show(sft) ++ "}\n" ++ 
                                                            "\\newcommand{\\cubicmetersZeroQuantity}{" ++ show(e) ++ "}\n" ++ 
                                                            "\\newcommand{\\cubicmetersZeroTotal}{" ++ show(c0_5t) ++ "}\n" ++ 
                                                            "\\newcommand{\\cubicmetersSixQuantity}{" ++ show(f) ++ "}\n" ++
                                                            "\\newcommand{\\cubicmetersSixTotal}{" ++ show(c6_15t) ++ "}\n" ++ 
                                                            "\\newcommand{\\waterBill}{" ++ show(water_bill) ++ "}\n" ++ 
                                                            "\\newcommand{\\clientmonth}{" ++ show(g) ++ "}\n" ++ 
                                                            "\\newcommand{\\clientyear}{" ++ show(h) ++ "}\n" ++

                                                            "\n\\documentclass{scrlttr2}\n\n" ++

                                                            "\\usepackage[utf8]{inputenc}\n" ++ 
                                                            "\\usepackage[T1]{fontenc}\n" ++ 
                                                            "\\usepackage{textcomp}\n" ++ 
                                                            "\\usepackage{lmodern}\n" ++ 
                                                            "\\usepackage{color}\n" ++ 
                                                            "\\usepackage{spreadtab}\n" ++ 
                                                            "\\usepackage{tabularx}\n" ++ 
                                                            "\\usepackage{booktabs}\n" ++ 
                                                            "\\usepackage{datetime}\n\n" ++ 

                                                            "\\newcommand{\\taxID}{Tax Number: 617498954}\n" ++ 
                                                            "\\renewcommand{\\familydefault}{\\sfdefault}\n\n" ++ 

                                                            "\\setkomavar{subject}{Water Bill of \\monthname[\\clientmonth] \\clientyear }\n" ++ 
                                                            "\\setkomavar{invoice}{\\clientyear\\clientmonth-01}\n" ++ 
                                                            "\\setkomavar{date}[Date of invoice]{\\today}\n" ++ 
                                                            "\\setkomavar{fromaddress}{3792 Pleasant Hill Road\\\\Musterhausen}\n" ++ 
                                                            "\\setkomavar{backaddressseparator}{-}\n" ++ 
                                                            "\\setkomavar{fromemail}[\\Letter~]{\\color[gray]{.3}info@watersmile.com}\n" ++ 
                                                            "\\setkomavar{frommobilephone}{\\color[gray]{.3}+1\\,562\\,232\\,9486}\n" ++ 
                                                            "\\setkomavar{frombank}{IBAN: 125\\,4485\\,1026\\,7843\\,4513\\,11\\\\BIC: AQDQ\\,XX\\,AQ\\,PTO\\\\meineBank}\n" ++ 
                                                            "\\setkomavar{firstfoot}{\n" ++ 
                                                            "  \\rule[3pt]{\\textwidth}{.4pt} \\\\ \n" ++ 
                                                            "  \\parbox[t]{\\textwidth}{\\footnotesize\n" ++ 
                                                                "    \\begin{tabular}[t]{@{}l}\n" ++ 
                                                                "      \\usekomavar{frombank}\n" ++ 
                                                                "    \\end{tabular}\n" ++ 
                                                                "    \\hfill\n" ++ 
                                                                "    \\begin{tabular}[t]{l@{}}\n" ++ 
                                                                "      \\taxID\n" ++ 
                                                                "    \\end{tabular}\n" ++ 
                                                            "  }\n" ++ 
                                                            "}\n\n" ++ 

                                                            "\\KOMAoptions{\n" ++ 
                                                            "  firsthead=true,\n" ++ 
                                                            "  fromalign=right,\n" ++ 
                                                            "  fromrule=false,\n" ++ 
                                                            "  fromemail=true,\n" ++ 
                                                            "  frommobilephone=true,\n" ++ 
                                                            "  symbolicnames=true,\n" ++ 
                                                            "  numericaldate=true,\n" ++ 
                                                            "  firstfoot=true,\n" ++ 
                                                            "  parskip=full\n" ++ 
                                                            "}\n\n" ++ 

                                                            "\\makeatletter\n" ++ 
                                                            "\\@addtoplength{sigbeforevskip}{-1.5em}\n" ++ 
                                                            "\\@addtoplength{firstfootvpos}{-2em}\n" ++ 
                                                            "\\makeatother\n\n" ++ 

                                                            "\\begin{document}\n" ++ 
                                                            "\\begin{letter}{N: \\clientId\\\\Name: \\toname\\\\Addres: \\toaddress}\n" ++ 
                                                            "\\opening{Water Consumption}\n\n" ++ 

                                                            "\\STsetdecimalsep{{,}}\n" ++ 
                                                            "\\STautoround*{2}\n" ++ 
                                                            "\\begin{spreadtab}{{tabularx}{\\textwidth}{Xrrr}}\n" ++ 
                                                            "\\toprule\n" ++ 
                                                            "@\\multicolumn{1}{l}{Description} &\n" ++ 
                                                            "@\\multicolumn{1}{l}{Price Unit} &\n" ++ 
                                                            "@\\multicolumn{1}{l}{Quantity} &\n" ++ 
                                                            "@\\multicolumn{1}{l}{Total}       \\\\\n" ++ 
                                                            "\\midrule\n" ++ 
                                                            "@Sanitation fee                                     &\n" ++ 
                                                            ":={1.50}\\, & \\sanitationfeeQuantity & :=\\sanitationfeeTotal\\, \\\\ \n" ++ 
                                                            "@CubicMeters (0 to 5):&\n" ++ 
                                                            ":={0.50}\\, & \\cubicmetersZeroQuantity & :=\\cubicmetersZeroTotal\\, \\\\ \n" ++ 
                                                            "@CubicMeters (6 to 15):&\n" ++ 
                                                            ":={1.00}\\, & \\cubicmetersSixQuantity & :=\\cubicmetersSixTotal\\, \\\\ \n" ++ 
                                                            "\\midrule \\midrule\n" ++ 
                                                            "\\multicolumn{4}{r}{Total :=\\waterBill\\,}\\\\ \n" ++ 
                                                            "\\bottomrule\n" ++ 
                                                            "\\end{spreadtab}\n" ++ 

                                                            "\\closing{}\n" ++ 
                                                            "\\end{letter}\n" ++ 
                                                            "\\end{document}")

-- findName :: (Idclient, Year) -> ListConsumption -> IO()
findName (idclient, year) [] = "" 
findName (idclient, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && year == h) = b
    | otherwise = findName (idclient, year) xs

-- printListConsumptionYear :: (Idclient, Year) -> ListConsumption -> IO()
printListConsumptionYear (idclient, year) [] = "" 
printListConsumptionYear (idclient, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && year == h) = "\t" ++ show(g) ++ "\t\t" ++ show(e) ++ "\t\t" ++ show(f) ++ "\n" ++ printListConsumptionYear (idclient, year) xs
    | otherwise = printListConsumptionYear (idclient, year) xs

totalConsumptionYear (idclient, year) [] = 0
totalConsumptionYear (idclient, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && year == h) = e + f + totalConsumptionYear (idclient, year) xs
    | otherwise = totalConsumptionYear (idclient, year) xs

printListWaterBillYear (idclient, year) [] = "" 
printListWaterBillYear (idclient, year) ((a, b, c, d): xs)
    | (idclient == a && year == d) = "\t" ++ show(c) ++ "\t\t" ++ show(b) ++ "\n" ++ printListWaterBillYear (idclient, year) xs
    | otherwise = printListWaterBillYear (idclient, year) xs

totalWaterBillYear (idclient, year) [] = 0
totalWaterBillYear (idclient, year) ((a, b, c, d): xs)
    | (idclient == a && year == d) = b + totalWaterBillYear (idclient, year) xs
    | otherwise = totalWaterBillYear (idclient, year) xs

printListOver5CubicmetersYear (year) [] = "" 
printListOver5CubicmetersYear (year) ((a, b, c, d, e, f, g, h): xs)
    | (year == h) = "\t" ++ show(g) ++ "\t\t" ++ show(a) ++ "\t\t" ++ show(b) ++ "\t" ++ show(f) ++ "\n" ++ printListOver5CubicmetersYear (year) xs
    | otherwise = printListOver5CubicmetersYear (year) xs

totalOver5CubicmetersYear (year) [] = 0
totalOver5CubicmetersYear (year) ((a, b, c, d, e, f, g, h): xs)
    | (year == h) = f + totalOver5CubicmetersYear (year) xs
    | otherwise = totalOver5CubicmetersYear (year) xs

listWaterBillYear :: (Idclient, Month, Name) -> ListInvoice -> IO()
listWaterBillYear (idclient, year, name) [] = error "ERROR" 
listWaterBillYear (idclient, year, name) ((a, b, c, d): xs)
    | (idclient == a && year == d) = do graph (idclient, name, year)
                                        printListInvoiceYear (idclient, year) ((a, b, c, d): xs) 
                                        appendFile "Graph.txt" ("}")                                      
    | otherwise = listWaterBillYear (idclient, year, name) xs

-- graph :: Idclient -> Water_bill -> Month -> Year -> IO()
graph (idclient, name, year) = do writeFile "Graph.txt" ("Digraph {\n" ++ 
                                                        "  labelloc=\"t\";\n  node[shape=plaintext];\n  edge[arrowhead=none];\n" ++ 
                                                        "  graph[fontsize=20, label=\"Monthly Payment of " ++  
                                                        show(year) ++ "\\nId: " ++ show(idclient) ++ ", Client: " ++ name ++ "\"];\n")

printListInvoiceYear :: (Idclient, Year) -> ListInvoice -> IO()
printListInvoiceYear (idclient, year) [] = error "ERROR" 
printListInvoiceYear (idclient, year) ((a, b, c, d): xs)
    | (idclient == a && year == d) = do let size = b*0.2
                                        appendFile "Graph.txt" ("  " ++ show(c) ++ 
                                                                "[shape=circle, label=\"\", style=filled, color=\"#006699\", fillcolor=\"#cceeff\", fixedsize=true, width=" ++ 
                                                                show(size) ++ ", height=" ++ show(size) ++ "];\n" ++ 
                                                                "  " ++ show(c) ++ " -> \"" ++ show(c) ++ " - " ++ show(b) ++ "€\";\n")
                                        printListInvoiceYear (idclient, year) xs
    | otherwise = printListInvoiceYear (idclient, year) xs

-- Insert the consumption of one client in one month
insertConsumption :: IO()
insertConsumption = do putStr "\nClient Id: " 
                       idclient <- getLine
                       putStr "Name: " 
                       name <- getLine
                       putStr "Address: " 
                       address <- getLine
                       putStr "Number of Persons: " 
                       npersons <- getLine
                       putStr "CubicMeters (0 to 5): " 
                       cubicmeters0_5 <- getLine
                       putStr "CubicMeters (6 to 15): " 
                       cubicmeters6_15 <- getLine
                       putStr "Month: " 
                       month <- getLine
                       putStr "Year: " 
                       year <- getLine
                       appendFile "WaterConsumption.txt" (idclient ++ "\t" ++ name ++ "\t" ++ address ++ "\t" ++ npersons ++ "\t" ++ cubicmeters0_5 ++ "\t" ++ cubicmeters6_15 ++ "\t" ++ month ++ "\t" ++ year ++ "\n")
                       putStr "Insert another one? (y/Y): " 
                       op <- getLine
                       if (op=="y" || op=="Y") then insertConsumption else return()


-- Create the invoice of a client
createInvoiceLatex :: ListConsumption -> IO()
createInvoiceLatex listConsumption = do putStr "Id of client: "
                                        idclient <- getLine
                                        putStr "Month: "
                                        month <- getLine
                                        putStr "Year: "
                                        year <- getLine 
                                        findWaterConsumption (read(idclient)::Idclient, read(month)::Month, read(year)::Year) listConsumption

-- Calculate the average consumption for one client in one year
calculateAverage listConsumption = do putStr "Id of client: "
                                      idclient <- getLine
                                      putStr "Year: "
                                      year <- getLine 
                                      let name = findName (read(idclient)::Idclient, read(year)::Year) listConsumption
                                      putStr ("\n\tWater Consumption of " ++ year ++ ", " ++ name ++ 
                                            "\n---------------------------------------------------------------------\n" ++ 
                                            "\tMonth\t\t[0,5]m³\t[6,15]m³\n\n")
                                      putStr (printListConsumptionYear (read(idclient)::Idclient, read(year)::Year) listConsumption)
                                      putStr "---------------------------------------------------------------------\n\tWater Average: "
                                      putStr (show (totalConsumptionYear (read(idclient)::Idclient, read(year)::Year) listConsumption/12))

-- 
calculateTotal listConsumption listInvoice = do putStr "Id of client: "
                                                idclient <- getLine
                                                putStr "Year: "
                                                year <- getLine
                                                let name = findName (read(idclient)::Idclient, read(year)::Year) listConsumption
                                                putStr ("\n\tWater Bill of " ++ year ++ ", " ++ name ++ 
                                                        "\n---------------------------------------------------------------------\n" ++ 
                                                        "\tMonth\t\tWater Bill\n\n")
                                                putStr(printListWaterBillYear (read(idclient)::Idclient, read(year)::Year) listInvoice)
                                                putStr "---------------------------------------------------------------------\n\tWater Bill: "
                                                putStr (show (totalWaterBillYear (read(idclient)::Idclient, read(year)::Year) listInvoice) ++ "€")

over5cubicmeters listConsumption = do let lastYear = "2019"
                                      putStr ("\n\tOver 5 cubicmeters of " ++ lastYear ++ 
                                              "\n---------------------------------------------------------------------\n" ++ 
                                              "\tMonth\t\tId\t\tName\t\t[6,15]m³\n\n")
                                      putStr (printListOver5CubicmetersYear (read(lastYear)::Year) listConsumption)
                                      putStr "---------------------------------------------------------------------\n\tWater Total: "
                                      putStr (show (totalOver5CubicmetersYear (read(lastYear)::Year) listConsumption) ++ "m³")

graphPayment listConsumption listInvoice = do putStr "Id of client: "
                                              idclient <- getLine
                                              putStr "Year: "
                                              year <- getLine
                                              let name = findName (read(idclient)::Idclient, read(year)::Year) listConsumption
                                              listWaterBillYear (read(idclient)::Idclient, read(year)::Year, name) listInvoice



-- Start program and shows the menu
main :: IO()
main = do listConsumption <- loadWaterConsumption 
          print listConsumption
          listInvoice <- loadWaterInvoice 
          print listInvoice
          putStr "\n---------------------------------------------------------------------\n" 
          putStr "-- 1. Insert the consumption of one client in one month            --\n" 
          putStr "-- 2. Create the invoice of a client in Latex                      --\n" 
          putStr "-- 3. Average consumption for one client in one year               --\n" 
          putStr "-- 4. Total payment for one client at the end of one year          --\n" 
          putStr "-- 5. Total of over 5 cubicmeters of all clients in the last year  --\n" 
          putStr "-- 6. Graph of a monthly payment of a client during one year       --\n" 
          putStr "---------------------------------------------------------------------\n\n|: "
          op <- getLine 
          if (op=="1") then do insertConsumption                                 
          else if (op=="2") then do createInvoiceLatex listConsumption
          else if (op=="3") then do calculateAverage listConsumption
          else if (op=="4") then do calculateTotal listConsumption listInvoice
          else if (op=="5") then do over5cubicmeters listConsumption
          else if (op=="6") then do graphPayment listConsumption listInvoice
          -- else if (op=="0") then do 
          else error "Wrong option"
          putStr "\n\nDo you want to continue? (y/Y): "
          op <- getLine
          if (op=="y" || op=="Y") then main else return()