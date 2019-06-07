-- Import modules for calculate current date
import Data.Time.Clock
import Data.Time.Calendar

-- Types used in water consumption and invoice
type Idclient = Int
type Name = String
type Address = String
type Npersons = Int
type Cubicmeters0_5 = Float
type Cubicmeters6_15 = Float
type Month = Int
type Year = Int
type Water_bill = Float

-- Types of water consumption and invoice
type Consumption = (Idclient, Name, Address, Npersons, Cubicmeters0_5, Cubicmeters6_15, Month, Year)
type Invoice = (Idclient, Water_bill, Month, Year)

-- Types of water consumption list and invoice list
type ListConsumption = [Consumption]
type ListInvoice = [Invoice]

-- Water consumption list and invoice list
listConsumption :: ListConsumption
listConsumption = []
listInvoice :: ListInvoice
listInvoice = []

-- Generate list of water consumption and invoice
gerListWaterConsumption :: [[String]] -> ListConsumption
gerListWaterConsumption [] = []
gerListWaterConsumption ([a,b,c,d,e,f,g,h]: xs) = (read(a)::Int, b, c, read(d)::Int, read(e)::Float, read(f)::Float, read(g)::Int, read(h)::Int): (gerListWaterConsumption xs)
gerListWaterInvoice :: [[String]] -> ListInvoice
gerListWaterInvoice [] = []
gerListWaterInvoice ([a,b,c,d]: xs) = (read(a)::Int, read(b)::Float, read(c)::Int, read(d)::Int): (gerListWaterInvoice xs)

-- Load water consumption and invoice
loadWaterConsumption :: IO ListConsumption
loadWaterConsumption = do s <- readFile "WaterConsumption.txt" 
                          return (gerListWaterConsumption (map words (lines s)))
loadWaterInvoice :: IO ListInvoice
loadWaterInvoice = do s <- readFile "WaterInvoice.txt" 
                      return (gerListWaterInvoice (map words (lines s)))

-- Sort list of water comsumption and invoice by id of client
sortConsumptionsId [] = []
sortConsumptionsId ((a,b,c,d,e,f,g,h):xs) = insertConsumptionsId (a,b,c,d,e,f,g,h) (sortConsumptionsId xs)
insertConsumptionsId (a,b,c,d,e,f,g,h) [] = [(a,b,c,d,e,f,g,h)]
insertConsumptionsId (a,b,c,d,e,f,g,h) ((a1,b1,c1,d1,e1,f1,g1,h1):ys) = if a<a1 then (a,b,c,d,e,f,g,h):(a1,b1,c1,d1,e1,f1,g1,h1):ys
                                                                                else (a1,b1,c1,d1,e1,f1,g1,h1):(insertConsumptionsId (a,b,c,d,e,f,g,h) ys)
sortInvoicesId [] = []
sortInvoicesId ((a,b,c,d):xs) = insertInvoicesId (a,b,c,d) (sortInvoicesId xs)
insertInvoicesId (a,b,c,d) [] = [(a,b,c,d)]
insertInvoicesId (a,b,c,d) ((a1,b1,c1,d1):ys) = if a<a1 then (a,b,c,d):(a1,b1,c1,d1):ys
                                                        else (a1,b1,c1,d1):(insertInvoicesId (a,b,c,d) ys)

-- Sort list of water comsumption and invoice by month
sortConsumptions [] = []
sortConsumptions ((a,b,c,d,e,f,g,h):xs) = insertConsumptions (a,b,c,d,e,f,g,h) (sortConsumptions xs)
insertConsumptions (a,b,c,d,e,f,g,h) [] = [(a,b,c,d,e,f,g,h)]
insertConsumptions (a,b,c,d,e,f,g,h) ((a1,b1,c1,d1,e1,f1,g1,h1):ys) = if g<g1 then (a,b,c,d,e,f,g,h):(a1,b1,c1,d1,e1,f1,g1,h1):ys
                                                                              else (a1,b1,c1,d1,e1,f1,g1,h1):(insertConsumptions (a,b,c,d,e,f,g,h) ys)
sortInvoices [] = []
sortInvoices ((a,b,c,d):xs) = insertInvoices (a,b,c,d) (sortInvoices xs)
insertInvoices (a,b,c,d) [] = [(a,b,c,d)]
insertInvoices (a,b,c,d) ((a1,b1,c1,d1):ys) = if c<c1 then (a,b,c,d):(a1,b1,c1,d1):ys
                                                      else (a1,b1,c1,d1):(insertInvoices (a,b,c,d) ys)

-- Show lists of water consumption and invoice
printListWaterConsumption [] = ""
printListWaterConsumption ((idclient, name, address, npersons, cubicmeters0_5, cubicmeters6_15, month, year): xs) =
    "[" ++ show(idclient) ++ "]\t\t" ++ name ++ "\t\t" ++ monthToText month ++ "\t\t" ++ show(year) ++ "\n" ++ (printListWaterConsumption xs)
printListWaterInvoice [] = ""
printListWaterInvoice ((idclient, water_bill, month, year): xs) = 
    "[" ++ show(idclient) ++ "]\t\t" ++ monthToText month ++ "\t\t" ++ show(year) ++ "\n" ++ (printListWaterInvoice xs)

-- Convert month numeric to text
monthToText :: Month -> String
monthToText month
    | (month == 1) = "Jan"
    | (month == 2) = "Fev"
    | (month == 3) = "Mar"
    | (month == 4) = "Apr"
    | (month == 5) = "May"
    | (month == 6) = "Jun"
    | (month == 7) = "Jul"
    | (month == 8) = "Aug"
    | (month == 9) = "Sep"
    | (month == 10) = "Oct"
    | (month == 11) = "Nov"
    | (month == 12) = "Dec"

-- Find the name of a client by id
findName :: (Idclient) -> ListConsumption -> String
findName (idclient) [] = "" 
findName (idclient) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a) = b
    | otherwise = findName (idclient) xs

-- Validate the consumption and the invoice
validateConsumption :: (Idclient, Name, Address, Npersons, Cubicmeters0_5, Cubicmeters6_15, Month, Year) -> Bool
validateConsumption (a, b, c, d, e, f, g, h)
    | (a<=0) = False
    | (d<=0) = False
    | (e<0 || e>5) = False
    | (f<0 || f>5) = False
    | (e<5 && f>0) = False
    | (g<=0 || g>12) = False
    | (h<=0) = False 
    | otherwise = True
validateInvoice :: (Idclient, Water_bill, Month, Year) -> Bool
validateInvoice (a, b, c, d)
    | (a<=0) = False
    | (b<0) = False
    | (c<=0 || c>12) = False
    | (d<=0) = False 
    | otherwise = True
   
------------------------------------------------------------------------------
--                               Não funciona                               --
------------------------------------------------------------------------------

-- Check if a register not exists in list of consumptions
ifNotExistsConsumption :: (Idclient, Month, Year) -> ListConsumption -> Bool
ifNotExistsConsumption (a0, g0, h0) [] = True
ifNotExistsConsumption (a0, g0, h0) ((a, b, c, d, e, f, g, h): xs) =
    if(a0==a && g0==g && h0==h) then False
    else ifNotExistsConsumption (a0, g0, h0) xs
-- Check if a register not exists in list of invoices
ifNotExistsInvoice :: (Idclient, Month, Year) -> ListInvoice -> Bool
ifNotExistsInvoice (a0, g0, h0) [] = True
ifNotExistsInvoice (a0, c0, d0) ((a, b, c, d): xs) = 
    if(a0==a && c0==c && d0==d) then False
    else ifNotExistsInvoice (a0, c0, d0) xs
------------------------------------------------------------------------------

-- Find a water consumption and create the invoice
createInvoice :: (Idclient, Month, Year) -> ListConsumption -> IO()
createInvoice (idclient, month, year) [] = error "ERROR" 
createInvoice (idclient, month, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && month == g && year == h) = do let sft = (1.5* (fromIntegral d))
                                                      let c0_5t = (0.5*e) 
                                                      let c6_15t = (1.0*f) 
                                                      let water_bill = (sft + c0_5t + c6_15t)
                                                      if(validateInvoice (idclient, water_bill, g, h)) then do
                                                        insertInvoice idclient water_bill g h
                                                        latexFile a b c d sft e c0_5t f c6_15t water_bill g h
                                                      else error "ERROR: please check the values"
    | otherwise = createInvoice (idclient, month, year) xs

-- Insert invoice in listInvoice
insertInvoice :: Idclient -> Water_bill -> Month -> Year -> IO()
insertInvoice idclient water_bill month year = do appendFile "WaterInvoice.txt" (show(idclient) ++ "\t" ++ show(water_bill) ++ "\t" ++ show(month) ++ "\t" ++ show(year) ++ "\n")

-- Create Latex file
latexFile :: Idclient -> Name -> Address -> Npersons -> Float -> Float -> Float -> Float -> Float -> Float -> Month -> Year -> IO()
latexFile a b c d sft e c0_5t f c6_15t water_bill g h = do writeFile "WaterInvoiceLatex.tex" ("\\newcommand{\\clientId}{" ++ show(a) ++ "}\n" ++ 
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

-- Show the list of water consumption in a year
printListConsumptionYear :: (Idclient, Year) -> ListConsumption -> String
printListConsumptionYear (idclient, year) [] = "" 
printListConsumptionYear (idclient, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && year == h) = "\t" ++ monthToText g ++ "\t\t" ++ show(e) ++ "\t\t" ++ show(f) ++ "\n" ++ printListConsumptionYear (idclient, year) xs
    | otherwise = printListConsumptionYear (idclient, year) xs

-- Total of a consumption of water in a year
totalConsumptionYear :: (Idclient, Year) -> ListConsumption -> Float
totalConsumptionYear (idclient, year) [] = 0
totalConsumptionYear (idclient, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && year == h) = e + f + totalConsumptionYear (idclient, year) xs
    | otherwise = totalConsumptionYear (idclient, year) xs

-- Show the list of water bill in a year
printListWaterBillYear :: (Idclient, Year) -> ListInvoice -> String
printListWaterBillYear (idclient, year) [] = "" 
printListWaterBillYear (idclient, year) ((a, b, c, d): xs)
    | (idclient == a && year == d) = "\t" ++ monthToText c ++ "\t\t" ++ show(b) ++ "€\n" ++ printListWaterBillYear (idclient, year) xs
    | otherwise = printListWaterBillYear (idclient, year) xs

-- Total of a water bill in a year
totalWaterBillYear :: (Idclient, Year) -> ListInvoice -> Float
totalWaterBillYear (idclient, year) [] = 0
totalWaterBillYear (idclient, year) ((a, b, c, d): xs)
    | (idclient == a && year == d) = b + totalWaterBillYear (idclient, year) xs
    | otherwise = totalWaterBillYear (idclient, year) xs

-- Show the list of water consumption over 5 cubicmeters in a year
printListOver5CubicmetersYear :: (Year) -> ListConsumption -> String
printListOver5CubicmetersYear (year) [] = "" 
printListOver5CubicmetersYear (year) ((a, b, c, d, e, f, g, h): xs)
    | (year == h) = "\t" ++ monthToText g ++ "\t\t" ++ show(a) ++ "\t\t" ++ b ++ "\t\t" ++ show(f) ++ "\n" ++ printListOver5CubicmetersYear (year) xs
    | otherwise = printListOver5CubicmetersYear (year) xs

-- Total of water consumption over 5 cubicmeters in a year
totalOver5CubicmetersYear :: (Year) -> ListConsumption -> Float
totalOver5CubicmetersYear (year) [] = 0
totalOver5CubicmetersYear (year) ((a, b, c, d, e, f, g, h): xs)
    | (year == h) = f + totalOver5CubicmetersYear (year) xs
    | otherwise = totalOver5CubicmetersYear (year) xs

-- Get the list of water bill for one client in a year and write a Dot file
listWaterBillYear :: (Idclient, Year, Name) -> ListInvoice -> IO()
listWaterBillYear (idclient, year, name) [] = error "ERROR" 
listWaterBillYear (idclient, year, name) ((a, b, c, d): xs)
    | (idclient == a && year == d) = do writeFile "Graph.dot" (graph (idclient, name, year))                                        
                                        appendFile "Graph.dot" (printListInvoiceYear (idclient, year) ((a, b, c, d): xs)) 
                                        appendFile "Graph.dot" ("}")                                      
    | otherwise = listWaterBillYear (idclient, year, name) xs

-- Write the graph
graph :: (Idclient, Name, Year) -> String
graph (idclient, name, year) = "Digraph {\n" ++ 
                                "  labelloc=\"t\";\n  node[shape=plaintext];\n  edge[arrowhead=none];\n" ++ 
                                "  graph[fontsize=20, label=\"Monthly Payment of " ++  
                                show(year) ++ "\\nId: " ++ show(idclient) ++ ", Client: " ++ name ++ "\"];\n"

-- Show the list of invoice in a year
printListInvoiceYear :: (Idclient, Year) -> ListInvoice -> String
printListInvoiceYear (idclient, year) [] = ""
printListInvoiceYear (idclient, year) ((a, b, c, d): xs)
    | (idclient == a && year == d) = "  " ++ show(c) ++ 
                                     "[shape=circle, label=\"\", style=filled, color=\"#006699\", fillcolor=\"#cceeff\", fixedsize=true, width=" ++ 
                                     show(b*0.2) ++ ", height=" ++ show(b*0.2) ++ "];\n" ++ 
                                     "  " ++ show(c) ++ " -> \"" ++ monthToText c ++ " - " ++ show(b) ++ " Eur\";\n" ++
                                     printListInvoiceYear (idclient, year) xs                                  
    | otherwise = printListInvoiceYear (idclient, year) xs


-- Insert the consumption of one client in one month in listConsumption
insertConsumption :: IO()
insertConsumption = do putStr "\nClient Id (ex. 1000): " 
                       idclient <- getLine
                       putStr "Name (ex. John): " 
                       name <- getLine
                       putStr "Address (ex. Lisbon): " 
                       address <- getLine
                       putStr "Number of Persons (ex. 1): " 
                       npersons <- getLine
                       putStr "CubicMeters [0,5] (ex. 3.5): " 
                       cubicmeters0_5 <- getLine
                       putStr "CubicMeters [6,15] (ex. 2.4): " 
                       cubicmeters6_15 <- getLine
                       putStr "Month (ex. 3): " 
                       month <- getLine
                       putStr "Year (ex. 2018): " 
                       year <- getLine
                       {-let notExists = ifNotExistsConsumption (read(idclient)::Idclient, read(month)::Month, read(year)::Year) listConsumption-}
                       let checkValues = validateConsumption (read(idclient)::Idclient, read(name)::Name, read(address)::Address, read(npersons)::Npersons, read(cubicmeters0_5)::Cubicmeters0_5, read(cubicmeters6_15)::Cubicmeters6_15, read(month)::Month, read(year)::Year)
                       if({-notExists &&-} checkValues) then do appendFile "WaterConsumption.txt" (idclient ++ "\t" ++ name ++ "\t" ++ address ++ "\t" ++ npersons ++ "\t" ++ cubicmeters0_5 ++ "\t" ++ cubicmeters6_15 ++ "\t" ++ month ++ "\t" ++ year ++ "\n")
                                                                putStr "Insert another one? (y/Y): " 
                                                                op <- getLine
                                                                if (op=="y" || op=="Y") then insertConsumption 
                                                                else return()
                       else error "ERROR: please check the values"
                       


-- Create the invoice of a client in latex and insert in listInvoice
createInvoiceLatex :: ListConsumption -> IO()
createInvoiceLatex listConsumption = do putStr "Id of client (ex. 1000): "
                                        idclient <- getLine
                                        putStr "Month (ex. 3): "
                                        month <- getLine
                                        putStr "Year (ex. 2018): "
                                        year <- getLine 
                                        {-let notExists = ifNotExistsInvoice (read(idclient)::Idclient, read(month)::Month, read(year)::Year) listInvoice-}
                                        createInvoice (read(idclient)::Idclient, read(month)::Month, read(year)::Year) listConsumption

-- Calculate the average consumption for one client in one year
calculateAverage :: ListConsumption -> IO()
calculateAverage listConsumption = do putStr "Id of client (ex. 1000): "
                                      idclient <- getLine
                                      putStr "Year (ex. 2018): "
                                      year <- getLine 
                                      if ((read(idclient)::Idclient)>0 || (read(year)::Year)>0) then do let listConsumptionSorted = sortConsumptions listConsumption
                                                                                                        let name = findName (read(idclient)::Idclient) listConsumption
                                                                                                        putStr ("\n\tWater Consumption of " ++ year ++ ", " ++ name ++ 
                                                                                                                "\n---------------------------------------------------------------------\n" ++ 
                                                                                                                "\tMonth\t\t[0,5]m³\t[6,15]m³\n\n")
                                                                                                        putStr (printListConsumptionYear (read(idclient)::Idclient, read(year)::Year) listConsumptionSorted)
                                                                                                        putStr "---------------------------------------------------------------------\n\tWater Average: "
                                                                                                        putStr (show (totalConsumptionYear (read(idclient)::Idclient, read(year)::Year) listConsumption/12) ++ "m³")
                                      else error "ERROR: please check the values"

-- Calculate a water bill for one client in a year
calculateTotal listConsumption listInvoice = do putStr "Id of client (ex. 1000): "
                                                idclient <- getLine
                                                putStr "Year (ex. 2018): "
                                                year <- getLine
                                                if ((read(idclient)::Idclient)>0 || (read(year)::Year)>0) then do let listInvoiceSorted = sortInvoices listInvoice
                                                                                                                  let name = findName (read(idclient)::Idclient) listConsumption
                                                                                                                  putStr ("\n\tWater Bill of " ++ year ++ ", " ++ name ++ 
                                                                                                                          "\n---------------------------------------------------------------------\n" ++ 
                                                                                                                          "\tMonth\t\tWater Bill\n\n")
                                                                                                                  putStr(printListWaterBillYear (read(idclient)::Idclient, read(year)::Year) listInvoiceSorted)
                                                                                                                  putStr "---------------------------------------------------------------------\n\tWater Bill: "
                                                                                                                  putStr (show (totalWaterBillYear (read(idclient)::Idclient, read(year)::Year) listInvoice) ++ "€")
                                                else error "ERROR: please check the values"

-- Total number of cubicmeters consummated over 5 cubicmeters of all clients in the last year
over5cubicmeters listConsumption = do now <- getCurrentTime
                                      let (year, month, day) = toGregorian $ utctDay now
                                      let lastYear = year-1
                                      let listConsumptionSorted = sortConsumptions listConsumption
                                      putStr ("\n\tOver 5 cubicmeters of " ++ show(lastYear) ++ 
                                              "\n---------------------------------------------------------------------\n" ++ 
                                              "\tMonth\t\tId\t\tName\t\t[6,15]m³\n\n")
                                      putStr (printListOver5CubicmetersYear (read(show(lastYear))::Year) listConsumptionSorted)
                                      putStr "---------------------------------------------------------------------\n\tWater Total: "
                                      putStr (show (totalOver5CubicmetersYear (read(show(lastYear))::Year) listConsumption) ++ "m³")

-- Generate a graph connecting the monthly payment of a client during one year
graphPayment listConsumption listInvoice = do putStr "Id of client (ex. 1000): "
                                              idclient <- getLine
                                              putStr "Year (ex. 2018): "
                                              year <- getLine
                                              if ((read(idclient)::Idclient)>0 || (read(year)::Year)>0) then do let listInvoiceSorted = sortInvoices listInvoice
                                                                                                                let name = findName (read(idclient)::Idclient) listConsumption
                                                                                                                listWaterBillYear (read(idclient)::Idclient, read(year)::Year, name) listInvoiceSorted
                                              else error "ERROR: please check the values"


-- Start program and shows the menu
main :: IO()
main = do listConsumption <- loadWaterConsumption 
          listInvoice <- loadWaterInvoice 
          putStr "\n---------------------------------------------------------------------\n" 
          putStr "-- 1. Insert the consumption of one client in one month            --\n" 
          putStr "-- 2. Create the invoice of a client in Latex                      --\n" 
          putStr "-- 3. Average consumption for one client in one year               --\n" 
          putStr "-- 4. Total payment for one client at the end of one year          --\n" 
          putStr "-- 5. Total of over 5 cubicmeters of all clients in the last year  --\n" 
          putStr "-- 6. Graph of a monthly payment of a client during one year       --\n" 
          putStr "---------------------------------------------------------------------\n\n|: "
          op <- getLine 
          if (op=="1") then do putStr "\t\tConsumptions registered for each client\n"
                               putStr "---------------------------------------------------------------------\n"
                               putStr(printListWaterConsumption (sortConsumptionsId listConsumption))
                               insertConsumption                                
          else if (op=="2") then do putStr "\t\tInvoices registered for each client\n"
                                    putStr "---------------------------------------------------------------------\n"
                                    putStr(printListWaterInvoice (sortInvoicesId listInvoice))
                                    putStr "\n"
                                    createInvoiceLatex listConsumption
          else if (op=="3") then do calculateAverage listConsumption
          else if (op=="4") then do calculateTotal listConsumption listInvoice
          else if (op=="5") then do over5cubicmeters listConsumption
          else if (op=="6") then do graphPayment listConsumption listInvoice
          else error "Wrong option"
          putStr "\n\nDo you want to continue? (y/Y): "
          op <- getLine
          if (op=="y" || op=="Y") then main else return()