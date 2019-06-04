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

-- Find a water consumption and return an invoice
findWaterConsumption :: (Idclient, Month, Year) -> ListConsumption -> IO()
findWaterConsumption (idclient, month, year) [] = error "ERROR" 
findWaterConsumption (idclient, month, year) ((a, b, c, d, e, f, g, h): xs)
    | (idclient == a && month == g && year == h) = do let sft = (1.5* (fromIntegral d))
                                                      let c0_5t = (0.5*e) 
                                                      let c6_15t = (1.0*f) 
                                                      let water_bill = (sft + c0_5t + c6_15t) 
                                                      insertInvoice idclient water_bill g h
    | otherwise = findWaterConsumption (idclient, month, year) xs

insertInvoice :: Idclient -> Water_bill -> Month -> Year -> IO()
insertInvoice idclient water_bill month year = do appendFile "WaterInvoice.txt" (show(idclient) ++ "\t" ++ show(water_bill) ++ "\t" ++ show(month) ++ "\t" ++ show(year) ++ "\n")

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
          if (op=="1") then do insertConsumption                                 
          else if (op=="2") then do print listConsumption
                                    createInvoiceLatex listConsumption
          -- else if (op=="3") then do 
          -- else if (op=="4") then do 
          -- else if (op=="5") then do 
          -- else if (op=="6") then do 
          -- else if (op=="0") then do 
          else error "Wrong option"
          putStr "Do you want to continue? (y/Y): "
          op <- getLine
          if (op=="y" || op=="Y") then main else return()