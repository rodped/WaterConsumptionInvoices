insert :: IO()
insert = do putStr "Client Id: "
            aa <- getLine
            putStr "Name: "
            b <- getLine
            putStr "Address: "
            c <- getLine
            putStr "Number of Persons: "
            d <- getLine
            putStr "CubicMeters (0 to 5): "
            e <- getLine
            putStr "CubicMeters (6 to 15): "
            f <- getLine
            putStr "Month: "
            g <- getLine
            putStr "Year: "
            h <- getLine
            appendFile "WaterConsumption.txt" (aa ++ "\t" ++ b ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ e ++ "\t" ++ f ++ "\t" ++ g ++ "\t" ++ h ++ "\n")
            putStr "Insert another one? (y/Y): "
            resp <- getLine
            if (resp=="y" || resp=="Y") then insert else return()

printLst [] = ""
printList ((aa,b,c,d,e,f,g,h):xs) = aa ++ "\t" ++ b ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ e ++ "\t" ++ f ++ "\t" ++ g ++ "\t" ++ h ++ "\n" ++ (printLst xs)

loadWaterConsumption = do s <- readFile "WaterConsumption.txt"
                          return (gerListWaterConsumption  (map words (lines s)))

gerListWaterConsumption [] = []
gerListWaterConsumption ([aa,b,c,d,e,f,g,h]:xs) = (aa,read(b)::Float,c,d,read(e)::Float,read(f)::Float,read(g)::Float,read(h)::Float):(gerListWaterConsumption xs)

--Shows the menu
main :: IO()
main = do list <- loadWaterConsumption
          putStr(printLst list)
          putStr "\n---------------------------------------------------------------------\n"
          putStr "-- 1. Insert the consumption of one client in one month            --\n"
          putStr "-- 2. Create the invoice of a client in Latex                      --\n"
          putStr "-- 3. Average consumption for one client in one year               --\n"
          putStr "-- 4. Total payment for one client at the end of one year          --\n"
          putStr "-- 5. Total of over 5 cubicmeters of all clients in the last year  --\n"
          putStr "-- 6. Graph of a monthly payment of a client during one year       --\n"
          putStr "-- 0. Exit and save                                                --\n"
          putStr "---------------------------------------------------------------------\n\n"
          op <- getLine
          if (op=="1") then do insert                                
          -- else if (op=="2") then do 
          -- else if (op=="3") then do 
          -- else if (op=="4") then do 
          -- else if (op=="5") then do 
          -- else if (op=="6") then do 
          -- else if (op=="0") then do 
          else error "Wrong option"
          putStr "Do you want to continue? (y/Y): "
          op <- getLine
          if (op=="y" || op=="Y") then main else return()

-- choice :: IO()
-- choice op = do if (op=="1") then do insert                                
--             -- else if (op=="2") then do 
--             -- else if (op=="3") then do 
--             -- else if (op=="4") then do 
--             -- else if (op=="5") then do 
--             -- else if (op=="6") then do 
--             -- else if (op=="0") then do 
--             else error "Wrong option"
--             putStr "Do you want to continue? (y/Y): "
--             op <- getLine
--             if (op=="y" || op=="Y") then main else return()
            