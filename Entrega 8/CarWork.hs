wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

data Car = Car { pass :: Int, passMax :: Int, gas :: Int, gasMax :: Int, km :: Int} deriving Eq

class CarClass a where
    toString :: a -> String
    in' :: a -> a
    out :: a -> a
    fuel :: Int -> a -> a
    drive :: Int -> a -> a

instance CarClass Car where
    toString (Car pass passMax gas gasMax km) = "pass: " ++ show pass ++ ", gas: " ++ show gas ++ ", km: " ++ show km

    in' (Car pass passMax gas gasMax km)
        | pass + 1 <= passMax = Car (pass+1) passMax gas gasMax km
        | otherwise = Car pass passMax gas gasMax km

    out (Car pass passMax gas gasMax km)
        | pass - 1 >= 0 = Car (pass-1) passMax gas gasMax km
        | otherwise = Car pass passMax gas gasMax km

    fuel x (Car pass passMax gas gasMax km)
        | gas+x <= gasMax = Car pass passMax (gas+x) gasMax km
        | otherwise = (Car pass passMax gasMax gasMax km)

    drive x (Car pass passMax gas gasMax km)
        | gas-x >= 0 && pass > 0 = (Car pass passMax (gas-x) gasMax (km+x))
        | pass > 0 = Car pass passMax 0 gasMax (km+gas)
        | otherwise = Car pass passMax gas gasMax km


beginCar :: Car
beginCar = Car 0 2 0 100 0

msg1 = "fail.: limite de pessoas atingido"
msg2 = "fail: nao ha ninguem no carro"
msg3 = "fail: tanque vazio"
msg4 x = "fail: tanque vazio apos andar " ++ show x ++ " km"

getPass :: Car -> Int
getPass (Car pass _ _ _ _) = pass

getPassMax :: Car -> Int
getPassMax (Car _ passMax _ _ _) = passMax

getGas :: Car -> Int
getGas (Car _ _ gas _ _) = gas

getGasMax :: Car -> Int
getGasMax (Car _ _ _ gasMax _) = gasMax

getKm :: Car -> Int
getKm (Car _ _ _ _ km) = km

class Manual a where
    main' :: a -> IO ()

instance Manual [a] where
    main' _ = 
        do
            let c = beginCar
            putStrLn (toString c)
            let c1 = in' c
                c2 = in' c1
            putStrLn (toString c2)
            let c3 = in' c2

            if c2 /= c3 then putStrLn (toString c3) else putStrLn msg1
            putStrLn (toString c3)
            let c4 = out c3
                c5 = out c4
                c6 = out c5

            if c5 /= c6 then putStrLn (toString c6) else putStrLn msg2
            putStrLn (toString c6)
            
            putStrLn "New Car\n"
            
            let c = beginCar
                c1 = fuel 60 c
            putStrLn (toString c1)
            
            let c2 = drive 10 c1
            
            if c2 /= c1 then putStrLn (toString c2) else putStrLn msg2
            
            let c3 = in' c2
                c4 = drive 10 c3
            putStrLn (toString c4)
            
            let c5 = drive 70 c4
            
            if getGas c4 - 70 >= 0 then putStrLn (toString c5) else putStrLn (msg4 (getGas c4))
            
            let c6 = drive 10 c5
            if c5 /= c6 then putStrLn (toString c5) else putStrLn msg3
            putStrLn (toString c6)
            
            let c7 = fuel 200 c6
            putStrLn (toString c7)

loop :: Car -> IO ()
loop c = 
        do
            line <- getLine

            if line == ""
                then return ()
                else
                    do
                        let ui = wordsWhen (==' ') line
                        case ui !! 0 of
                            "in" -> 
                                do
                                    let c1 = in' c

                                    if c1 /= c then putStr "" else putStrLn msg1
                                    loop c1
                            "out" ->
                                do
                                    let c1 = out c

                                    if c1 /= c then putStr "" else putStrLn msg2
                                    loop c1

                            "show" -> putStrLn (toString c) >> loop c
                            "drive" -> 
                                do

                                    let x = read (ui !! 1) :: Int
                                        c1 = drive x c

                                    if getPass c > 0
                                        then if getGas c > 0
                                                then if getGas c - x >= 0
                                                        then putStr ""
                                                        else putStrLn (msg4 (getGas c))
                                                else putStrLn msg3
                                        else putStrLn msg2
                                    loop c1
                            "fuel" -> 
                                do
            
                                    let x = read (ui !! 1) :: Int
                                        c1 = fuel x c
                                    loop c1
                            _ -> putStrLn "fail: comando invalido" >> loop c

class Solver a where
    main'' :: a -> IO ()

instance Solver [a] where
    main'' _ = loop beginCar

main = main'' []