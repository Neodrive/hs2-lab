import Students
quicksortStudentsByProperty [] property = []
quicksortStudentsByProperty (x:xs) property = 
	let smallerSorted = quicksortStudentsByProperty [ a | a <- xs, (property a) <= (property x) ] property
	    biggerSorted = quicksortStudentsByProperty [ a | a <- xs , (property a) > (property x) ] property
	in smallerSorted ++ [x] ++ biggerSorted
	
divideInto4groups [] = []
divideInto4groups (x:xs) = divideInto4groups_ (x:xs) [] [] [] [] 0

divideInto4groups_ [] y1 y2 y3 y4 step = y1 : y2 : y3 : y4 : []
divideInto4groups_ (x:xs) y1 y2 y3 y4 step
    | mod step 8 == 0 = divideInto4groups_ xs (x : y1) y2 y3 y4 (step + 1)
    | mod step 8 == 1 = divideInto4groups_ xs y1 (x : y2) y3 y4 (step + 1)
    | mod step 8 == 2 = divideInto4groups_ xs y1 y2 (x : y3) y4 (step + 1)
    | mod step 8 == 3 = divideInto4groups_ xs y1 y2 y3 (x : y4) (step + 1)
    | mod step 8 == 4 = divideInto4groups_ xs y1 y2 y3 (x : y4) (step + 1)
    | mod step 8 == 5 = divideInto4groups_ xs y1 y2 (x : y3) y4 (step + 1)
    | mod step 8 == 6 = divideInto4groups_ xs y1 (x : y2) y3 y4 (step + 1)
    | otherwise = divideInto4groups_ xs (x : y1) y2 y3 y4 (step + 1)


getAverageIQ [] = 0
getAverageIQ (x:xs) = getAverageIQ_ (x:xs) 0 0

getAverageIQ_ [] step summIQ = (fromIntegral summIQ) / step
getAverageIQ_ (x:xs) step summIQ = getAverageIQ_ xs (step + 1) (summIQ + (iq x))

main = do
	let dividedList = divideInto4groups (quicksortStudentsByProperty students iq)
	
	putStrLn ("Список имён студентов первой группы: " ++ "(средний IQ = " ++ show (getAverageIQ (dividedList !! 0)) ++ " )")
	mapM_ putStrLn $ map (\(pos, st) -> show pos ++ "). " ++ name st) $ zip [1..] (dividedList !! 0)
	putStrLn ""
	
	putStrLn ("Список имён студентов второй группы: " ++ "(средний IQ = " ++ show (getAverageIQ (dividedList !! 1)) ++ " )")
	mapM_ putStrLn $ map (\(pos, st) -> show pos ++ "). " ++ name st) $ zip [1..] ((divideInto4groups (quicksortStudentsByProperty students iq)) !! 1)
	putStrLn ""
	
	putStrLn ("Список имён студентов третьей группы: " ++ "(средний IQ = " ++ show (getAverageIQ (dividedList !! 2)) ++ " )")
	mapM_ putStrLn $ map (\(pos, st) -> show pos ++ "). " ++ name st) $ zip [1..] (dividedList !! 2)
	putStrLn ""
	
	putStrLn ("Список имён студентов четвёртой группы: " ++ "(средний IQ = " ++ show (getAverageIQ (dividedList !! 3)) ++ " )")
	mapM_ putStrLn $ map (\(pos, st) -> show pos ++ "). " ++ name st) $ zip [1..] (dividedList !! 3)