safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead a = Just (head a)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail a = Just (tail a)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast a = Just (last a)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit a = Just (init a)

myFromMaybe :: a -> Maybe a -> a
myFromMaybe def Nothing = def
myFromMaybe _ (Just a) = a

splitWith:: (a -> Bool) -> [a] -> [[a]]
splitWith pred (x:xs) | pred x == True  = x:(splitWith pred xs)
                      | pred x == False = (x:[]):(splitWith pred xs)