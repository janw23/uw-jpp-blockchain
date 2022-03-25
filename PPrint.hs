-- jw418479
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS (showString "\n")
pprH = intercalateS (showString " ")

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep (x:xs) = foldl (.) id (x : map (sep .) xs)
intercalateS sep [] = id

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f lst = pprV $ map f lst

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
