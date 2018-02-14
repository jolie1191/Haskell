{-# LANGUAGE GADTs, KindSignatures #-}

module Main where

type Graph i a = [(i,a,[(String,i)])]

data RoomId = AboveDeck | BelowDeck | Captain'sQuarters | SharkShark
        deriving (Eq, Ord, Show, Enum)

type Plan = Graph RoomId String

plan :: Plan
plan = [(AboveDeck, "Above Deck", [("down", BelowDeck)
                                  ,("in", Captain'sQuarters)
                                  ,("jump", SharkShark)
                                  ])
       ,(Captain'sQuarters, "In Captain's Quarters",
                                  [("out", AboveDeck)])
       ]                          
        

showHere :: Plan -> RoomId -> String
showHere [] roomId = error "You win. Computer brain explodes"
showHere ((i,s,dirs):rest) roomId = 
        if i == roomId 
        then s ++ "\nYou can go: " ++ unwords (map fst dirs)
        else showHere rest roomId

tryGo :: Plan -> RoomId -> String -> Maybe RoomId
tryGo []                _      _   = Nothing
tryGo ((i,s,dirs):rest) roomId dir = 
        if i == roomId 
        then case lookup dir dirs of
                Nothing -> Nothing
                Just nxt -> Just nxt
        else tryGo rest roomId dir

type State = RoomId

readMe :: State -> IO ()
readMe st = do
        putStrLn "What do you want to do?"
        xs <- getLine
        evalMe (words xs) st

evalMe :: [String] -> State -> IO ()
evalMe ["init"] st = 
        printMe (showHere plan st) st
evalMe ["go",dir] st = 
        -- find out if you can do this.
        case tryGo plan st dir of
          Nothing      -> printMe "sorry; I can't do that" st
          Just roomId' -> 
                  let st' = roomId'
                  in printMe (showHere plan st') st'
evalMe _ st =
        printMe "Valid commands are: go <dir>" st

printMe :: String -> State -> IO ()
printMe msg st = do
        putStrLn msg
        readMe st
       
main :: IO ()
main = evalMe ["init"] AboveDeck



