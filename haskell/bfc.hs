import System.Environment (getArgs)

compile :: String -> [String]
compile = compile' s n ["main = do"]
 where
  compile' :: String -> Int -> [String]
  compile' [] _ ss = ss
  compile' ('>':cs) n ss = compile' cs (succ n)
    (ss ++ [ "  let i" ++ show (succ n) ++ " = succ i" ++ show n
           , "  let mem" ++ show (succ n) ++ " = mem" ++ show n
           ])
  compile' ('<':cs) n ss = compile' cs (succ n)
    (ss ++ [ "  let i" ++ show (succ n) ++ " = pred i" ++ show n
           , "  let mem" ++ show (succ n) ++ " = mem" ++ show n
           ])
  compile' ('+':cs) n ss = compile' cs (succ n)
    (ss ++ [ "  let (x" ++ show n ++ ", y" ++ show n ++ ") = splitAt i" ++ show n ++ " " ++ "mem" ++ show n
           , "  let i" ++ show (succ n) ++ " = i" ++ show n
           , "  let mem" ++ show (succ n) ++ " = x" ++ show n ++ " ++ (succ $ head y" ++ show n ++ "):tail y" ++ show n
           ])
  compile' ('-':cs) n ss = compile' cs (succ n)
    (ss ++ [ "  let (x" ++ show n ++ ", y" ++ show n ++ ") = splitAt i" ++ show n ++ " " ++ "mem" ++ show n
           , "  let i" ++ show (succ n) ++ " = i" ++ show n
           , "  let mem" ++ show (succ n) ++ " = x" ++ show n ++ " ++ (pred $ head y" ++ show n ++ "):tail y" ++ show n
           ])
  compile' ('.':cs) n ss = compile' cs (succ n)
    (ss ++ [ "  let (x" ++ show n ++ ", y" ++ show n ++ ") = splitAt i" ++ show n ++ " " ++ "mem" ++ show n
           , "  c" ++ show n ++ " <- getChar"
           , "  let i" ++ show (succ n) ++ " = i" ++ show n
           , "  let mem" ++ show (succ n) ++ " = x" ++ show n ++ " ++ c" ++ show n ++ ":tail y" ++ show n
           ])
  compile' (',':cs) n ss = compile' cs (succ n)
    (ss ++ [ "  putChar $ mem" ++ show n ++ " !! i" ++ show n
           , "  let i" ++ show (succ n) ++ " = i" ++ show n
           , "  let mem" ++ show (succ n) ++ " = mem" ++ show n
           ])
  compile' ('[':cs) n ss =
  compile' (']':cs) n ss =
 where

main :: IO ()
main = do
  args <- getArgs
  let readall = if length args == 0 then getContents else readFile $ head args
  s <- readall
  mapM_ putStrLn $ compile s

