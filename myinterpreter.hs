import Tokens
import Grammar
import System.Environment
import System.IO
import Control.Exception
 
-- Loaded Values, Buffer, [(Var, Value)], Exit (yes/no), Loop Commands, Current Instruction (and the rest)
data ProgState = State [[Int]] [[Int]] [(Int, Int)] Program Program
       | ErrorState String
       deriving Show
 
data ExitException = ExitSuccess
       deriving (Show)
 
instance Exception ExitException
 
main :: IO ()
main = do
       ( filename : _ ) <- getArgs
       sourceText <- readFile filename
       let parsedProg = parseCalc ((alexScanTokens sourceText) ++ [TokenEnd])
       checkForBadFile parsedProg
       inited <- initIOState parsedProg
       startState <- executePreamble inited
       endState <- catch (executeLoop startState) exitHandler
       return () -- Should be unreachable

checkForBadFile :: Program -> IO ()
checkForBadFile prog = do
       checkForManyLoops prog 0
       checkForEndPreLoop prog
       checkForBadVarUse prog []

checkForBadVarUse :: Program -> [Int] -> IO ()
checkForBadVarUse End _ = return ()
checkForBadVarUse (Line Loop rest) xs = checkForBadVarUse rest xs
checkForBadVarUse (Line (Buffer x y) rest) xs = do
       checkForBadVarUseExp x xs
       checkForBadVarUseExp y xs
       checkForBadVarUse rest xs
checkForBadVarUse (Line (SetVar x y) rest) xs = do
       checkForBadVarUseExp y (x:xs)
       checkForBadVarUse rest (x:xs)
checkForBadVarUse (Line (Out x) rest) xs = do
       checkForBadVarUseExp x xs
       checkForBadVarUse rest xs

checkForBadVarUseExp :: Exp -> [Int] -> IO ()
checkForBadVarUseExp (Var x) xs
 | not (elem x xs) = hPutStrLn stderr ("WARNING\nUse of variable " ++ show x ++ " before it is initialised")
 | otherwise = return ()
checkForBadVarUseExp (Mul l r) xs = do
       checkForBadVarUseExp l xs
       checkForBadVarUseExp r xs
checkForBadVarUseExp (Div l r) xs = do
       checkForBadVarUseExp l xs
       checkForBadVarUseExp r xs
checkForBadVarUseExp (Sub l r) xs = do
       checkForBadVarUseExp l xs
       checkForBadVarUseExp r xs
checkForBadVarUseExp (Add l r) xs = do
       checkForBadVarUseExp l xs
       checkForBadVarUseExp r xs
checkForBadVarUseExp (Release l r) xs = do
       checkForBadVarUseExp l xs
       checkForBadVarUseExp r xs
checkForBadVarUseExp (IntVal x) xs = do return ()

checkForEndPreLoop :: Program -> IO ()
checkForEndPreLoop End = hPutStrLn stderr ("WARNING\nEnd detected before loop, consider fixing if unintentional")
checkForEndPreLoop (Line Loop rest) = return ()
checkForEndPreLoop (Line _ rest) = checkForEndPreLoop rest

checkForManyLoops :: Program -> Int -> IO ()
checkForManyLoops (Line Loop rest) x = checkForManyLoops rest (x + 1)
checkForManyLoops (Line _ rest) x = checkForManyLoops rest x
checkForManyLoops End x
       | x == 1 = return ()
       | otherwise = hPutStrLn stderr ("WARNING\nOnly 1 Loop is permitted per file, loops found: " ++ (show x))
 
exitHandler :: ExitException -> IO ProgState
exitHandler e = return (State [] [] [] End End)
 
executeLoop :: ProgState -> IO ProgState
executeLoop (ErrorState s) = do
       hPutStrLn stderr s
       return (ErrorState s)
executeLoop (State loaded buffer vars loop End) = do
       let newState = State loaded (genEmptyBuff (length loaded)) vars loop loop
       retVal <- executeLoop newState
       return retVal
executeLoop (State loaded buffer vars loop (Line com rest)) = do
       newState <- executeCommand (com, State loaded buffer vars loop rest)
       retVal <- executeLoop newState
       return retVal
 
executeCommand :: (Command, ProgState) -> IO ProgState
executeCommand (SetVar x e, s) = do return (setVar (SetVar x e, s))
executeCommand (Out e, s) = do
       out (Out e, s)
       return s
executeCommand (Buffer x y, s) = do
       retval <- stdInLoadWrapper (Buffer x y, s)
       return (retval)
executeCommand (Loop, s) = do
       return (ErrorState "Loop is not an executable command")
 
initIOState :: Program -> IO (ProgState, Program)
initIOState prog = do
       let loop = getLoop prog
       loaded <- loadLine
       let buffer = genEmptyBuff (length loaded)
       return (State loaded buffer [] loop loop, removeLoop prog)
 
genEmptyBuff :: Int -> [[Int]]
genEmptyBuff 0 = []
genEmptyBuff x = []:genEmptyBuff (x - 1)
 
stdInLoadWrapper :: (Command, ProgState) -> IO ProgState
stdInLoadWrapper (Buffer amount from, State loads buff vars loop instr)
 | amountNeeded > 0 = do
       newState <- loadToLoad amountNeeded (State loads buff vars loop instr)
       return (loadToBuffer (Buffer amount from, newState))
 | otherwise = do return (loadToBuffer (Buffer amount from, State loads buff vars loop instr))
 where numOf = evalExp [] vars amount
       buffIndex = evalExp [] vars from
       amountNeeded = numOf - (length (loads!!(buffIndex - 1)))
 
mergeStdInLineToLoad :: [[Int]] -> [[Int]] -> [[Int]]
mergeStdInLineToLoad [] loads = loads
mergeStdInLineToLoad _ [] = []
mergeStdInLineToLoad (li:line) (lo:loads) = (lo ++ li):(mergeStdInLineToLoad line loads)
 
loadToLoad :: Int -> ProgState -> IO ProgState
loadToLoad 0 s = do return s
loadToLoad x (State loads buff vars loop instr) = do
       newVals <- loadLine
       if length newVals == 0 then
             return (State loads buff vars loop instr)
       else do
             let newLoads = mergeStdInLineToLoad newVals loads
             nextIteration <- loadToLoad (x - 1) (State newLoads buff vars loop instr)
             return nextIteration
 
loadToBuffer :: (Command, ProgState) -> ProgState
loadToBuffer (Buffer amount from, State loads buff vars loop instr) = State newLoads newBuff vars loop instr
       where numOf = evalExp [] vars amount
             buffIndex = (evalExp [] vars from)
             amountToBuffer = minimum (length (loads!!(buffIndex - 1)):[numOf])
             buffLoadPair = (take amountToBuffer $ loads!!(buffIndex - 1), drop amountToBuffer $ loads!!(buffIndex - 1))
             newLoads = (take (buffIndex - 1) loads) ++ [snd buffLoadPair] ++ (tail $ drop (buffIndex - 1) loads)
             newBuff = (take (buffIndex - 1) buff) ++ [buff!!(buffIndex - 1) ++ fst buffLoadPair] ++ (tail $ drop (buffIndex - 1) buff)
 --splits into pair ([tobufferlist], [tobecomenewload (what's left behind)])
 
loadLine :: IO [[Int]]
loadLine = do
       eof <- isEOF
       if eof then
             return []
       else do
             line <- getLine
             return [[x] | x <- (map read $ words line :: [Int])]
 
getLoop :: Program -> Program
getLoop (Line Loop x) = x
getLoop (Line _ x) = getLoop x
 
removeLoop :: Program -> Program -- Maybe turn into a genSections :: Program -> (Program, Program) TODO ("Loop" explicit or implicit?)
removeLoop End = End
removeLoop (Line Loop x) = End
removeLoop (Line x y) = (Line x (removeLoop y))
 
executePreamble :: (ProgState, Program) -> IO ProgState
executePreamble (s, End) = do 
       return (s)
executePreamble (s, (Line (Out e) rest)) = do
       out ((Out e), s)
       executePreamble (s, rest)
executePreamble (s, Line (SetVar x e) rest) = do
       executePreamble (setVar ((SetVar x e), s), rest)
executePreamble (_, _) = do
       return (ErrorState "Invalid Command for Preamble (only Out and SetVar permitted)")
 
-- sets variable with ID "x" to a value expressed in e
setVar :: (Command, ProgState) -> ProgState
setVar (SetVar x e, State loads buff vars loop instr) 
 | x `elem` map fst vars = State loads buff (map (\f -> if f == m then (x, y) else f) vars) loop instr --replace (x, _) with (x, y) if it exists
 | otherwise = State loads buff (vars ++ [(x, y)]) loop instr
 where
       m = [(a, b) | (a, b) <- vars, a == x]!!0
       y = (evalExp buff vars e)
setVar (_, _) = ErrorState "Incorrect usage of SetVar; format: SetVar ~Int Expression"
 
evalExp :: [[Int]] -> [(Int, Int)] -> Exp -> Int
evalExp buff vars (Add x y) = (evalExp buff vars x) + (evalExp buff vars y)
evalExp buff vars (Sub x y) = (evalExp buff vars y) - (evalExp buff vars x)
evalExp buff vars (Mul x y) = (evalExp buff vars x) * (evalExp buff vars y)
evalExp buff vars (Div x y) = (evalExp buff vars x) `div` (evalExp buff vars y)
evalExp buff vars (Release e1 e2)
 | (0 < x && x <= length (buff!!(y - 1))) = buff!!(y - 1)!!(x - 1) --get from list y [- 1], val at index x [- 1]
 | otherwise = throw (ExitSuccess)
 where x = evalExp buff vars e1
       y = evalExp buff vars e2
evalExp buff vars (IntVal x) = x
evalExp buff vars (Var x) = [b | (a, b) <- vars, a == x]!!0
 
out :: (Command, ProgState) -> IO ()
out (Out e, State loads buff vars loop instr) = do
       putStrLn (show (evalExp buff vars e))
out (_, _) = hPutStrLn stderr "Incorrect usage of Out; format: Out Expression"