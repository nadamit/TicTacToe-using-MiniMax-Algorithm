import Data.List
import Data.Char

--All possible combinations, where an user ot computer can win
possibleCombinatons = [[1,4,7],[2,5,8],[3,6,9],[1,2,3],[4,5,6],[7,8,9],[1,5,9],[3,5,7]]
possiblePos = [1..9]

data Board = Board [Int][Int] Bool deriving Eq
--Board Datatype---list of Xs, list of Ys and a flag for computer or user.

--checks if the list of Xs is present in the winning configuration
ifXWins::Board->Bool
ifXWins (Board xS oZ _) =  (checkl ([check xS (winningComb)|winningComb<-possibleCombinatons]))

--checks if the list of Os is present in the winning configuration 
ifOWins::Board->Bool
ifOWins (Board xS oZ _) =  (checkl ([check oZ (winningComb)|winningComb<-possibleCombinatons]))
	
checkl [] = False
checkl (x:xs) = x || checkl xs

check list [] = True
check [] list = False
check (x:xs) (y:ys) = if x==y then (check xs ys) else (if y>x then check xs (y:ys) else(False))


possibleMoves::Board->[Board]
possibleMoves (Board listofXs listofOz True) = 
 [Board (insert' listofXs possibleblocks) listofOz False|
  possibleblocks<-possiblePos, not(elem possibleblocks listofXs), not(elem possibleblocks listofOz)]
possibleMoves (Board listofXs listofOz False) =  
	[Board listofXs (insert' listofOz posibleblocks) True 
	| posibleblocks<-possiblePos, not(elem posibleblocks listofXs), not(elem posibleblocks listofOz)]


insert' :: [Int] -> Int -> [Int]
insert' [] x = [x]
insert' (x:lst) y = if y>x then  (x:(insert' lst y)) else (y:x:lst) 


algorithm::Board->(Board,Int)
algorithm (Board xlist ylist bool) = if ifXWins (Board xlist ylist bool) then ((Board [9999] [9999] True),100)
 else ( if ifOWins (Board xlist ylist bool) then ((Board [9999] [9999] True),0) else ( if ((length xlist) + (length ylist)==9) 
 	then ((Board [9999] [9999] True),50) else (optimalMove (bool) (possibleMoves (Board xlist ylist bool)))))

   

cmpMove config configs bool= let
								 (configuration,overAllWeight) = optimalMove bool configs 
								 weight = snd (algorithm config)
							 in  (if weight==100 then (config,weight) else (if weight>=overAllWeight then (config,weight) else (configuration,overAllWeight)))



usrMove config configs bool= let
								 (configuration,overAllWeight) = optimalMove bool configs 
								 weight = snd (algorithm config)
							 in  (if weight==0 then (config,weight) else (if weight<=overAllWeight then ((config,weight)) else ((configuration,overAllWeight))))
			

optimalMove::Bool->[Board]->(Board,Int)
optimalMove bool [config] = (config, snd (algorithm config))
optimalMove bool (config:configs) 
			| bool = cmpMove config configs bool
			|otherwise = usrMove config configs bool


--------------------------------------------------------------show Board-----------------------------------------------------------------------------
instance Show Board where
	show(Board xList oList _) = prettyPrint (convertToListOfXsandOz xList oList) 0


convertToListOfXsandOz::[Int]->[Int]->[Maybe Char]
convertToListOfXsandOz (xList) (yList) = obtainList (xList) (yList) [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]

obtainList::[Int]->[Int]->[Maybe Char]->[Maybe Char]
obtainList xlist ylist dummy = put xlist ((put ylist dummy 'O')) 'X'


put [] list char = list
put (y:ys) list char = put ys (putElem char 1 list y []) char

putElem::Char->Int->[Maybe Char]->Int->[Maybe Char]->[Maybe Char]
putElem char counter (x:xs) pos dummy
				|counter==pos = (reverse dummy)++[Just char]++ xs
				|otherwise = putElem char (counter+1) xs pos (x:dummy)
 

prettyPrint::[Maybe Char]->Integer->[Char]
prettyPrint [] counter= [] 
prettyPrint config 1=(prettyPrint config 0) 
prettyPrint config counter="\n"++(printl(take 3 config) "\n" 0)++prettyPrint(drop 3 config)(counter + 1)

printl::[Maybe Char]->[Char]->Integer->[Char]
printl [] string _  = string++ " "
printl x  string 3 = printl x (string++ " ") 0
printl (x:xs) string counter = (printl xs (string ++ " "++ (maybeToChar x)) (counter + 1)) 

maybeToChar::Maybe Char->String
maybeToChar  Nothing = "_"
maybeToChar (Just char) =  (showLitChar char "") 

display::Board->IO()
display board = putStrLn(show board)


runGame::IO()
runGame = 
	do
		putStrLn "Enter name"
		name<-getLine
		putStrLn ("*** Welcome to TicTacToe:"++name++"*** ")
		putStrLn ("Enter 1, Player to start")
		putStrLn ("Enter 2, Computer to start")
		choice<-getLine
		case choice of
			"1"->do
					let config = (Board [] [] False)
					display config
					gameStart name config
					
			"2"->do
					let config = (Board [] [] True)
					display config
					gameStart name config
			_  -> putStrLn "Illegal Input !!!"
		

insrt::Board->Int->Board
insrt (Board xlist olist bool) num = if((not (elem num xlist))&&  (elem num possiblePos) && (not (elem num olist))) then (Board xlist (insert' olist num) (not bool) ) else ((Board [9999] [9999] True))


gameStart::String->Board->IO()
gameStart name (Board xlist olist bool)
	|ifXWins (Board xlist olist bool) = putStrLn("Computer Wins!!"++" , "++name++" "++"Lost")
	|ifOWins (Board xlist olist bool) = putStrLn(name++":"++"Wins the game!!")
	|(length xlist)+(length olist)==9 			  = putStrLn("Draw!!")
	|bool = 
		do
			putStrLn("Let me think for a moment.........")
			(display (fst(algorithm (Board xlist olist bool))))
			gameStart name (fst(algorithm (Board xlist olist bool)))

	|otherwise = do
					putStrLn("Enter position where you want to place your token!")
					number<-getLine
					let userBoard = insrt (Board xlist olist bool) (read number::Int)
					display userBoard
					gameStart name userBoard








