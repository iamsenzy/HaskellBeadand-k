type Pos = (Int, Int)
type Cell = (Pos, Int)
type Sudoku = [Cell]
type Block = Int

sudoku :: Sudoku
sudoku = [((0,0),3),((0,1),6),((0,4),7),((0,5),1),((0,6),2),
          ((1,1),5),((1,6),1),((1,7),8),
          ((2,2),9),((2,3),2),((2,5),4),((2,6),7),
          ((3,4),1),((3,5),3),((3,7),2),((3,8),8),
          ((4,0),4),((4,3),5),((4,5),2),((4,8),9),
          ((5,0),2),((5,1),7),((5,3),4),((5,4),6),
          ((6,2),5),((6,3),3),((6,5),8),((6,6),9),
          ((7,1),8),((7,2),3),((7,7),6),
          ((8,2),7),((8,3),6),((8,4),9),((8,7),4),((8,8),3)]
sudoku2 :: Sudoku
sudoku2 = [((0,0),5),((0,1),3),((0,4),7),
           ((1,0),6),((1,3),1),((1,4),9),((1,5),5),
           ((2,1),9),((2,2),8),((2,7),6),
           ((3,0),8),((3,4),6),((3,8),3),
           ((4,0),4),((4,3),8),((4,5),3),((4,8),1),
           ((5,0),7),((5,4),2),((5,8),6),
           ((6,1),6),((6,6),2),((6,7),8),
           ((7,3),4),((7,4),1),((7,5),9),((7,8),5),
           ((8,4),8),((8,7),7),((8,8),9)]
numsInRow :: Sudoku -> Int -> [Int]
numsInRow l@(x:xs) s = [snd(i)| i<-l,fst(fst(i))==s]

numsInCol :: Sudoku -> Int -> [Int]
numsInCol l@(x:xs) s = [snd(i)| i<-l,snd(fst(i))==s]

posToBlock :: Pos -> Block
posToBlock (x,y) = x - (x `mod` 3) + y `div` 3

blockToPositions :: Block -> [Pos]
blockToPositions s
   | s `notElem` [0..8] = error ("blockToPositions: bad block number " ++ show s)
   | otherwise = [ (x,y) | x <- [0..8], y <- [0..8], s == (x - (x `mod` 3) + y `div` 3) ]

numsInBlock :: Sudoku -> Block -> [Int]
numsInBlock l@(x:xs) b = [snd(i)|i<-l,j<-(blockToPositions b),fst(fst(i))==fst(j) && snd(fst(i))==snd(j)]  
   
allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = not (elem x xs)&&allUnique xs


isSudokuPuzzle :: Sudoku -> Bool
isSudokuPuzzle sud = and [and [ x `elem` [0..8] && y `elem` [0..8] && z `elem` [1..9] | ((x,y), z) <- sud ] , and [ allUnique a | a <- [numsInRow sud i | i <- [0..8] ]] , and [ allUnique a | a <- [numsInCol sud i | i <- [0..8] ]] , and [ allUnique a | a <- [numsInBlock sud i | i <- [0..8] ]]]

isFilled :: Sudoku -> Bool
isFilled l@(x:xs) = allUnique [(fst(fst(i)),snd(fst(i)))|i<-l] &&
                    length [(fst(fst(i)),snd(fst(i)))|i<-l] == 81
					
isSolved :: Sudoku -> Bool
isSolved l@(x:xs) = isSudokuPuzzle l &&  length [(fst(fst(i)),snd(fst(i)))|i<-l] == 81

isBlank :: Sudoku -> Pos -> Bool
isBlank sudoku (x,y) = (x,y) `notElem` [ (j,k) | ((j,k),l) <- sudoku ]

blankPositions :: Sudoku -> [Pos]
blankPositions sud = [ (x,y) | x <- [0..8], y <- [0..8], isBlank sud (x,y) ]

possibleNumsOnPos :: Sudoku -> Pos -> [Int]
possibleNumsOnPos sud (x,y)  
   | isBlank sud (x,y) = [ i | i <- [1..9], i `notElem` numsInRow sud x, i `notElem` numsInCol sud y, i `notElem` numsInBlock sud (posToBlock (x,y)) ]
   | otherwise = []
   
possibleNumsForBlankPos :: Sudoku -> [(Pos, [Int])]
possibleNumsForBlankPos sud = [ ((x,y), possibleNumsOnPos sud (x,y)) | x <- [0..8], y <- [0..8], isBlank sud (x,y)]

hasSolution :: [(Pos, [Int])] -> Bool
hasSolution [] = False
hasSolution x = and [ not (null k) | ((i,j),k) <- x ]

uniqueNumForBlankPos :: [(Pos, [Int])] -> [(Pos, Int)]
uniqueNumForBlankPos x = [ ((i,j),head k) | ((i,j),k) <- x, length k == 1 ]

insertElem :: Sudoku -> Pos -> Int -> Sudoku
insertElem sud (x,y) n 
   | isBlank sud (x,y) = ((x,y),n):sud
   | otherwise = error ("insertElem: position " ++ show (x,y) ++ " is not blank")
   
step :: Sudoku -> [Sudoku]
step sud
   | isSolved sud = [sud]
   | hasSolution (p) && not (null (u)) = [ insertElem sud (x) (y) ]
   | hasSolution (p) && null (u) = [ insertElem sud (head (blankPositions sud)) x | x <- possibleNumsOnPos sud (head (blankPositions sud)) ]
   | not (hasSolution (p)) = []
      where
        (x,y)= head u
        p =possibleNumsForBlankPos sud
        u = uniqueNumForBlankPos (p)
   
solve :: Sudoku -> [Sudoku]
solve sud 
    | not (isSudokuPuzzle sud) = error "solve: improper sudoku"
    | otherwise = 
     until done f l 
       where 
         l = return sud
         f (x:xs) = (f xs) ++ step x 
         f [] = []
         done m = and (map isSolved m ) && and (map isSudokuPuzzle m)