module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Board = [Row]

type Row = [Square]

data Square = Square {
    position :: Position,
    isVisited :: Bool
} deriving (Show, Eq)

data Position = Position {
    row :: Number,
    column :: Char
} deriving (Show, Eq)

type Move = Position -> [Position]

type Path = [Position]

pathsToVisitAllBoard :: Board -> Move -> Position -> [Path]
pathsToVisitAllBoard board move = filter (visitsAll board) . allPossiblePaths board move

visitsAll :: Board -> Path -> Bool
visitsAll board = all areAllRowsVisited . foldl visitSquareInBoard board

areAllRowsVisited :: Row -> Bool
areAllRowsVisited = all isVisited

visitSquareInBoard :: Board -> Position -> Board
visitSquareInBoard board position = map (visitSquareInRow position) board

visitSquareInRow :: Position -> Row -> Row
visitSquareInRow position = map (visitSquare position)

visitSquare :: Position -> Square -> Square
visitSquare aPosition square | position square == aPosition = changeVisited True square
                             | otherwise = square

changeVisited :: Bool -> Square -> Square
changeVisited visited square = square { isVisited = visited }

allPossiblePaths :: Board -> Move -> Position -> [Path]
allPossiblePaths board move position | noMovesLeft board move position = []
                                     | ???