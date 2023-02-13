type color = Black | White


type point = {row:int; column:int; id: int}

type player = {team: color; score: int; territory: (point list)}

type dimension = int * int

(*We model the board using only the set stones*)
type board = point list * dimension



type gameState = {
  playerBlack: player;
  playerWhite: player;
  toMove: color;
  finished: bool;
  previousBoard: board option;
  currentBoard: board;
}

let initPlayer color = {team=color; score=0; territory=[]}

let initBoard width height = ([],(width, height))

let initGame width height = {
  playerBlack=initPlayer Black;
  playerWhite=initPlayer White;
  toMove=Black;
  finished=false;
  previousBoard=None;
  currentBoard=initBoard width height;
}

let didBoardChange gs = match gs.previousBoard with
  | None -> true
  | Some b -> b = gs.currentBoard

(*Coordinate are 0 indexed*)
let validPoint dimension point =
  point.row >= 0 &&
  point.row < fst dimension &&
  point.column >= 0 &&
  point.column < snd dimension

let updateBoard (board: board) point: board option =
  if validPoint (snd board) point then
    Some (point::(fst board), snd board)
  else
    None

