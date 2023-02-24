open Base

type color = Black | White

let (=) = Poly.(=);;

type point =
  {color: (color [@sexp.opaque]) [@compare.ignore] ; row:int; column:int; }
  [@@deriving compare, sexp, hash]

module Point = struct
  module T = struct
    type t = point [@@deriving compare, sexp, hash]
  end
  include T
  include Comparable.Make(T)
end

module PSet = struct
  module T = struct
    type t = Base.Set.M(Point).t [@@deriving compare, sexp, hash]
  end
  include T
  include Comparable.Make(T)
end

type points = PSet.t

type player = {team: color; score: int}

type dimension = {width:int; height:int}

(*We model the board using only the set stones*)
type board = points * dimension



type gameState = {
  playerBlack: player;
  playerWhite: player;
  toMove: color;
  finished: bool;
  previousBoard: board option;
  currentBoard: board;
}

let l1Dist p1 p2 = abs (p1.row - p2.row) + abs (p1.column - p2.column)

let emptyPSet () : points= Set.empty (module Point)

let initPlayer color = {team=color; score=0}

let initBoard dimension: board = (emptyPSet (),dimension)

let initGame dimension = {
  playerBlack=initPlayer Black;
  playerWhite=initPlayer White;
  toMove=Black;
  finished=false;
  previousBoard=None;
  currentBoard=initBoard dimension;
}

let label = function
| Black -> "black"
| White -> "white"

let showPoint p =
  let {row; column; color} = p in
  Caml.Printf.printf "(%d,%d) color: %s\n" row column (label color)

let showPoints (p:points) = Set.iter p ~f:showPoint

let didBoardChange gs = match gs.previousBoard with
  | None -> true
  | Some b ->  b = gs.currentBoard

(*Coordinate are 0 indexed*)
let validPoint dimension point =
  point.row >= 0 &&
  point.row < dimension.height &&
  point.column >= 0 &&
  point.column < dimension.width

let onBoard (board: board) point =
  let points, dim = board in
  validPoint dim point && Set.mem points point

let addPieceToBoard (board: board) point: board option =
  let points, dim = board in
  if validPoint dim point && not (Set.mem points point)  then
    Some (Set.add points point, dim)
  else
    None

let adjacant p1 p2 =
    l1Dist p1 p2 <= 1

let shareTeam p1 p2 = p1.color = p2.color

let isNeighbour dimension p1 p2 =
  validPoint dimension p1 && validPoint dimension p2 && adjacant p1 p2

  (*update code to allow to fill component out!!*)
let connected (board:board) (p:point) :points =
  let points, dim = board in
  Set.filter points ~f:(isNeighbour dim p)

let chain (board: board) (p:point) :points =
  let points, dim = board in
  Set.filter points ~f:(fun pp -> isNeighbour dim p pp && shareTeam p pp)

let maxLiberties dimension p =
  let rem = if p.row = 0 || p.row = dimension.height - 1 then
    3 else 4 in
  let rem = if p.column = 0 || p.column = dimension.width - 1 then
    rem - 1 else rem in
  rem


(*finds the liberties of the point if it is in the board otherwise it will find the liberties if the point was added*)
let liberties board p =
    let c = connected board p in
    maxLiberties (snd board) p - Set.length c

