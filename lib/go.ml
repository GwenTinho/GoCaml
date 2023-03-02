open Base
open Graph

type color = Black | White
let (=) = Poly.(=);;

type point = Piece of
  {color: (color [@sexp.opaque]) [@compare.ignore] ; row:int; column:int; }
   | Border of {row:int; column:int; } [@@deriving compare, sexp, hash]

type coord = {row:int; column:int} [@@deriving compare, sexp, hash]

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

let toCoords = function
| Piece {color=_; row; column} -> {row; column}
| Border {row; column} -> {row; column}

let l1Dist c1 c2 = abs (c1.row - c2.row) + abs (c1.column - c2.column)

let emptyPSet () : points= Set.empty (module Point)

let initPlayer color = {team=color; score=0}

let initBoard dimension: board =
  let {width; height} = dimension in
  let borderPieceCount = 2 * (width + height) in
  let pointList = List.init borderPieceCount ~f:(fun i -> match i with
  | i when i < width -> Border {row= -1; column=i}
  | i when i < width + width -> let i = i - width in Border {column=i;row=height+1}
  | i when i < 2 * width + height -> let i = i - 2 * width in Border {column= -1;row=i}
  | i -> let i = i - 2 * width - height in Border {column=width+1;row=i}
  ) in
  let pts = Set.of_list (module Point) pointList in
  (pts,dimension)

let initGame dimension = {
  playerBlack=initPlayer Black;
  playerWhite=initPlayer White;
  toMove=Black;
  finished=false;
  previousBoard=None;
  currentBoard=initBoard dimension;
}

let labelcolor = function
| Black -> "black"
| White -> "white"

let labelpiece = function
| Piece {row=_; column=_;color} -> labelcolor color
| Border _ -> "Border"

let showPoint p =
  let {row; column} = toCoords p in
  Caml.Printf.printf "(%d,%d) color: %s\n" row column (labelpiece p)

let showPoints (p:points) = Set.iter p ~f:showPoint

let didBoardChange gs = match gs.previousBoard with
  | None -> true
  | Some b ->  b = gs.currentBoard

(*Coordinate are 0 indexed*)
let validCoord dimension coord =
  coord.row >= 0 &&
  coord.row < dimension.height &&
  coord.column >= 0 &&
  coord.column < dimension.width

let validPiece dimension = function
| Border _ -> true
| p -> validCoord dimension (toCoords p)


let onBoard (board: board) point =
  let points, dim = board in
  validPiece dim point && Set.mem points point

let addPieceToBoard (board: board) point: board option =
  let points, dim = board in
  if validPiece dim point && not (Set.mem points point)  then
    Some (Set.add points point, dim)
  else
    None

let adjacant p1 p2 =
    l1Dist (toCoords p1) (toCoords p2) <= 1

let shareTeam p1 p2 = match p1,p2 with
| (Piece p1, Piece p2) -> p1.color = p2.color
| _ -> false

let isNeighbour dimension p1 p2 =
  validCoord dimension (toCoords p1) && validCoord dimension (toCoords p2) && adjacant p1 p2

(*Neighbours is reflexive!*)
let neigbours (board:board) (p:point) :point list =
  let points, dim = board in
  Set.to_list (Set.filter points ~f:(isNeighbour dim p))

let connected (board:board) (p:point) :points =
  Set.of_list (module Point) (dfs (neigbours board) p)

let neigboursSameTeam (board:board) (p:point) :point list =
  let points, dim = board in
  Set.to_list (Set.filter points ~f:(fun pp -> isNeighbour dim p pp && shareTeam p pp))

let chain (board: board) (p:point) :points =
  Set.of_list (module Point) (dfs (neigboursSameTeam board) p)

(*A point is surrounded if it has 5 neighours (1 up 1 down 1 left 1 right 1 itself)*)
let surrounded (board:board) (p:point) = List.length (neigbours board p) = 5

(*TODO find out if a chain is surrounded
  - a component is dead if every point in the component has colored neigbours or is on the edge
  *)
let isDead board (comp:points) = Set.for_all comp ~f:(surrounded board)












