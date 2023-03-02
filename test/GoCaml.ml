open Core.Go
open Base

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;

let p1:point = (Piece {row=3; column=4; color=Black})
let p2:point = (Piece {row=4; column=4; color=Black})
let p3:point = (Piece {row=5; column=4; color=Black})

let c1 = {row=3; column=4}
let c2 = {row=4; column=4}
let c3 = {row=5; column=4}

(*L1 distance tests*)
let%test "metric1" = l1Dist c1 c1 = 0
let%test "metric2" = l1Dist c1 c2 = 1
let%test "metric3" = l1Dist c3 c2 = 1
let%test "metric4" = l1Dist c1 c3 = 2

(*Valid point*)
let dim = {width=5; height=5}

let%test "valid1" = validPiece dim p1
let%test "valid2" = validPiece dim p2
let%test "valid3" = not (validPiece dim p3)

let p4:point = (Piece {row=0; column=1; color=Black})

let b: board =
  let t = emptyPSet () in
  let t = Set.add t p1 in
  let t = Set.add t p2 in
  let t = Set.add t p4 in
  (t,dim)


(*On Board tests*)

let pw = (Piece {row=3; column=4; color=White})

let%test "onBoardTest1" = onBoard b p1
let%test "onBoardTest2" = onBoard b p2
let%test "onBoardTest3" = not (onBoard b p3)
let%test "onBoardTest4" = onBoard b p4
let%test "onBoardTest5" = onBoard b pw

let p5:point = (Piece {row=0; column=0; color=Black})
let p6:point = (Piece {row=3; column=1; color=Black})
let p7:point = (Piece {row=5; column=1; color=Black})

(*Add pieces tests*)

let%test "addPieceTest1" = addPieceToBoard b p5 <> None
let%test "addPieceTest2" = addPieceToBoard b p6 <> None
let%test "addPieceTest3" = addPieceToBoard b p7 = None
let%test "addPieceTest4" = addPieceToBoard b p1 = None

(*Share team & adjacant & isNeighbour*)

let%test "sameTeam1" = shareTeam p1 p2
let%test "sameTeam2" = not (shareTeam p1 pw)

let%test "adjacant1" = adjacant p1 p2
let%test "adjacant2" = adjacant p1 p1
let%test "adjacant3" = adjacant p2 pw
let%test "adjacant4" = not (adjacant p1 p3)
let%test "adjacant5" = adjacant p2 p3

let%test "isNeighbour1" = isNeighbour dim p1 p2
let%test "isNeighbour2" = isNeighbour dim p1 p1
let%test "isNeighbour3" = isNeighbour dim p2 pw
let%test "isNeighbour4" = not (isNeighbour dim p1 p3)
let%test "isNeighbour5" = not (isNeighbour dim p2 p3)
let%test "isNeighbour6" = isNeighbour dim (Piece {row=3; column=3;color=Black}) pw
