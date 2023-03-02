open Core.Go
open Base

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;


(*Add tests for connected onwards*)


let board =
  let dim = {width=9; height=9} in
  let t = emptyPSet () in

  (*Component1, also a chain*)
  let t = Set.add t (Piece {row=1; column=4; color=Black}) in
  let t = Set.add t (Piece {row=2; column=4; color=Black}) in
  let t = Set.add t (Piece {row=3; column=4; color=Black}) in
  let t = Set.add t (Piece {row=4; column=4; color=Black}) in

  (*Component2, also a chain*)
  let t = Set.add t (Piece {row=1; column=2; color=White}) in
  let t = Set.add t (Piece {row=2; column=2; color=White}) in
  let t = Set.add t (Piece {row=3; column=2; color=White}) in
  let t = Set.add t (Piece {row=4; column=2; color=White}) in
  let t = Set.add t (Piece {row=1; column=1; color=White}) in
  let t = Set.add t (Piece {row=2; column=1; color=White}) in
  let t = Set.add t (Piece {row=3; column=1; color=White}) in
  let t = Set.add t (Piece {row=4; column=1; color=White}) in

  (*Component3, not a chain*)
  let t = Set.add t (Piece {row=8; column=1; color=White}) in
  let t = Set.add t (Piece {row=8; column=2; color=Black}) in
  let t = Set.add t (Piece {row=8; column=3; color=White}) in
  (t,dim)

let%test_unit "boardsize" = [%test_eq: int] (Set.length (fst board)) 15

let%test_unit "connected1comp1" = [%test_eq: int] (Set.length (connected board (Piece {row=1; column=4; color=Black}))) 4
let%test_unit "chain1comp1" = [%test_eq: int] (Set.length (chain board (Piece {row=1; column=4; color=Black}))) 4

let%test_unit "connected2comp1" = [%test_eq: int] (Set.length (connected board (Piece {row=1; column=4; color=White}))) 4
let%test_unit "chain2comp1" = [%test_eq: int] (Set.length (chain board (Piece {row=1; column=4; color=White}))) 1

let%test_unit "connected3comp1" = [%test_eq: int] (Set.length (connected board (Piece {row=0; column=4; color=White}))) 5
let%test_unit "chain3comp1" = [%test_eq: int] (Set.length (chain board (Piece {row=0; column=4; color=White}))) 1
