open Abalone.Engine
open Abalone.Arena

let board =
  Alcotest.testable
    (Fmt.of_to_string (Format.asprintf "%a" pp_board))
    equal_board

let endplay =
  Alcotest.testable
    (Fmt.of_to_string (Format.asprintf "%a" pp_endplay))
    equal_endplay

let example_1 =
  init
    [
      [ None; Some B; None; Some B; Some B ];
      [ None; None; None; Some B; Some B; None ];
      [ Some N; None; None; None; Some B; None; None ];
      [ None; Some N; None; None; Some B; Some B; None; None ];
      [ None; None; Some N; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; None; None ];
      [ None; None; None; None; Some N; None; None ];
      [ Some N; Some N; Some N; None; Some N; None ];
      [ Some N; Some N; Some N; Some N; None ];
    ]

let example_2 =
  init
    [
      [ Some B; Some B; None; Some B; Some B ];
      [ None; None; None; Some B; Some B; None ];
      [ Some N; None; None; None; Some B; None; None ];
      [ None; Some N; Some B; Some B; Some B; Some B; None; None ];
      [ None; None; Some N; None; None; None; None; None; None ];
      [ None; None; None; None; None; None; Some N; None ];
      [ None; None; None; None; Some N; None; None ];
      [ None; Some N; Some N; None; Some N; None ];
      [ Some N; Some N; Some N; Some N; None ];
    ]

let hp = Pos.hv 2 4

let catch_arena f =
  try match f (pair ~n:player_random_2 ~b:player_random) with _ -> true
  with _ -> false
