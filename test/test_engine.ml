open Abalone.Engine
open Abalone.Gen
open Utils
(* unit test *)
let test_is_valid_move board player_opt dep arr desired =
  let result = is_valid_move board player_opt dep arr in
  Alcotest.test_case "valid_move" `Quick (fun () ->
      Alcotest.(check bool) "same result" desired result)

let check_gen_pos board =
  let open QCheck in
  Test.make ~count:1 ~name:"only valid moves on board" bool (fun i ->
      let player = if i then Some N else Some B in
      let tmp_dep, tmp_arr = gen_pos board player in
      is_valid_move board player tmp_dep tmp_arr)      


let test_move res board player_opt dep arr () =
  Alcotest.(check bool)
    "same result" true
    (equal_board (move board player_opt dep arr) res)

let test_pos i j on_success on_fail =
  try
    let _ = Pos.hv i j in
    on_success
  with Invalid_pos -> on_fail

let check_pos =
  let open QCheck in
  Test.make ~count:10000 ~name:"Assert positions are well formed."
    (pair small_int small_int) (fun (i, j) ->
      let test = test_pos i j in
      match (i, j) with
      | i, j when i > 8 || j > 8 -> test false true
      | i, j when 4 - Stdlib.abs (4 - i) + 4 < j -> test false true
      | i, j when i < 0 || j < 0 -> test false true
      | _ -> true)

let () =
  let open Alcotest in
  run "Engine"
    [
      ("Positions", [ QCheck_alcotest.to_alcotest check_pos ]);
      ( "gen_pos",
      [
        QCheck_alcotest.to_alcotest (check_gen_pos init_board);
        QCheck_alcotest.to_alcotest (check_gen_pos example_1);
        QCheck_alcotest.to_alcotest (check_gen_pos example_2);
        QCheck_alcotest.to_alcotest
          (check_gen_pos
             (init
                [
                  [ Some B; Some B; Some B; Some B; Some B ];
                  [ Some B; Some B; Some B; Some B; Some B; Some B ];
                  [ None; None; Some B; Some B; Some B; None; None ];
                  [ None; None; None; None; None; None; None; None ];
                  [ None; None; None; None; None; None; None; None; None ];
                  [ None; None; None; None; None; None; None; None ];
                  [ None; None; Some N; Some N; Some N; None; None ];
                  [ Some N; Some N; Some N; Some N; Some N; Some N ];
                  [ Some N; Some N; Some N; Some N; Some N ];
                ]));
        QCheck_alcotest.to_alcotest
          (check_gen_pos
             (init
                [
                  [ Some B; Some B; Some B; Some B; Some B ];
                  [ None; None; Some B; Some B; Some B; None ];
                  [ None; None; None; None; None; None; None ];
                  [ None; None; None; Some N; None; None; None; None ];
                  [ None; None; None; Some N; None; None; None; None; None ];
                  [ None; None; Some N; None; None; None; None; None ];
                  [ None; None; None; None; None; None; None ];
                  [ None; None; None; None; None; None ];
                  [ Some N; Some N; Some N; Some N; Some N];
                ]));
      ] );
      ( "is_valid_move",
      [
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 2 ]
          [ Pos.hv 5 2 ]
          true;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 1 ]
          [ Pos.hv 5 2 ]
          false;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 2 2 ]
          [ Pos.hv 3 2 ]
          false;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 2; Pos.hv 6 3; Pos.hv 6 4; Pos.hv 7 0 ]
          [ Pos.hv 5 2; Pos.hv 5 3; Pos.hv 5 4; Pos.hv 6 0 ]
          false;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 2; Pos.hv 6 3; Pos.hv 6 4 ]
          [ Pos.hv 5 2; Pos.hv 5 3; Pos.hv 5 4 ]
          true;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 2; Pos.hv 6 3; Pos.hv 7 0 ]
          [ Pos.hv 5 2; Pos.hv 5 3; Pos.hv 6 0 ]
          false;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 2; Pos.hv 7 1 ]
          [ Pos.hv 5 2; Pos.hv 6 1 ]
          true;
        test_is_valid_move init_board (Some N)
          [ Pos.hv 6 2; Pos.hv 7 1 ]
          [ Pos.hv 5 2; Pos.hv 6 1 ]
          true;
      ] );
      ( "test_move",
        [
          test_case "test_move_diagonal_1" `Quick
            (test_move
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None; None ];
                    [ None; None; Some N; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (Some N)
               [ Pos.hv 2 3; Pos.hv 3 3; Pos.hv 4 3 ]
               [ Pos.hv 3 3; Pos.hv 4 3; Pos.hv 5 2 ]);
          test_case "test_move_diagonal_2" `Quick
            (test_move
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; Some N; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; None; Some N; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (Some N)
               [ Pos.hv 5 4; Pos.hv 4 4; Pos.hv 3 3 ]
               [ Pos.hv 4 4; Pos.hv 3 3; Pos.hv 2 2 ]);
          test_case "test_move_inline_1" `Quick
            (test_move
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; Some N; Some N; Some N; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; Some N; Some N; Some N; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (Some N)
               [ Pos.hv 3 1; Pos.hv 3 2; Pos.hv 3 3 ]
               [ Pos.hv 3 2; Pos.hv 3 3; Pos.hv 3 4 ]);
          test_case "test_move_inline_2" `Quick
            (test_move
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (init
                  [
                    [ None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; Some N; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None ];
                    [ None; None; None; None; None ];
                  ])
               (Some N)
               [ Pos.hv 3 2 ]
               [ Pos.hv 3 3 ]);
          test_case "test_move_when_pawn_is_moved" `Quick
            (test_move
               (init
                  [
                    [ Some B; Some B; Some B; Some B; Some B ];
                    [ Some B; Some B; Some B; Some B; Some B; Some B ];
                    [ None; None; Some B; Some B; Some B; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; Some N; Some N; Some N; None; None ];
                    [ Some N; Some N; Some N; Some N; Some N; Some N ];
                    [ None; Some N; Some N; Some N; Some N ];
                  ])
               (init
                  [
                    [ Some B; Some B; Some B; Some B; Some B ];
                    [ Some B; Some B; Some B; Some B; Some B; Some B ];
                    [ None; None; Some B; Some B; Some B; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; Some N; Some N; Some N; None; None ];
                    [ Some N; Some N; Some N; Some N; Some N; Some N ];
                    [ Some N; Some N; Some N; Some N; Some N ];
                  ])
               (Some N)
               [ Pos.hv 8 0; Pos.hv 7 1; Pos.hv 6 2 ]
               [ Pos.hv 7 1; Pos.hv 6 2; Pos.hv 5 3 ]);
          test_case "test_move_when_pawn_is_moved" `Quick
            (test_move
               (init
                  [
                    [ Some B; Some B; Some B; Some B; Some B ];
                    [ Some B; Some B; Some B; Some B; Some B; Some B ];
                    [ None; None; Some B; Some B; Some B; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; Some N; None; None; None; None ];
                    [ None; None; Some N; Some N; Some N; None; None ];
                    [ Some N; Some N; Some N; Some N; Some N; Some N ];
                    [ None; Some N; Some N; Some N; Some N ];
                  ])
               (init
                  [
                    [ Some B; Some B; Some B; Some B; Some B ];
                    [ Some B; Some B; Some B; Some B; Some B; Some B ];
                    [ None; None; Some B; Some B; Some B; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None; None ];
                    [ None; None; None; None; None; None; None; None ];
                    [ None; None; Some N; Some N; Some N; None; None ];
                    [ Some N; Some N; Some N; Some N; Some N; Some N ];
                    [ Some N; Some N; Some N; Some N; Some N ];
                  ])
               (Some N)
               [ Pos.hv 8 0; Pos.hv 7 1; Pos.hv 6 2 ]
               [ Pos.hv 7 1; Pos.hv 6 2; Pos.hv 5 3 ]);
               test_case "test_eject_ennemy_pawn" `Quick
               (test_move
                  (init
                     [
                       [ Some B; None; None; None; None ];
                       [ Some N; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None ];
                       [ None; None; None; None; None ];
                     ])
                  (init
                     [
                       [ Some B; None; None; None; None ];
                       [ Some B; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None ];
                       [ None; None; None; None; None ];
                     ])
                  (Some N)
                  [ Pos.hv 4 0; Pos.hv 3 0; Pos.hv 2 0 ]
                  [ Pos.hv 3 0; Pos.hv 2 0; Pos.hv 1 0 ]);
                  test_case "test_eject_own_pawn" `Quick
               (fun () -> Alcotest.check_raises "Expect Invalid move" Invalid_move 
                  (fun () -> ignore ((move
                  (init
                     [
                       [ Some N; None; None; None; None ];
                       [ Some N; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None ];
                       [ None; None; None; None; None ];
                     ])
                  (Some N)
                  [ Pos.hv 2 0; Pos.hv 1 0;]
                  [ Pos.hv 1 0; Pos.hv 0 0;]))));
                  test_case "test_side_push" `Quick
               (fun () -> Alcotest.check_raises "Expect Invalid move" Invalid_move 
                  (fun () -> ignore ((move
                  (init
                     [
                       [ Some B; None; None; None; None ];
                       [ Some B; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None ];
                       [ Some N; None; None; None; None; None; None; None ];
                       [ None; Some N; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None; None ];
                       [ None; None; None; None; None; None ];
                       [ None; None; None; None; None ];
                     ])
                  (Some N)
                  [ Pos.hv 4 1; Pos.hv 3 0; Pos.hv 2 0]
                  [ Pos.hv 3 0; Pos.hv 2 0; Pos.hv 1 0]))));
        ] );
      ( "Pos module",
        [
          test_case "int_of_hpos" `Quick (fun () ->
              check int "same result" 2 (int_of_hpos (fst hp)));
          test_case "int_of_vpos" `Quick (fun () ->
              check int "same result" 4 (int_of_vpos (snd hp)));
        ] );
      ( "occupied_position",
        [
          test_case "Some N : true" `Quick (fun () ->
              check bool "same result" true
                (occupied_position example_1 (Pos.hv 7 2 :: []) (Some N)));
          test_case "Some N : false" `Quick (fun () ->
              check bool "same result" false
                (occupied_position example_1
                   [ Pos.hv 0 0; Pos.hv 2 6; Pos.hv 2 0 ]
                   (Some N)));
          test_case "Some B : true" `Quick (fun () ->
              check bool "same result" true
                (occupied_position example_1 (Pos.hv 0 4 :: []) (Some B)));
          test_case "Some B : false" `Quick (fun () ->
              check bool "same result" false
                (occupied_position example_1
                   [ Pos.hv 1 1; Pos.hv 2 0 ]
                   (Some B)));
          test_case "None : true" `Quick (fun () ->
              check bool "same result" true
                (occupied_position example_1
                   [ Pos.hv 3 2; Pos.hv 4 3; Pos.hv 8 4 ]
                   None));
          test_case "None : false" `Quick (fun () ->
              check bool "same result" false
                (occupied_position example_1
                   [ Pos.hv 0 1; Pos.hv 7 0; Pos.hv 3 4 ]
                   None));
        ] );
    ]
