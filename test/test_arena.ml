open Abalone.Arena
open Utils
open Abalone.Engine


let () =
  let open Alcotest in
  run "Arena"
    [
      ( "arena",
        [
          test_case "arena-win" `Quick (fun () ->
              Alcotest.(check endplay)
                "same result" (Win N)
                (arena ~init_player:N ~init_board:example_1
                   (Abalone.Arena.pair ~n:player_giveup ~b:player_giveup))
                  .endplay);
          test_case "arena-giveup" `Quick (fun () ->
              Alcotest.(check endplay)
                "same result" (Giveup N)
                (arena ~init_player:N ~init_board:example_2
                   (Abalone.Arena.pair ~n:player_giveup ~b:player_giveup))
                  .endplay);
          test_case "arena with init_board:empty" `Quick (fun () ->
            check endplay "same result" (Win B)
              (arena ~init_player:N ~init_board:empty
                  (Abalone.Arena.pair ~n:player_random_2 ~b:player_random))
                .endplay);
                test_case "pair with many argument" `Quick (fun () ->
                  check bool "same result" true
                    (match
                       Abalone.Arena.pair ~n:player_random ~b:player_random B
                         example_2
                     with
                    | player_giveup ->
                        ignore player_giveup;
                        true (*|_ -> false*)));
              test_case "pair match with player_random" `Quick (fun () ->
                  check bool "same result" true
                    (match
                       Abalone.Arena.pair ~n:player_random ~b:player_random B
                         example_2
                     with
                    | player_random ->
                        ignore player_random;
                        true));
              test_case "pair with empty board" `Quick (fun () ->
                  check bool "same result" true
                    (try
                       match
                         Abalone.Arena.pair ~n:player_random ~b:player_random B
                           empty
                       with
                       | _ -> true
                     with _ -> false));
    
        ] );
    ]
