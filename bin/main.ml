open Abalone.Engine
open Abalone.Arena

let () =
  (* let board = init_board in
     pp_board Format.std_formatter board;
     let dep,arr = player_cli N board in
     (* let arr = [ Pos.hv 1 0; Pos.hv 2 0 ] in *)
     List.iter (fun (i,j) -> Format.printf "%d %d " (int_of_hpos i) (int_of_vpos j)) dep;
     List.iter (fun (i,j) -> Format.printf "%d %d " (int_of_hpos i) (int_of_vpos j)) arr;
     Format.printf "After move @,";
     let b' = move init_board (Some B) dep arr in
     pp_board Format.std_formatter b' *)
  let result = arena (pair ~n:(player_random) ~b:player_random) in
  Format.printf "%a@," pp_board result.final;
  Format.printf "Game ends with %a@," pp_endplay result.endplay
