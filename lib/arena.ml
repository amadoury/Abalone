open Engine
open Gen

let swap = function N -> B | B -> N

type trace = (pos list * pos list) list
type endplay = Win of player | Giveup of player

let equal_endplay e1 e2 =
  match (e1, e2) with
  | Win p1, Win p2 -> p1 = p2
  | Giveup p1, Giveup p2 -> p1 = p2
  | _ -> false

let pp_endplay oc = function
  | Win p -> Format.fprintf oc "Win %a" pp_player p
  | Giveup p -> Format.fprintf oc "Giveup %a" pp_player p

type result = { trace : trace; endplay : endplay; final : board }

let player_giveup _board = ([], [])

let arena ?(init_player = N) ?(init_board = Engine.init_board) players =
  let rec go board player trace =
    let opponent = swap player in
    match win board with
    | Some N -> { trace = List.rev trace; endplay = Win N; final = board }
    | Some B -> { trace = List.rev trace; endplay = Win B; final = board }
    | None ->
        let rec f () =
          try
            match players player board with
            | [], [] ->
                {
                  trace = List.rev trace;
                  endplay = Giveup player;
                  final = board;
                }
            | dep, arr ->
                go
                  (move board (Some player) dep arr)
                  opponent ((dep, arr) :: trace)
          with _ -> f ()
        in
        f ()
  in
  go init_board init_player []

let player_random board =
  gen_pos board (Some B)

let player_random_2 board =
  gen_pos board (Some N)

let pair ~n:player_N ~b:player_B player =
  match player with N -> player_N | B -> player_B

let player_cli player board =
  Format.printf "@[<v>Player %a to play @," pp_player player;
  Format.printf "@[<v>%a@]@," pp_board board;
  let get_coord () =
    Format.printf "Enter coord (i,j) @]@.";
    try Scanf.scanf " %d %d\n" (fun i j -> Some (Pos.hv i j))
    with Scanf.Scan_failure _ -> None
  in
  let rec get_list_coord n acc =
    if n = 0 then acc else get_list_coord (n - 1) (acc @ [ get_coord () ])
  in
  let rec len_list () =
    Format.printf "numbers of pawn to move : @]@.";
    try
      Scanf.scanf " %d\n" (fun i ->
          if i <= 0 || i > 3 then len_list () else Some i)
    with Scanf.Scan_failure _ -> None
  in
  let n = len_list () in
  let f nb =
    List.map
      (fun i -> match i with Some (a, b) -> (a, b) | None -> Pos.hv 0 0)
      (get_list_coord nb [])
  in
  match n with
  | Some nb ->
      let d =
        Format.printf "starting positions : @]@.";
        f nb
      in
      ( d,
        (Format.printf "ending positions : @]@.";
         f nb) )
  | None -> ([], [])
