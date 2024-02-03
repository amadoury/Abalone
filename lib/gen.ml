open Engine

(* generate first pos *)
let fst_pt () =
  Random.self_init ();
  let rand_i = Random.int 9 in
  if rand_i <= 4 then
    let rand_j = Random.int 4 + rand_i + 1 in
    Pos.hv rand_i rand_j
  else
    let rand_j = Random.int 4 + (9 - rand_i) in
    Pos.hv rand_i rand_j

let vec_up = [ (-1, -1); (-1, 0); (0, 1); (1, 1); (1, 0); (0, -1) ]
let vec_mid = [ (-1, -1); (-1, 0); (0, 1); (1, 0); (1, -1); (0, -1) ]
let vec_down = [ (-1, 0); (-1, 1); (0, 1); (1, 0); (1, -1); (0, -1) ]

(* generate a valid departure position (dep : pos list) *)
let gen_dep () =
  let pt = fst_pt () in

  let len_dep = Random.int 3 in
  let rand = Random.int 6 in
  let rec aux v acc =
    let nb, list_dep = acc in
    if nb = 0 then list_dep
    else
      let vi, vj = (int_of_hpos (fst v), int_of_vpos (snd v)) in
      let list_vec =
        if vi < 4 then vec_up else if vi = 4 then vec_mid else vec_down
      in
      try
        let v1 =
          Pos.hv
            (vi + fst (List.nth list_vec rand))
            (vj + snd (List.nth list_vec rand))
        in
        aux v1 (nb - 1, list_dep @ [ v1 ])
      with Invalid_pos -> list_dep
  in
  aux pt (len_dep, [ pt ])

(* generate a valid arrival list based on the departure list passed to the function *)
let gen_arr dep =
  let rec aux () =
    try
      let rand = Random.int 6 in
      dep
      |> List.map (fun v ->
             let vi, vj = (int_of_hpos (fst v), int_of_vpos (snd v)) in
             let list_vec =
               if vi < 4 then vec_up else if vi = 4 then vec_mid else vec_down
             in
             Pos.hv
               (vi + fst (List.nth list_vec rand))
               (vj + snd (List.nth list_vec rand)))
    with Invalid_pos -> aux ()
  in
  aux ()

(* check if the pawn in dep is for player *)
let gen_pos (board : board) (player_opt : player option) =
  let rec aux b p_opt =
    let dep = gen_dep () in
    if occupied_position b dep p_opt then
      let arr = gen_arr dep in
      if is_valid_move b p_opt dep arr then (dep, arr) else aux b p_opt
    else aux b p_opt
  in
  aux board player_opt
