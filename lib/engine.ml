type player = N | B
type board = player option list list

(* [hpos] and [vpos] are sealed off to avoid producing invalid
   positions. Only way to create indices is through the [Pos]
   module. *)

type hpos = H of int
type vpos = V of int
type pos = hpos * vpos

let int_of_hpos (H i) = i
let int_of_vpos (V i) = i

exception Invalid_pos
exception Invalid_move

let equal_board b b1 = b = b1

(* Make sure pretty-printed notation can be parsed back by OCaml *)
let pp_hpos oc = function H i -> Format.fprintf oc "Pos.(h %d)" i
let pp_vpos oc = function V i -> Format.fprintf oc "Pos.(v %d)" i
let pp_pos oc = function H i, V j -> Format.fprintf oc "Pos.hv %d %d" i j
let equal_hpos (H i) (H j) = i = j
let equal_vpos (V i) (V j) = i = j
let equal_pos (H i1, V j1) (H i2, V j2) = i1 = i2 && j1 = j2

(* Setup a namespace for convenient short notations *)
module Pos = struct
  let hv i j =
    if
      i < 0 || j < 0 || i > 8
      || (i <= 4 && j > 4 + i)
      || (i > 4 && j > 4 + (8 - i))
    then raise Invalid_pos;
    (H i, V j)
end

let init lb =
  assert (List.length lb = 9);

  let f (valid, line_length, line_length_change) line =
    let line_length_change =
      if line_length = 9 then -line_length_change else line_length_change
    in
    ( List.length line = line_length && valid,
      line_length + line_length_change,
      line_length_change )
  in
  let g = function a, _, _ -> a in

  assert (g (List.fold_left f (true, 5, 1) lb));
  lb

let empty =
  [
    [ None; None; None; None; None ];
    [ None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None; None ];
    [ None; None; None; None; None; None ];
    [ None; None; None; None; None ];
  ]

let init_board =
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
  ]

let pp_player oc p =
  match p with N -> Format.fprintf oc "N" | B -> Format.fprintf oc "B"

let get b (H i) (V j) = List.nth (List.nth b i) j

let has_duplicate dep =
  dep
  |> List.exists (fun v -> List.length (List.filter (fun x -> x = v) dep) <> 1)

(* *)
let has_duplicate_hpos dep =
  dep
  |> List.exists (fun (H i, V _) ->
         List.length (List.filter (fun (H i', V _) -> i = i') dep) <> 1)

let possible_placement p1 p2 =
  ( int_of_hpos (fst p2) - int_of_hpos (fst p1),
    int_of_vpos (snd p2) - int_of_vpos (snd p1) )

let next_board_vector x y line =
  if x = 0 then (x, y)
  else if line = 4 then
    match (x, y) with
    | -1, 0 -> (-1, -1)
    | -1, 1 -> (-1, 0)
    | 1, 0 -> (1, -1)
    | 1, 1 -> (1, 0)
    | _ -> (x, y)
  else (x, y)
    

(* is_inline : checks if the coordinates of the starting pawns are in line and consecutive *)
let rec is_consec_inline (dep : pos list) =
  match dep with
  | [] -> true
  | (H _, V _) :: [] -> true
  | (H i, V j) :: ((H i', V j') :: _ as t) ->
      if i <> i' || (abs (j - j') <> 1 && i = i') || has_duplicate dep then
        false
      else is_consec_inline t

let rec is_consec_diagonal_aux dep =
  match dep with
  | [] -> true
  | _ :: [] -> true
  | (H i, V j) :: ((H i', V j') :: _ as t) ->
      if not (has_duplicate_hpos dep) then
        if (i <= 4 && i' < 4) || (i < 4 && i' <= 4) then
          if i' - i = 1 && (j = j' || j' - j = 1) then is_consec_diagonal_aux t
          else if i' - i = -1 && (j = j' || j' - j = -1) then
            is_consec_diagonal_aux t
          else false
        else if (i >= 4 && i' > 4) || (i > 4 && i' >= 4) then
          if i' - i = 1 && (j = j' || j' - j = -1) then is_consec_diagonal_aux t
          else if i' - i = -1 && (j = j' || j' - j = 1) then
            is_consec_diagonal_aux t
          else false
        else false
      else false

let is_consec_diagonal dep = 
  match dep with 
  | [] -> true
  | _ :: [] -> true
  |_ :: _ :: [] -> is_consec_diagonal_aux dep
  | p1 :: p2 :: p3 :: [] ->
    let x, y = possible_placement p1 p2 in 
    let x1, y1 = possible_placement p2 p3 in
    let x,y = next_board_vector x y (int_of_hpos(fst p1)) in
    (x, y) = (x1, y1) && is_consec_diagonal_aux [p1; p2; p3]
  |_ -> false


(* check if the inline move is valid *)
let is_shift_inline dep arr =
  if is_consec_inline dep && is_consec_inline arr then
    if
      abs
        (int_of_hpos (fst (List.nth dep 0)) - int_of_hpos (fst (List.nth arr 0)))
      = 1
    then List.for_all2 (fun p p' -> is_consec_diagonal [ p; p' ]) dep arr
    else if
      int_of_hpos (fst (List.nth dep 0)) = int_of_hpos (fst (List.nth arr 0))
    then
      let rec aux dep arr =
        match (dep, arr) with
        | [], [] -> true
        | (H _, V j) :: t, (H _, V j') :: t' ->
            if abs (j - j') = 1 then aux t t' else false
        | _ -> false
      in
      aux dep arr
    else false
  else false

(* check if the diagonal move is valid *)
let is_shift_diagonal dep arr =
  if is_consec_diagonal dep && is_consec_diagonal arr then
    let flagi =
      List.for_all2
        (fun vd va -> int_of_hpos (fst vd) = int_of_hpos (fst va))
        dep arr
    in
    let flagj =
      List.for_all2
        (fun vd va -> abs (int_of_vpos (snd vd) - int_of_vpos (snd va)) = 1)
        dep arr
    in
    if flagi && flagj then true
    else if
      List.length arr = 1
      && (is_consec_diagonal (dep @ arr) || is_consec_inline (dep @ arr))
    then true
    else if List.length dep == List.length arr then
      List.for_all2 (fun p p' -> is_consec_diagonal [ p; p' ]) dep arr
      || List.for_all2 (fun p p' -> is_consec_inline [ p; p' ]) dep arr
    else false
  else false

(* valid_move : check if the move is valid *)
let is_valid_shift dep arr =
  let len_dep = List.length dep in
  let len_arr = List.length arr in
  if len_dep = 0 || len_arr = 0 || len_dep <> len_arr || len_dep > 3 then false
  else is_shift_inline dep arr || is_shift_diagonal dep arr


let occupied_position board dep player_opt =
  List.fold_left (fun acc (x, y) -> acc && get board x y = player_opt) true dep


let placeToPush x0 y0 x y =
  let rec aux k xt yt xv yv list =
    if k = 0 then list
    else
      try
        let xv, yv = next_board_vector xv yv xt in
        let xt, yt = (xt + xv, yt + yv) in
        [ Pos.hv xt yt ] @ aux (k - 1) xt yt xv yv list
      with Invalid_pos -> list
  in
  aux 3 x0 y0 x y []

let placeToPushWithVector dep arr =
  let len = List.length dep in
  if equal_pos (List.nth dep 1) (List.nth arr 0) then
    let x, y =
      possible_placement (List.nth dep (len - 2)) (List.nth dep (len - 1))
    in
    let p = List.nth dep (len - 1) in
    let x0, y0 = (int_of_hpos (fst p), int_of_vpos (snd p)) in
    placeToPush x0 y0 x y
  else
    let x, y = possible_placement (List.nth dep 1) (List.nth dep 0) in
    let p = List.nth dep 0 in
    let x0, y0 = (int_of_hpos (fst p), int_of_vpos (snd p)) in
    placeToPush x0 y0 x y

(* verify if the move is valid*)
let is_valid_move board player_option dep arr =
  (* if the shift is invalid or the starting list does not contain the player's pawns
     then we return false *)
  if
    (not (is_valid_shift dep arr))
    || not (occupied_position board dep player_option)
  then false
    (*if the arrival list is empty either we do lateral shift on empty squares or
       we move only one pawn on an emty square then we return true*)
  else if occupied_position board arr None then true
  else if List.length dep = 1 then false
  else
    let x, y = possible_placement (List.nth dep 0) (List.nth dep 1) in
    let x', y' = possible_placement (List.nth dep 0) (List.nth arr 0) in
    if not (x = x' && y = y') then false
    else
      let place = placeToPushWithVector dep arr in
      let a = match player_option with Some a -> a | _ -> N in
      Format.printf "START PLACE %a" pp_player a;
      List.iter
        (fun (i, j) ->
          Format.printf "place:%d %d. " (int_of_hpos i) (int_of_vpos j))
        place;
      Format.printf "@,";
      if occupied_position board [ List.nth place 0 ] player_option then false
      else
        (*function which counts the number of pawns in the list from p_opt*)
        let nb p_opt l =
          let f acc (a, b) =
            if List.nth (List.nth board (int_of_hpos a)) (int_of_vpos b) = p_opt
            then acc + 1
            else acc
          in
          List.fold_left f 0 l
        in
        let valid p_opt_adv =
          let p_opt =
            match p_opt_adv with
            | Some N -> Some B
            | Some B -> Some N
            | _ -> None
          in
          let len = List.length place in
          let nb_opt = nb p_opt place in
          let nb_opt_adv = nb p_opt_adv place in
          (* if there's only one pawn we can move it*)
          if len = 1 then true
            (* if there 3 pawns or there's 2 adversary pawns at the start while we need to move 2 pawns
               or there's 1 pawn from the player on the first case then we can't move
                  else we can*)
          else if
            nb_opt + nb_opt_adv = 3
            || nb_opt_adv = 2
               && List.length dep = 2
               && (len = 2
                  ||
                  let a, b = List.nth place 2 in
                  not
                    (List.nth (List.nth board (int_of_hpos a)) (int_of_vpos b)
                    = p_opt_adv))
            || (let a, b = List.nth place 0 in
                List.nth (List.nth board (int_of_hpos a)) (int_of_vpos b))
               = p_opt
          then false
          else true
        in
        match player_option with
        | Some N -> valid (Some B)
        | Some B -> valid (Some N)
        | _ -> false

let mapi k b =
  b |> List.mapi (fun i line -> line |> List.mapi (fun j v -> k (H i) (V j) v))

let set (b : board) i j (p : player option) : board =
  b |> mapi (fun si sj v -> if equal_hpos i si && equal_vpos j sj then p else v)

let move board player_option dep arr =
  if not (is_valid_move board player_option dep arr) then raise Invalid_move
  else
    (* function which changes all the positions given in the list
       to p*)
    let rec set_list l p b =
      match l with [] -> b | (h, v) :: pl -> set_list pl p (set b h v p)
      (*we remove the pawns that we'll move*)
    in
    let b = set_list dep None board in
    let player_option_adv =
      match player_option with Some N -> Some B | Some B -> Some N | _ -> None
    in
    (*if we move only one pawn or do a lateral shift, there is no pawns pushed
        then we can place the pawns*)
    if List.length arr = 1 then set_list arr player_option b
    else
      let x, y = possible_placement (List.nth dep 0) (List.nth dep 1) in
      let x', y' = possible_placement (List.nth dep 0) (List.nth arr 0) in

      if not (x = x' && y = y') then set_list arr player_option b
      else
        (*list of positions after dep*)
        let place = placeToPushWithVector dep arr in
        (*we get the number of pawns pushed*)
        let _, nb =
          List.fold_left
            (fun (b, n) (H i, V j) ->
              if not b then (b, n)
              else if get board (H i) (V j) = None then (not b, n)
              else (b, n + 1))
            (true, 0) place
        in
        (*if we push 2 pawns there will be one one the third case and we place our pawns on the first case
           and other before
           (example : BBBNNO
           -> (previously we removed our pawns) OOONNO
           -> (we add the adversary pawn on the third case of place ) OOONNN
           -> we add our pawns on the arrival positions OBBBNN)
           same if we push only 1 pawn (example : BBNO -> OONO -> OOON -> OBBN)
           else we only remove then add our pawns on arrival (example : BBBO** -> OOOO** -> OBBB** ) *)
        let b =
          if
            (nb = 2 && List.length place = 2)
            || (nb = 1 && List.length place = 1)
          then b
          else if nb = 2 then
            set b
              (fst (List.nth place 2))
              (snd (List.nth place 2))
              player_option_adv
          else if nb = 1 then
            set b
              (fst (List.nth place 1))
              (snd (List.nth place 1))
              player_option_adv
          else b
        in
        set_list arr player_option b

(* pretty-printer for board *)
let pp_board oc b =
  [ (0, 4); (1, 5); (2, 6); (3, 7); (4, 8); (5, 7); (6, 6); (7, 5); (8, 4) ]
  |> List.iter (fun (i, k) ->
         let rec space len =
           if len = 0 then ()
           else (
             Format.fprintf oc " ";
             space (len - 1))
         in
         Format.fprintf oc "%d " i;
         space (8 - k);
         List.init (k + 1) (fun x -> x)
         |> List.iter (fun j ->
                match get b (H i) (V j) with
                | None -> Format.fprintf oc "O "
                | Some p -> Format.fprintf oc "%a " pp_player p);
         Format.fprintf oc "@.")

let win board =
  let n, b =
    List.fold_left
      (List.fold_left (fun (n, b) x ->
           match x with
           | Some N -> (n + 1, b)
           | Some B -> (n, b + 1)
           | None -> (n, b)))
      (0, 0) board
  in
  if n <= 8 then Some B else if b <= 8 then Some N else None

