type player = N | B

type hpos
(** horizontal position (row) *)

type vpos
(** vertical position (column) *)

type board
type pos = hpos * vpos

exception Invalid_pos

exception Invalid_move
(** Raised when a player attempts to overtake a pre-existing position *)

(** Smart position constructors *)
module Pos : sig
  val hv : int -> int -> pos
  (** Convert an integer into a position. [raise
      Invalid_pos] if the i and j are out of the board *)
end

val int_of_hpos : hpos -> int
(** convert hpos to int *)

val pp_hpos : Format.formatter -> hpos -> unit
(** pp_printter for hpos type *)

val equal_hpos : hpos -> hpos -> bool
(** equal_hpos compare two hpos *)

val int_of_vpos : vpos -> int
(** convert vpos to int *)

val pp_vpos : Format.formatter -> vpos -> unit
(** pretty_printer for *)

val equal_vpos : vpos -> vpos -> bool
(** equal_vpos compare two vpos *)

val pp_pos : Format.formatter -> pos -> unit
val equal_pos : pos -> pos -> bool

val equal_board : board -> board -> bool
(** compare two board *)

(* pretty printer for player *)
val pp_player : Format.formatter -> player -> unit

val occupied_position : board -> pos list -> player option -> bool
(** check pos is occupied by player *)

val pp_board : Format.formatter -> board -> unit
(** pretty printer for board *)

val is_valid_move : board -> player option -> pos list -> pos list -> bool
(** check if the move is valid *)

val move : board -> player option -> pos list -> pos list -> board
(** do the move from pos to arr *)

val init : player option list list -> board
(** val init : player option list list -> board *)

val empty : board
(** [empty] board, with no position taken. *)

val init_board : board
(**  board, with the initial position for each player. *)

val get : board -> hpos -> vpos -> player option
(** [get board hpos vpos] queries the state of the [board] at the
    given position [(hpos, vpos)]. *)

val set : board -> hpos -> vpos -> player option -> board
(** [set board hpos vpos player] puts down the [player]'s mark at the
    given position [(hpos, vpos)]. *)

(* check if a player win the game *)
val win : board -> player option
