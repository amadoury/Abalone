open Engine

type trace = (pos list * pos list) list

(** Game outcome *)
type endplay = Win of player | Giveup of player

val pp_endplay : Format.formatter -> endplay -> unit
val equal_endplay : endplay -> endplay -> bool

type result = {
  trace : trace; (* Trace *)
  endplay : endplay;  (** Outcome *)
  final : board;  (** Final state *)
}

val arena :
  ?init_player:player ->
  ?init_board:board ->
  (player -> board -> pos list * pos list) ->
  result
(** Final state of the game *)

val pair :
  n:(board -> pos list * pos list) ->
  b:(board -> pos list * pos list) ->
  player ->
  board ->
  pos list * pos list

val player_giveup : board -> pos list * pos list
val player_cli : player -> board -> pos list * pos list
val player_random : board -> pos list * pos list
val player_random_2 : board -> pos list * pos list

