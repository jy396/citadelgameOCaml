open State

type personality =
{
  assassin :int;
  thief : int;
  magician : int;
  king : int;
  bishop : int;
  merchant : int;
  architect : int;
  warlord : int;
}

type bot =
{
  ai_name : int;
  personality : personality;
  actors_before : State.actor list;
  actors_after : State.actor list
}

val set_actors_before_after : state -> bot -> bot
val choose_from_available : bot -> state -> actor
val choose_action : int -> state -> string
val choose_build : player -> player list -> build_card option
val assassin_target : bot ->  state ->  int -> personality  -> actor
val set_probability : state -> bot -> personality
val thief_target : state -> int -> bot -> actor
val magician_action :int -> state -> string
val architect_move :int -> state -> build_card option
val warlord_target : int -> state -> player option
val building_to_destroy : player option -> int -> state -> build_card option
val change_personality : int -> state -> personality -> personality
val choose_discard : state -> int -> build_card list -> build_card