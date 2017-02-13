type actor  =
 Nil | Assassin | Thief | Magician | King | Bishop | Merchant
 | Architect | Warlord

type build_card  = {
card_number: int;
name: string;
color: string;
score: int;
cost: int;
description: string}

type player = {
name: int;
is_ai: bool;
role : actor ;
points: int;
gold : int ;
buildings : build_card list ;
hand : build_card list
}

type state = {

players : player list ; (*player,points *)
deck : build_card list  ;
king : player;
faceup :  actor list;
facedown : actor list  ;
killed : actor;
robbed : actor ; (*player number*)
robber : actor;
available_actors : actor list ;
}
val n_random_cards : 'a list -> int -> ('a list * 'a list)
val replace: state -> player -> player list
val type_actor : string -> actor
val match_actors : int -> actor

val actor_to_string : actor -> string
val gold_inc : player -> int
val find_player : int -> state -> player
val is_four_colors : player -> bool
val match_actors : int -> actor
val inv_match_actors : actor -> int