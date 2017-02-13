(* Contains data and helper functions to represent game state.*)

(* [actor] represents possible job roles that the players can pick.
 * Nil is a placeholder used during initialization, before players pick
 * the role they wish to play during the round.*)
type actor  =
 Nil | Assassin | Thief | Magician | King | Bishop | Merchant
 | Architect | Warlord

(* [build_card] represents an individual building card. 
 * Each building card is built from parsing the JSON from
 * the deck.json file. *)
type build_card  = {
card_number: int;
name: string;
color: string;
score: int;
cost: int;
description: string}

(* [player] represents an individual player in the game.
 * The players are given a "name", which is a number from 0
 * up to the number of players in the game - 1. 
 * Player 0 is ALWAYS the human player.
 * The [buildings] field represents buildings built by the player,
 * and the hand represents the cards the player is holding. 
 * The [is_ai] flag is used to check if the input from this player
 * is from the ai (and thus from aihandler.ml) or from console. *)
type player = {
name: int;
is_ai: bool;
role : actor ;
points: int;
gold : int ;
buildings : build_card list ;
hand : build_card list
}

(* [state] represents the overall state of the game, keeping track of
 * who is picked/not picked during a picking phase, what cards are remaining
 * in deck, and who has been killed/robbed/is the king. *)
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

(* [n_random_cards] returns n number of cards randomly from a given 
 * deck of cards. It is polymorphic to allow use in decks containing 
 * player cards or building cards.*)
val n_random_cards : 'a list -> int -> ('a list * 'a list)

(* [replace] takes in an individual player and returns the updated list of 
 * players that reflects the update in the given player input.*)
val replace: state -> player -> player list

(* [type_actor] takes in a string and returns the actor represented by that string.
 * ex) "warlord" returns Warlord *)
val type_actor : string -> actor

(* [match_actors] takes in an int and returns the actor of that number.
 * Numbers are from 1 to 8, and the number of the job is the turn that the actor is revealed.
 * For example, 1 -> Assassin, 2 -> Thief. See README for the number-actor pairing.*)
val match_actors : int -> actor

(* [actor_to_string] is the inverse function of [type_actor].*)
val actor_to_string : actor -> string

(* [gold_inc] checks the player's role and the number of royalty buildings the player has
 * related to the particular role, and returns how much the player's gold should increase
 * as a result of using the powers related to his/her job.*)
val gold_inc : player -> int

(* [find_player] returns the player with the given number in the given state. *)
val find_player : int -> state -> player

(* [is_four_colors] checks and returns true if the player has buildings of four different colors.*)
val is_four_colors : player -> bool

(* [inv_match_actors] is the inverse function of [match_actors].*)
val inv_match_actors : actor -> int

(*[actor_to_player] returns a player given the actor and the state.*)
val actor_to_player : actor -> state -> player 