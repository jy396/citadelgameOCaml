open Aihandler
open State
open Ai


(* The game has three main functions : (1) it has to receive the action from the
 * players and update the player's status based on its actors based on the
 * character's power. (i.e  update the state of the game), and (2) need to
 * receive commands from the GUI and send updated state to the GUI.
 *)

(* (*[random_card] returns a random card from any list of cards*)
val random_card : 'a list -> 'a

[random_card] returns a tuple of n random cards from any list of cards
 * and a new list with those cards taken out
val n_random_cards : 'a list -> int -> ('a list * 'a list) *)

(*[inv_match_actors] takes in an int and returns the corresponding actor*)
val int_to_actor : int -> actor

(*[actor_to_int] takes in an actor and returns the corresponding int*)
val actor_to_int : actor -> int

(*[replace] returns an updated player list by replacing the player's old information
 * with new information. *)
(* val replace :  state -> player  -> player list
 *)
(*[update_hand] draws 2 cards and lets the player choose which to discard.
 *Returns an updated state with the discarded card added back in the deck
 * and the chosen card added to the player's hand*)
val update_hand : state -> player -> state

(*[action] performs an action by either drawing two gold pieces or
 * drawing two cards and discarding one of them. Returns an updated state. *)
val action : player -> state -> bot array -> state

(*[gold_inc] takes in a player and incrememnts the player's gold
 * according to the player's role*)
val gold_inc : player -> int

(* val power_thief : state-> player -> bot array -> state
val power_assassin : state -> player -> bot array -> state
val power_magician : state -> player -> bot array -> state
val power_king : state -> player -> state
val power_merchant : state -> player -> state
val power_bishop : state -> player -> state
val power_architect : state -> player -> bot array -> state
val power_warlord : state -> player -> bot array -> state
 *)
val main :int -> unit

(*[state_to_player] takes in a state and finds the specific with the
 * same name as the player input *)
val state_to_player : state -> player -> player

(*[is_robbed] takes in a state and  int and returns a updated state
 * if the player has been robbed *)
val is_robbed : state -> int -> state

(*[get_king]  takes in a player list and a state  and returns the specific
 * player who has the role king*)
val get_king : player list -> state -> player
val action : player -> state -> bot array -> state

(*[gold_inc] takes in a player and returns the incremented in t *)
val gold_inc : player -> int

(*[order_list] takes in a player list and player and outputs an ordered player
 * list based on the order of the players *)
val order_list : player list -> player ->  player list

(*[killed] takes in a state and a int and checks whether the current player
 * has been killed *)
val killed : state -> int -> bool

(*[actor_to_player] takes in a actor and state and outputs a player whose role
 * is the same as the input actor *)
val actor_to_player : actor -> state -> player


(*[counter_Find] takes in an int and a actor and returns a player with the
 * specific *)
val counter_find : int -> state -> player
