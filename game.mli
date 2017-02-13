open Aihandler
open State
open Ai


(* The game has three main functions : (1) it has to receive the action from the
 * players and update the player's status based on its actors based on the
 * character's power. (i.e  update the state of the game), and (2) need to
 * receive commands from the GUI and send updated state to the GUI.
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

(*[init_player_list ] initicializes the player list and build_card list
 * as a tuple. Returns an int, build_card list and outputs a tuple *)
val init_player_list : int -> build_card list -> (player list * build_card list)

(*[state_to_player] takes in a state and finds the specific with the
 * same name as the player input *)
val state_to_player : state -> player -> player

(*[is_robbed] takes in a state and  int and returns a updated state
 * if the player has been robbed *)
val is_robbed : state -> int -> state

(*[get_king]  takes in a player list and a state  and returns the specific
 * player who has the role king*)
val get_king : player list -> state -> player

(*[action] takes in a player , state and bot array and returns a state and
 * returns the udpated state after the player completed ones action *)
val action : player -> state -> bot array -> state

(*[gold_inc] takes in a player and returns the incremented in t *)
val gold_inc : player -> int

(*[order_list] takes in a player list and player and outputs an ordered player
 * list based on the order of the players *)
val order_list : player list -> player ->  player list

(*[killed] takes in a state and a int and checks whether the current player
 * has been killed *)
val killed : state -> int -> bool

(*[counter_find] takes in an int and a actor and returns a player with the
 * of the specific counter *)
val counter_find : int -> state -> player

(*[update_state ] updates the state after one round of the game. Inputs state
 * and bot array and outputs type state *)
val update_state : state -> bot array ->  state


(*[game_loop] enables the game to loop until a player has 6 buildings and
 * the deck has not enough cards. Inputs state, bot array and outputs print
 * statement of type unit *)
val game_loop : state -> bot array -> unit


(*[main]takes in an int and returns a unit. main enables the game to run*)
val main :int -> unit

