(*Interface for an input/output module that prints out help messages
 *and takes in input. *)
 
open State

(*[command] takes in a state and prints out a description of
 * the current actor's power*)
val command : state -> string

(*[choose_card] allows the player to discard one of two cards*)
val choose_card : bool -> string -> build_card list -> state-> string

(*[print_role] prints the player's actor role*)
val print_role : actor ->  unit

(*[print_building] prints the build card's score/points*)
val print_building : build_card -> unit

(*[print_player] prints the player's name, gold, hand, buildings built
 * and points*)
val print_player : player -> unit

(* [print_chooser] prints out who's choosing at a given turn
 * during the picking phase.*)
val print_chooser : int -> bool -> unit

(*[print_job_list] prints a list of actors and their corresponding numbers.*)
val print_job_list : unit -> unit

(*The following functions display help on the jobs and the game in general. *)
val help : unit -> unit
val help_job : actor -> bool -> string -> state -> string
val help_job_warlord : bool -> string -> bool-> state -> string
val help_job_kbma : actor -> unit

(* Shows information about the target actor that the warlord chose.*)
val warlord_show_target : state -> actor -> unit

(* Error functions to display when the player chooses the incorrect
 * action or actor. *)
val error_actor : actor -> unit
val error_action : string -> unit

(* [display_hand] shows the hand of the current human player. *)
val display_hand : state -> unit

(* [display_score] shows the current standings of all the players in game.*)
val display_score : state -> unit

(* [round_end] shows the ending of the round.*)
val round_end : state -> unit

(* [choose_another_actor] is the error message for trying to choose an actor
 * that cannot be chosen during the picking phase. *)
val choose_another_actor : unit -> unit

(* [warlord_gold_err] and [warlord_card_err] is displayed when the warlord 
 * cannot destroy the building.
 * The other functions with warlord are also error functions. *)
val warlord_gold_err : unit -> unit
val warlord_card_err : unit -> unit
val warlord_cant_target_bishop : unit -> unit
val warlord_incorrect_actor : unit->unit
val warlord_actor_not_in_round: unit -> unit

(* [cant_build_card], [cant_build_gold] and [all_built]
 * are warnings that are shown when player cannot build a selected building. *)
val cant_build_gold : unit -> unit
val cant_build_card : unit -> unit
val all_built : unit -> unit

(* [gui_choose_action] and [gui_choose_build] prompt the user to make a choice
 * in the choosing/building phase, and returns it as a string to the caller.*)
val gui_choose_action : bool -> string -> state -> string
val gui_choose_build : bool -> string -> state ->  string

(* [ai_is_choosing] indicates that ai is making a choice.*)
val ai_is_choosing : int -> unit

(* [print_available_actors] shows the remaining actors in the current pick.*)
val print_available_actors :  actor list -> state-> string

(* Functions to notify the thief player that the targets are invalid.*)
val robbed_is_dead : unit -> unit
val robbed_yourself : unit -> unit
val thief_cant_choose_assassin : unit -> unit

(* Prints out the current game state in a pretty format for the player to see.*)
val print_state : int-> state -> unit

(* [actor_not_in_round] prints out that the actor is not in the current round.*)
val actor_not_in_round: unit -> unit

(*[invalid_json_file] prints out the warning for an invalid JSON.*)
val invalid_json_file: unit -> unit

(*[cant_find_file] prints out the warning for no JSON file found for deck.*)
val cant_find_file : unit -> unit

(*[display_final] prints out the winner and the final ranking.*)
val display_final : state -> unit

(*[final_points_list] returns a list of players with updated final points in 
 *order of first place to last.*)
val final_points_list : state -> player list

(*[not_enough_cards] returns message that there isn't enough cards in the deck 
 *for the game to be played and terminates the game.*)
val not_enough_cards : state -> unit

(*[face_up] displays the face-up cards that are not being played in the
 *given round. *)
val face_up : actor list -> unit

(*[actors_turn] displays which actor's turn it is currently.*)
val actors_turn : actor -> unit

(*[you_got_killed] lets the player know he/she has been killed.*)
val you_got_killed : actor -> unit 