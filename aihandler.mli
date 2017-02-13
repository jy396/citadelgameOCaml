open State
open Ai
open Yojson.Basic.Util



(**
 * [ai_init] initializes the ailist by taking in the number of ai players as the
 * input. the int refering to a specific ai player starts from 2 because 1
 * is allocated to the human player. the ai personality is initialized
 * by calling on the initializing functions in ai.ml *)
val ai_init : int -> bot array

(* [ai_choose_actor] returns an actor that the ai chooses as a string,
 * whilst updating the bot array [ba] accordingly by setting the actors_before
 * and actors_after fields *)
val ai_choose_actor : state -> bot array -> int -> string

(*[ai_action] returns the string that is fed into the function in game.ml
 * that decides whether the ai player wants to draw a card or take two gold *)
val ai_action : state-> int -> string

(*[ai_build] returns the string that is fed into the function in game.ml that
 * decides which building the ai player wants to build *)
val ai_build : state -> int -> string

(*[ai_power] returns the string that is fed into the function in game.ml
 * that decides how the respective power will be executed. Note that it is
 * different per actor *)
val ai_power : state -> bot array -> int -> string

(*[ai_update] updates the bot array [ba] with new personality values for each
 * bot element by calling the change_personality function *)
val ai_update : state-> bot array  -> unit

(*[ai_choose_card] returns name of the build_card ai decides to discard *)
val ai_choose_card : state -> int -> build_card list -> string