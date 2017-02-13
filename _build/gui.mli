(*gui.mli text based*)
open State
val command : state -> string
val choose_card : bool -> string -> build_card list -> state-> string
val print_role : actor ->  unit
val print_building : build_card -> unit
val print_player : player -> unit
val print_chooser : int -> bool -> unit
val print_init_state : unit -> unit
val find_actor : int -> player list -> int
val print_revealed : int -> state -> unit
val print_killed : int -> unit
val print_job_list : unit -> unit
val help : unit -> unit
val help_job : actor -> bool -> string -> state -> string
val help_job_warlord : bool -> string -> bool-> state -> string
val warlord_show_target : state -> actor -> unit
val help_job_kbma : actor -> unit
val error_actor : actor -> unit
val error_action : string -> unit
val display_hand : state -> unit
val gold_or_draw : unit -> unit
val cant_build : unit -> unit
val all_built : unit -> unit
val display_score : state -> unit
val round_end : state -> unit
val yes_no : unit -> unit
val bad_init : unit -> unit
val choose_another_actor : unit -> unit
val warlord_gold_err : unit -> unit
val warlord_card_err : unit -> unit
val cant_build_gold : unit -> unit
val cant_build_card : unit -> unit
val gui_choose_action : bool -> string -> state -> string
val gui_choose_build : bool -> string -> state ->  string
val ai_is_choosing : int -> unit
val print_available_actors :  actor list -> state-> string
val robbed_is_dead : unit -> unit
val robbed_yourself : unit -> unit
val thief_cant_choose_assassin : unit -> unit
val print_state : int-> state -> unit
val warlord_cant_target_bishop : unit -> unit
val warlord_incorrect_actor : unit->unit
val warlord_actor_not_in_round: unit -> unit
val actor_not_in_round: unit -> unit
val invalid_json_file: unit -> unit
val cant_find_file : unit -> unit
val done_with_a_round : unit -> unit
val display_final : state -> unit
val final_points_list : state -> player list
val not_enough_cards : state -> unit
val face_up : actor list -> unit
val announce_role_choosing : unit -> unit