open State
open Ai
open Yojson.Basic.Util


(*type actor to equivalent string*)
let actor_to_string (a:actor) =
  match a with
  |Nil ->"none"
  |Assassin->"assassin"
  |Thief->"thief"
  |Magician->"magician"
  |King->"king"
  |Bishop->"bishop"
  |Merchant->"merchant"
  |Architect->"architect"
  |Warlord->"warlord"

let bop_to_string (bop:build_card option) =
   match bop with
  |None ->"none"
  |Some x-> x.name

(**
 * [ai_init] initializes the ailist by taking in the number of ai players as the
 * input. the int refering to a specific ai player starts from 2 because 1
 * is allocated to the human player. the ai personality is initialized
 * by calling on the initializing functions in ai.ml *)
let ai_init num =
  let p_json_to_record p_json=
    let assassin = p_json|>member "assassin"|>to_int in
    let thief = p_json|>member "thief"|>to_int in
    let magician = p_json|>member "magician"|>to_int in
    let king = p_json|>member "king"|>to_int in
    let bishop = p_json|>member "bishop"|>to_int in
    let merchant = p_json|>member "merchant"|>to_int in
    let architect = p_json|>member "architect"|>to_int in
    let warlord = p_json|>member "warlord"|>to_int in
    {assassin = assassin;thief = thief; magician= magician;king = king;bishop=bishop;merchant=merchant;architect=architect;warlord=warlord}
  in
  let ai_init_helper pl n =
    {ai_name = n+1; personality = List.nth pl n; actors_before = []; actors_after = []}
  in
  let pj = Yojson.Basic.from_file "ai_presets.json" in
  let pl = List.map p_json_to_record (pj|>member "personalities"|>to_list) in
  Array.init num (ai_init_helper pl)

let ai_choose_actor (s:state) (ba: bot array) (n:int)=
  Array.set ba (n-1) (set_actors_before_after s (Array.get ba (n-1)));
  actor_to_string (choose_from_available (Array.get ba (n-1)) s)

(*[ai_action] returns the string that is fed into the function in game.ml
 * that decides whether the ai player wants to draw a card or take two gold *)
let ai_action (s:state) (n:int) =
  choose_action n s

(*[ai_build] returns the string that is fed into the function in game.ml that
 * decides which building the ai player wants to build *)
let ai_build (s:state) (n:int) =
  bop_to_string (choose_build (find_player n s) (s.players))

(*[ai_power] returns the string that is fed into the function in game.ml
 * that decides how the respective power will be executed. Note that it is
 * different per actor *)
let ai_power (s:state) (ba: bot array) (n:int) =
  let b = if n = 0 then Array.get ba n
          else Array.get ba (n-1)
 in
  match ((find_player n s).role) with
  |Assassin -> actor_to_string (assassin_target b s n (set_probability s b))
  |Thief -> actor_to_string (thief_target s n b)
  |Magician -> magician_action n s
  |Architect -> bop_to_string (architect_move n s)
  |Warlord -> let target_pop = warlord_target n s in

     let target_actor =
       (match target_pop with
        |None->Nil |Some x->x.role)
     in

     (actor_to_string target_actor)^" "^(bop_to_string (building_to_destroy target_pop n s))
  |Nil |King |Bishop |Merchant -> failwith "ai_power should not be called for this actor"

let ai_update (s: state) (ba: bot array) =
    for i = 0 to ((Array.length ba) - 1) do
    let b = Array.get ba i in
    Array.set ba i {ai_name=b.ai_name; personality= change_personality b.ai_name s b.personality;
      actors_before=[]; actors_after=[]}
    done

let ai_choose_card (s:state) (n:int) (cards:build_card list) =
  (choose_discard s n cards).name
