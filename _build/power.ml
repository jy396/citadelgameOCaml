open State
open Gui
open Aihandler
open Ai
open Str


(*[rid_blank] gets rid of any blank spaces in a string *)
let rid_blank = Str.global_replace (Str.regexp " ") ""

(*Allows the thief to use its power and returns an updated state*)
(*NEED TO ADD GUI FUNCTIONS FOR THESE PRINT STATEMENTS!!!*)
let rec power_thief (s: state) (p : player) (ba : bot array) =
  let cmd =
    if (p.is_ai) then help_job p.role true (ai_power s ba p.name) s
    else help_job p.role false " " s
  in
    let robbed' = type_actor cmd in
    if robbed' = Nil then power_thief s p ba
    else if robbed' = s.killed then let () = robbed_is_dead () in power_thief s p ba
    else
      let () =
      (if (not(List.exists (fun x -> x.role= robbed') s.players))
        then
        actor_not_in_round ()
      else ())
      in
      match robbed' with
      | Thief -> robbed_yourself (); {s with robbed = robbed'}
      | Magician | King | Bishop | Merchant | Architect | Warlord -> {s with robbed = robbed'}
      | Assassin -> thief_cant_choose_assassin (); power_thief s p ba
      | _ -> error_actor p.role; power_thief s p ba

(*[power_assassin] returns an updated state in correspondence with
 *  *)
let rec power_assassin (s: state) (p : player) (ba : bot array) =
  let cmd =
    if (p.is_ai) then help_job p.role true (ai_power s ba p.name) s
    else help_job p.role false " " s
  in
  let killed' = type_actor cmd in
  if killed' = Nil then power_assassin s p ba
    else
      (let () =
        (if (not(List.exists (fun x -> x.role= killed') s.players))
           then actor_not_in_round ()
        else ())
      in
      match killed' with
      | Thief | King | Bishop | Merchant | Architect | Warlord -> {s with killed = killed'}
      |  _          -> error_actor p.role; power_assassin s p ba)

(*[power_magician] returns an updated state in correspondence after the magician
 * uses one of its powers. *)
let rec power_magician (s:state) (p : player) (ba : bot array) =
  let command =
    if (p.is_ai) then help_job p.role true (ai_power s ba p.name) s
    else help_job p.role false " " s
  in
        match command with
       | "gold" -> let updated_magician = {p with gold = p.gold + 4} in
                    {s with players = (replace s updated_magician)}
       | "draw" -> let tup = n_random_cards s.deck 4 in
                   let hand' = fst(tup) in
                   let deck'= snd(tup) in
                  let updated_magician = {p with hand = hand'@p.hand} in
                  {s with players = (replace s updated_magician); deck = deck'}
       | _ -> error_actor p.role; power_magician s p ba

(*Allows the king to use its power and returns an updated state*)
let power_king (s: state) (p:player)=
  help_job_kbma p.role;
  let updated_king = {p with gold = p.gold + gold_inc p } in
  {s with players = (replace s updated_king)}

(*Allows the merchant to use its power and returns an updated state*)
let power_merchant (s: state) (p:player) =
  help_job_kbma p.role;
  let updated_merchant = {p with gold = p.gold + gold_inc p + 1 } in
  {s with players = (replace s updated_merchant)}

(*Allows the bishop to use its power and returns and updated state*)
let power_bishop s p=
  help_job_kbma p.role;
  let updated_bishop = {p with gold = p.gold + gold_inc p} in
  {s with players = (replace s updated_bishop)}

let rec is_in_build (building :build_card list) (b : string) : bool =
  match building with
   | [] -> false
   | h :: t -> let name = String.lowercase_ascii (h.name) in
              if b = name then true else is_in_build t b

let rec warlord_correct_actor (s:state) (p:player) (cmd : string)=
    let victim= try type_actor cmd with _ -> let () = warlord_incorrect_actor () in warlord_correct_actor s p (command (s)) in
    if victim = Bishop then let () = warlord_cant_target_bishop () in warlord_correct_actor s p (command (s))
    else if not(List.exists (fun x -> x.role = victim ) s.players) then
          let() = warlord_actor_not_in_round () in
          warlord_correct_actor s p (command(s))
    else let () = warlord_show_target s victim in victim

let find_actor (a:actor) (s:state) : player=
  try (List.find(fun x -> x.role = a) (s.players))
  with Not_found -> failwith "actor not found in find_actor."

let find_building (n:string) (p:player) (is_built:bool): build_card=
  let bl = if (is_built) then p.buildings else p.hand in
  try (List.find(fun (x:build_card) -> (String.lowercase_ascii x.name) = n) bl)
  with Not_found -> failwith "Could not find building given the build_card name."


(*Allows the warlord to use its power and returns an updated state*)
let power_warlord (s : state) (p : player) (ba : bot array)=
  let updated_warlord = {p with gold = p.gold + gold_inc p } in
  let s = {s with players = (replace s updated_warlord)} in
  let ai_input =
    (if (p.is_ai)
      then Str.split (regexp "[ \t]+") (ai_power s ba p.name)
    else [" ";" "])
  in
  (*Allows warlord to destroy a building*)
  let cmd =
    if (p.is_ai)
    then
      let ai_fst = List.nth ai_input 0 in
      help_job_warlord true ai_fst true s
    else help_job_warlord false " " true s
  in
  if cmd = "none" then s
  else
    (
    let victim= warlord_correct_actor s p cmd in  (*checking if is correct actor*)
    let rec helper (s: state) (victim:actor) : state  =
      let building_name =
        if (p.is_ai)
        then
          let ai_snd = List.nth ai_input 1 in
          help_job_warlord true ai_snd false s
        else help_job_warlord false " " false s
      in
      if building_name  = "none" then s else
      let victim_record = find_actor victim s in
      if (is_in_build (victim_record.buildings) (building_name)) then
        (
          let building = find_building (building_name) victim_record true in
          if (p.gold >=building.cost-1)
            then
            (
              let victim_buildings =
              List.filter (fun (x:build_card) -> x<>building) victim_record.buildings in
              let victim_points = victim_record.points - (building.score) in
              let updated_victim = {victim_record with buildings = victim_buildings;
                                    points = victim_points} in
              let victim_plist  = replace s updated_victim in
              let s' = {s with players = victim_plist} in
              let updated_player = {p with gold = p.gold - (building.cost -1)} in
              let player_plist = replace s' updated_player in
              {s' with players = player_plist}
            )
          else
            let () = warlord_gold_err () in helper s victim
        )
        else let () = warlord_card_err () in helper s victim
      in helper s victim)




  (*[is_in_hand] output boolean that checks whether the command is in the players
   * hand when building a card*)
let is_in_hand (p:player) (b_name:string) : bool =
  let rec is_in_hand_helper b (hl: build_card list)=
    match hl with
    |[]->false
    |h::t-> if b = (String.lowercase_ascii h.name) then true else is_in_hand_helper b t
  in
  is_in_hand_helper b_name (p.hand)


let rec get_hand (hand:build_card list )  command =
  match hand with
   | [] -> failwith "not in hand"
   | h :: t -> if (String.lowercase_ascii h.name) = command then h else get_hand t command


let rec match_hand (hand:build_card list )  command =
  match hand with
   |  [] -> false
   | h :: t -> if command = (String.lowercase_ascii h.name) then true else match_hand t command

  (*Allows player to build one building*)
let rec build (p:player) (s:state) : state =
  let building_name =
    if (p.is_ai) then gui_choose_build true (ai_build s p.name) s
    else gui_choose_build false " " s

  in
  if building_name = "none" then s

  else if (not (is_in_hand p building_name)) then
    let() = cant_build_card () in build p s
  else
    (let building = get_hand (p.hand) building_name in
    if (p.gold >=building.cost)
      then
      (
        let updated_hand = List.filter (fun (x:build_card) -> x<>building) p.hand in
        let updated_buildings = building::p.buildings in
        let updated_points = p.points + building.score in
        let updated_gold = p.gold - building.cost in
        let player =
        {p with hand = updated_hand; buildings = updated_buildings; points = updated_points; gold = updated_gold} in
        let plist  = replace s player in
        {s with players = plist}
      )

       else let () = cant_build_gold () in  build p s  )



let rec power_architect (s:state) (p:player) (ba : bot array) =
  help_job_kbma p.role;

  (*draw two random cards from the deck*)
  let two_rand_cards = n_random_cards (s.deck) 2 in
  let cards = fst two_rand_cards in
  let deck' = snd two_rand_cards in
  let hand' = p.hand @ cards in
  let player = { p with hand = hand' } in
  let plist = replace s player in
  let s' = {s with deck = deck'; players =plist} in

  (*builds a card*)
  build player s'

(**COMPILEEEEDD *****)
let power_player (p : player) (s: state) (ba: bot array)=
  let actor  = p.role in
  match actor with
   | Assassin  ->  power_assassin s p ba
   | Thief     ->  power_thief s p ba
   | Magician  ->  power_magician s p ba
   | King      ->  power_king s p
   | Bishop    ->  power_bishop s p
   | Merchant  ->  power_merchant s p
   | Architect ->  power_architect s p ba
   | Warlord   ->  power_warlord s p ba
   | Nil       ->  s
