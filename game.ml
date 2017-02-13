open Yojson.Basic.Util
open Aihandler
open State
open Str
open Gui
open Ai
open Power

exception Noplayer


let update_hand (s:state) (p:player) : state =
  let two_rand_cards = n_random_cards (s.deck) 2 in
  let cards = fst two_rand_cards in
  let deck = snd two_rand_cards in
  let rec update_hand_helper (s:state) (p:player) : state =
    let cmd =
      (if (p.is_ai)
         then choose_card true (ai_choose_card s p.name cards) cards s
       else choose_card false " " cards s)
    in
      if (cmd = String.lowercase_ascii ((List.nth cards 0).name))
        then
          let deck' =  deck @ [List.nth cards 0] in
          let new_hand = (List.nth cards 1) :: p.hand in
          let updated_players = replace s {p with hand = new_hand} in
            {s with deck = deck'; players = updated_players}
       else if (cmd = String.lowercase_ascii ((List.nth cards 1).name))
         then
           let deck' =  deck @ [List.nth cards 1] in
           let new_hand = (List.nth cards 0)::p.hand in
           let updated_players = replace s {p with hand = new_hand}
         in
           {s with deck = deck'; players=updated_players}
         else
           (error_action cmd; update_hand_helper s p)
  in
    update_hand_helper s p


let state_to_player (s:state)  p=
  let rec helper s lst  =
    match lst with
    | [] -> p
    | h::t -> (if h.name = p.name then h
               else helper s t)
  in helper s s.players


let rec action (p : player) (s : state) (ba : bot array) : state =
  let cmd =
    if (p.is_ai)
      then gui_choose_action true (ai_action s p.name) s
    else gui_choose_action false " " s
  in
    match cmd with
     | "gold" -> let player =
        (if p.role = Merchant then {p with gold = p.gold + 3}
           else {p with gold = p.gold + 2}) in
         let player_lst = replace s player in
           {s with players = player_lst}
     | "draw" -> let s_1 = update_hand s p in
                 let p_1 = state_to_player s_1 p in
                 (if p_1.role = Merchant then
                   (let updated_merch = {p_1 with gold = p_1.gold+1} in
                      let plist = replace s_1 updated_merch in
                        {s_1 with players = plist})
                  else s_1)
     |   _   -> error_action cmd ; action p s ba

 let gold_inc (p:player) : int =
   let lst = p.buildings in
    let rec helper lst =
      match lst with
       | [] -> 0
       | h::t  ->
         (match p.role with
         | King     ->  if h.color = "yellow" then 1 + helper t else helper t
         | Bishop   ->  if h.color = "blue" then 1 + helper t else helper t
         | Merchant ->  if h.color = "green" then 1 + helper t else helper t
         | Warlord  ->  if h.color = "red" then 1 + helper t else helper t
         | _ -> failwith "not a valid actor in gold_inc")
     in helper lst


let init_player_list (num_players: int)(deck : build_card list) =
  let rec helper tup n name=
     if n = 0 then tup
       else
     let acc = fst tup in
     let deck = snd tup in
     let rtup = n_random_cards deck 3 in
     let hand = fst rtup in
     let deck' = snd rtup in
     if name = 0 then
       helper (({role = Nil; gold = 2; buildings = []; hand = hand ; name = name;
                 is_ai = false ; points = 0} :: acc), deck') (n-1) (name -1)
      else
       helper (({role = Nil; gold = 2; buildings = []; hand = hand ; name = name;
             is_ai = true ; points = 0} :: acc), deck') (n-1) (name -1)
  in helper ([], deck) num_players (num_players -1)

(* [card_json_to_record card_json] returns a card determined by the JSON object
 * [card_json] *)
let card_json_to_record card_json=
  let card_number = card_json|>member "card_number"|>to_int in
  let name = card_json|>member "name"|>to_string in
  let color = card_json|>member "color"|>to_string in
  let score = card_json|>member "score"|>to_int in
  let cost = card_json|>member "cost"|>to_int in
  let description = card_json|>member "description"|>to_string in
  {card_number;name;color;score;cost;description}


(* [deck_json_to_card_list file_name] is the deck which is a list of cards as
 * determined by the JSON object from the json file named [file_name] *)
let deck_json_to_card_list file_name =
  let emptyjson_object = `Assoc [("jooho", `List [])] in
  let d =
    (try (Yojson.Basic.from_file file_name) with
      |Yojson.Json_error _ ->
         let () = invalid_json_file () in  emptyjson_object
      |Sys_error _ ->let () = cant_find_file () in  emptyjson_object)
  in
    (match d with
    |`Assoc [("jooho", `List [])] ->[]
    |n -> List.map card_json_to_record (n|>member "cards"|>to_list))


let inv_type_actor c =
  match c with
   | Assassin  -> "assasin"
   | Thief     -> "thief"
   | King      -> "king"
   | Bishop    -> "bishop"
   | Merchant  -> "merchant"
   | Architect -> "architect"
   | Warlord   -> "warlord"
   | Magician  -> "magician"
   | Nil       -> ""

(*[before_king] inputs player_list king_idx and acc outputs reverse of list
 * before the king position *)
let rec before_king lst king_idx acc =
 match lst with
  | [] -> acc
  | h ::  t -> if h.name!=king_idx
                 then before_king t king_idx (h :: acc)
               else acc


(*[after_king] inputs playerlist and king_idx and acc and outputs the order
 * of players after the king outputs playerlist *)
 let rec after_king lst king_idx acc =
  match lst with
   | [] -> acc
   | h :: t -> if h.name =king_idx
                  then after_king t king_idx (h::acc@t)
               else after_king t king_idx acc


 let order_list (lst: player list) (king:player) : (player list) =
   let before  = before_king lst king.name [] in
   let after = after_king lst king.name [] in
   after @ (List.rev before)

let rec avail_actors (ordered:player list) (s:state) (ba: bot array) : state =
 match ordered  with
  | [] -> s
  | h :: t ->
    let cmd =
      (if (h.is_ai)
         then ((ai_is_choosing (h.name)); ai_choose_actor s ba h.name)
       else (print_available_actors s.available_actors s))in
         if (type_actor cmd) = Nil
           then
           (let () = choose_another_actor () in
            avail_actors ordered s ba)
         else
          (if (List.mem (type_actor cmd) s.available_actors)
             then
          (let leftoverslst =
            List.filter (fun x -> x<>(type_actor cmd)) s.available_actors in
           let player_list =
             replace s {h with role = type_actor cmd } in
           let newstate =
             {s with available_actors = leftoverslst; players = player_list} in
               avail_actors t newstate ba)
           else
            (let () = choose_another_actor () in
             avail_actors ordered s ba))

(*[create_leftovers] returns updated state with faceup , facedown and
 * available_actors initialized *)
let create_leftovers (s:state) (num_players_int : int ) : state =
  let actor_list =
    [Assassin;Thief;Magician;King;Bishop;Merchant;Architect;Warlord] in
  let facedown_tup  = (n_random_cards actor_list 1)  in
  let faceup_tup = (match num_players_int with
               | 4 -> (n_random_cards (snd facedown_tup) 2)
               | 5 -> (n_random_cards (snd facedown_tup) 1)
               | 6 -> (n_random_cards (snd facedown_tup) 0)
               | 7 -> (n_random_cards (snd facedown_tup) 0)
               |  _ -> failwith "invalid number of players") in
  let () = face_up (fst faceup_tup) in
  {s with faceup = fst faceup_tup; facedown = fst facedown_tup ;
   available_actors = snd faceup_tup }

(*[first_king] returns a state with an initialized  *)
let first_init (king : player) : state =
  {
  players  = []   ;
  deck     = []   ;
  king     = king ;
  faceup   = []   ;
  facedown = []   ;
  killed   = Nil  ;
  robbed   = Nil  ;
  robber   = Nil  ;
  available_actors = [] ;
  }

let init_state (num_players_int : int) (ba: bot array) : state =
  let deck = deck_json_to_card_list "deck.json" in
  let player_tup = init_player_list num_players_int deck in
  let king = List.nth (fst player_tup) (Random.int num_players_int) in
  let s' = create_leftovers (first_init king) num_players_int in
  let ordered = order_list (fst player_tup) king in
  let s'' = avail_actors ordered {s' with players = (fst player_tup)} ba in
  {s'' with deck = (snd player_tup) }

let rec get_king lst (s : state)  =
  match lst with
   | [] -> s.king
   | h::t -> if (h.role = King) then h else get_king t s


let is_robbed (s:state) (counter:int) : state=
  if (s.robbed = Nil)||(not(List.exists (fun x -> x.role=s.robbed) s.players))
    then s
  else
    let player =  actor_to_player (s.robbed) s in
    if player.role = (match_actors counter)
      then
       (let robber = actor_to_player (Thief) s in
        let robbed = actor_to_player (s.robbed) s in
        let players =
          replace s {robber with gold = (robber.gold + robbed.gold)} in
        let newstate = {s with players = players} in
        let players = replace newstate {robbed with gold = 0} in
          {s with players = players})
    else s


let killed (s:state) (counter:int) =
  let killed_counter = s.killed |> inv_match_actors in
  (counter = killed_counter)

let rec counter_find (counter:int) (s:state)  : player  =
  try (List.find (fun x -> x.role = (match_actors counter)) s.players)
  with Not_found -> counter_find (counter +1)  s

let update_state (s:state) (ba : bot array) : state  =
  let counter = 1 in
  let rec helper s counter =
    if counter = 9 then let () =  ai_update s ba in s
    else if (killed s counter) then let () = you_got_killed (match_actors counter) 
      in helper s (counter + 1)
    else if not(List.exists (fun x -> x.role = (match_actors counter)) s.players)
      then helper s (counter + 1)
    else
    (
      let is_robbed_state = is_robbed s counter in
      let player = counter_find counter is_robbed_state in
      let () = actors_turn (player.role) in
      let () = print_state counter is_robbed_state in
      let action_state = action player is_robbed_state ba in
      let player' = state_to_player action_state player in
      let power_state = power_player player' action_state ba in
      let player'' = state_to_player power_state player' in
      let build_state =
      if(player''.role = Magician) then
        power_state
      else
        build player'' power_state in
      helper build_state (counter+1)
    )
  in helper s counter

let rec game_loop s ba=
  let s' = update_state s ba in
  let () = round_end (s') in
  if (List.exists (fun p -> List.length (p.buildings)>=6) s'.players)
   then display_final s'
  else if (List.length s'.deck < (List.length s'.players)+5)
    then not_enough_cards s
  else
   (let newstate  = create_leftovers s' (List.length s'.players) in
    let newking    = get_king newstate.players s in
    let ordered    = order_list newstate.players newking in
    let s''        = avail_actors ordered newstate ba in
    let newerstate =
    {s'' with king = newking ;  killed = Nil ; robbed  = Nil ; robber = Nil} in
    game_loop newerstate ba)

 let main num_p =
   let () = Random.self_init() in 
   let bot_array = ai_init (num_p-1) in
   let s = init_state num_p bot_array in
    game_loop s bot_array
