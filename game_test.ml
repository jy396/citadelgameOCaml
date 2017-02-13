(*State and game logic = game.ml*)
open Yojson.Basic.Util
(* open Gui
 *)
exception Noplayer

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
(* current_player : int;
 *)
points_list: (int*int) list; (*player,points *)
players : player list ;
deck : build_card list  ;
king : player;
faceup :  actor list;
facedown : actor list  ;
killed : actor;
robbed : actor ; (*player number*)
robber : actor;
available_actors : actor list ;
}


(*returns a random card from the deck*)
let random_card deck =
  List.length deck |> Random.int |> List.nth deck

(*returns n random cards and an updated deck with length-n*)
let n_random_cards deck n=
  let rec helper tup n=
    if n = 0 then tup
    else
    let deck = snd tup in
    let acc = fst tup in
    let rc = random_card deck in
    let deck' = List.filter(fun x -> x<> rc) deck in
    helper ((rc::acc), deck') (n-1)
  in
  helper ([],deck) n

(* Helper functions for better_than and worse_than methods.*)
let best_player l =
  let first = List.nth l 0 in
  List.fold_left (fun a x -> if snd(x) > snd(a) then x else a) first l
  let worst_player l =
  let first = List.nth l 0 in
  List.fold_left (fun a x -> if snd(x) < snd(a) then x else a) first l

(* Finds the player who is just better than the current player represented by n.
 * If there is no better player, choose the worst player currently playing.
 * Returns int * int (player, points) *)
  let better_than (s:state) (n:int) =
  let curr = snd(List.nth s.points_list n) in
  let newl = List.filter (fun x -> snd(x) > curr) s.points_list in
  if List.length newl = 0 then worst_player s.points_list
  else worst_player newl

(* Finds the player who is just worse than the current player represented by n.
 * If there is no worse player, choose the best player currently playing.
* Returns int * int (player, points) *)
  let worse_than (s:state) (n:int) =
  let curr = snd(List.nth s.points_list n) in
  let newl = List.filter (fun x -> snd(x) < curr) s.points_list in
  if List.length newl = 0 then best_player s.points_list
  else best_player newl

(* Gives back a four-tuple associated with the number of the four royalty
 * colored buildings the player associated with [n] has.
 * The tuple is (yellow, blue, green, red). *)
let royalty_buildings (p:player) =
  let ycount = List.fold_left (fun a x -> if x.color = "yellow" then a+1 else a)
                0 p.buildings in
  let bcount = List.fold_left (fun a x -> if x.color = "blue" then a+1 else a)
                0 p.buildings in
  let gcount = List.fold_left (fun a x -> if x.color = "green" then a+1 else a)
                0 p.buildings in
  let rcount = List.fold_left (fun a x -> if x.color = "red" then a+1 else a)
                0 p.buildings in
  (ycount,bcount,gcount,rcount)

  (*get_ranklist returns the players in the order of first place to last place*)

 let get_ranklist (s:state) =
    let rl = List.sort (fun (x1,y1) (x2, y2) -> compare y2 y1) s.points_list in
    List.map (fun (a,b) -> b) rl


  (*get_rank returns n for nth place of player [p]*)
 let get_rank (s:state) (p:int) =
   let list = get_ranklist s in
    let rec rank_num list acc =
      match list with
      |[]->9
      |h::t-> if (h=p) then acc
              else rank_num list acc
    in rank_num list 1


(*[match_actors] matches int (0-8) with corresponding actor type*)
  let match_actors n =
  match n with
  | 0 -> Nil
  | 1 -> Assassin
  | 2 -> Thief
  | 3 -> Magician
  | 4 -> King
  | 5 -> Bishop
  | 6 -> Merchant
  | 7 -> Architect
  | 8 -> Warlord
  | _ -> failwith "Invalid actor match actors"

(*[inv_match_actors] matches actor type with corresponding int (0-8) as defined
 * in match_actors *)
  let inv_match_actors (a:actor) =
  match a with
  | Nil -> 0
  | Assassin -> 1
  | Thief -> 2
  | Magician -> 3
  | King -> 4
  | Bishop -> 5
  | Merchant -> 6
  | Architect -> 7
  | Warlord -> 8

(*Parses the command*)
let command () =
  Pervasives.read_line() |> String.trim |> String.lowercase_ascii

(*Returns an updated player list by replacing the player's old information
 * with new information. *)
let replace (state : state) (player : player) : (player list) =
  let name = player.name in
  let player_lst = List.filter (fun x -> x.name<>name) state.players in
  player::player_lst

(*Draws 2 cards and lets the player choose which to discard.
 *Returns an updated state*) (*the command is the card that I would like to add*)
let update_hand state player=
  let deck = state.deck in
  let two_rand_cards = n_random_cards deck 2 in
  let cards = fst two_rand_cards in
  let deck = snd two_rand_cards in
  let fcard_name = (List.nth cards 0).name in
  let scard_name = (List.nth cards 1).name in
  let rec helper state =
    let () = print_endline "These are the two cards you picked:" in
    let () = print_endline fcard_name in
    let () = print_endline scard_name in
    let () = print_endline "Which card would you like to discard?" in
    match command() with
    | x when (x=fcard_name) -> let ret_card = List.nth cards 0 in
                                 let deck' =  deck @  (ret_card::[]) in
                                 let new_hand = (List.nth cards 1) :: player.hand in
                                 let updated_players = replace state {player with hand = new_hand} in
                                 {state with deck = deck'; players=updated_players}

    | y when (y=scard_name) -> let ret_card = List.nth cards 1 in
                                 let deck' =  deck @  (ret_card::[]) in
                                 let new_hand = (List.nth cards 0) :: player.hand in
                                 let updated_players = replace state {player with hand = new_hand} in
                                 {state with deck = deck'; players=updated_players}
(*


    let return_card = List.nth cards 1 in
                                 let deck' = deck @ (return_card::[]) in
                                 let update_hand = (List.nth cards 0) :: player.hand in
                                 let update = replace state {player with hand = update_hand} in
                                 {state with deck = deck'; players=update} *)
    | _ -> print_endline "This is not a valid card name."; helper state
  in helper state

(* Carries out an action by either drawing two gold pieces or drawing two cards
  and discarding one of them.
changed the action command to string not a variant *)
let rec action (p : player) state  =
  print_endline "Choose an action. If you want to draw two pieces of gold, type \"gold\" or if you want to draw cards, type \"draw\".";
  let command = command() in
   match command with
    | "gold" ->
              let player =
                if p.role = Merchant then {p with gold = p.gold + 3}
                else {p with gold = p.gold + 3} in
              let player_lst = replace state player in
              {state with players = player_lst}
    | "draw" -> let s = update_hand state p in
                if p.role = Merchant then
                  (let updated_merch = {p with gold = p.gold+1} in
                  let plist = replace s updated_merch in
                  {s with players = plist})
                else s
    | _ -> let () = print_endline "Invalid action command. \n Please enter
           \"gold\" or \"draw\"" in action p state

let gold_inc p =
  let lst = p.buildings in
    let rec helper lst =
      match lst with
       | [] -> 0
       | h::t  -> match p.role with
                  | King     ->  if h.color = "yellow" then 1 + helper t else helper t
                  | Bishop   ->  if h.color = "blue" then 1 + helper t else helper t
                  | Merchant ->  if h.color = "green" then 1 + helper t else helper t
                  | Warlord  ->  if h.color = "red" then 1 + helper t else helper t
                  | _ -> failwith "not a valid actor in gold_inc"
     in helper lst

(*Returns player record given the player number*)
let find_player num state=
  try (List.find(fun x -> x.name = num) (state.players))
  with Not_found -> failwith "player not found in find_player."

(*Returns record given the actor*)
let find_actor actor state=
  try (List.find(fun x -> x.role = actor) (state.players))
  with Not_found -> failwith "actor not found in find_player."

 (***COMPILLLEEDD*****)
 let find_building (n:string) (player:player)=
  try (List.find(fun (x:build_card) -> (String.lowercase_ascii x.name) = n) (player.buildings))
  with Not_found -> failwith "Could not find building given the build_card name."

 (***COMPPIILLLEEEDDD*****)
 let find_hand (name: string) (player : player) =
  try (List.find(fun (x: build_card) -> (String.lowercase_ascii x.name)= x.name) (player.hand))
  with Not_found -> failwith "Could not find building given the build_card name."

(*change the string to actor type if its anything else change it to Nil and make sure that all of this done
* at this case *)
let type_actor c =
  match c with
   | "assassin"  -> Assassin
   | "thief"     -> Thief
   | "king"      -> King
   | "bishop"    -> Bishop
   | "merchant"  -> Merchant
   | "architect" -> Architect
   | "warlord"   -> Warlord
   | "magician"  -> Magician
   |  _        -> Nil

(*Allows the thief to use its power and returns an updated state*)
let rec power_thief (s: state) p =
  print_endline "Which actor would you like to rob?";

  let robbed' = type_actor (command ()) in
  if robbed' = s.killed then let () = print_endline "The actor you wannt to rob is dead" in power_thief s p
  else
    match robbed' with
    | Thief -> print_endline "You robbed yourself!"; {s with robbed = robbed'}
    | Magician | King | Bishop | Merchant | Architect | Warlord -> {s with robbed = robbed'}
    | Assassin -> print_endline "You cannot choose the Assassin. Choose again."; power_thief s p
    | _ -> print_endline "You did not enter a correct actor"; power_thief s p


(*[power_assassin] returns an updated state in correspondence with
 *  *)
let power_assassin (s: state) p =
  let rec get_command ()=
    if(p.is_ai) then () (*getter function for assassin*)
    else
      print_endline "Which actor would you like to kill?";
      command ()
  in
    let rec helper s =
      match get_command () with
       | "thief"     -> {s with killed = Thief }
       | "king"      -> {s with killed = King}
       | "bishop"    -> {s with killed = Bishop }
       | "merchant"  -> {s with killed = Merchant }
       | "architect" -> {s with killed = Architect }
       | "warlord"   -> {s with killed = Warlord }
       |  _          -> print_endline "You did not enter a valid character.\n
                        Please try again."; helper s in
      helper s

(*[power_magician] returns an updated state in correspondence after the magician
 * uses one of its powers. *)
let power_magician (s:state) p =
  let rec get_command () =
    if (p.is_ai) then ()(*getter function for magician*)
    else
      print_endline "Would you like to receive four gold or draw four cards?\n
                    You cannot build any district cards this round.";
      command()
  in
    let rec helper s =
        match get_command () with
       | "gold" -> let updated_magician = {p with gold = p.gold + 4} in
                    {s with players = (replace s updated_magician)}
       | "draw" -> let tup = n_random_cards s.deck 4 in
                   let hand' = fst(tup) in
                   let deck'= snd(tup) in
                  let updated_magician = {p with hand = hand'@p.hand} in
                  {s with players = (replace s updated_magician); deck = deck'}
       | _ -> if (p.is_ai) then helper s  (*fix!*)
              else
              let () = print_endline "Please type \"gold\" or \"draw\".
                                      Please try again." in
              helper s
    in helper s

(*Allows the king to use its power and returns an updated state*)
let power_king (s: state) (p:player)=
  let () =
    (if (p.is_ai) then ()
    else
      print_endline "You will receive a gold coin for any yellow building
      cards you have.";)
  in
  let updated_king = {p with gold = p.gold + gold_inc p } in
  {s with players = (replace s updated_king)}

(*Allows the merchant to use its power and returns an updated state*)
let power_merchant (s: state) (p:player)=
  let () =
    (if (p.is_ai) then ()
    else
      print_endline "You will get a gold coin for any green building
      cards you have and an an extra gold coin for an action you take.";)
  in
  let updated_merchant = {p with gold = p.gold + gold_inc p + 1 } in
  {s with players = (replace s updated_merchant)}

(*Allows the bishop to use its power and returns and updated state*)
let power_bishop s p=
  let () =
    (if (p.is_ai) then ()
    else
      print_endline "You will get a gold coin for any blue building
            cards you have.";) in
  let updated_bishop = {p with gold = p.gold + gold_inc p} in
  {s with players = (replace s updated_bishop)}

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


let rec print_hand (l : build_card list) =
  match l with
  | [] -> ()
  | h::t -> print_endline h.name; print_hand t


  (*Allows player to build one building*)
let build (p:player) (s:state) : state =
  print_endline "Choose a building to build.";
  let () = print_hand p.hand in
  let rec helper s =
    let building_name = command () in
    if building_name = "none"
      then s
    else if (not (is_in_hand p building_name))
      then (let () = print_endline "You do not have this card in your hand. Please pick another card." in helper s)
    else
      (let building = get_hand (p.hand) building_name in
       if (p.gold >=building.cost)
         then
           let updated_hand = List.filter (fun (x:build_card) -> x<>building) p.hand in
           let updated_buildings = building::p.buildings in
           let updated_points = p.points + building.score in
           let updated_gold = p.gold - building.cost in
           let player =
           {p with hand = updated_hand; buildings = updated_buildings; points = updated_points; gold = updated_gold} in
           let plist  = replace s player in
           {s with players = plist}
       else
         let () = print_endline "You do not have enough gold to build the
         building. \n Choose another building or type none to not build any buildings." in helper s)
  in helper s

let rec match_hand (hand:build_card list )  command =
  match hand with
   |  [] -> false
   | h :: t -> if command = (String.lowercase_ascii h.name) then true else match_hand t command


let power_architect (s:state) (p:player)=
  (*draw two random cards from the deck*)
  if (p.is_ai) then ()
  else print_endline "Two extra cards will be drawn from the deck and put in
                      your hand.";
  let two_rand_cards = n_random_cards (s.deck) 2 in
  let cards = fst two_rand_cards in
  let deck' = snd two_rand_cards in
  let hand = p.hand @ cards in
  let player = { p with hand = hand } in
  let plist = replace s player in
  let s = {s with deck = deck'; players =plist} in
  let rec helper s =
  (*builds up to two cards*)
    if (p.is_ai) then () (*call ai getter and store in cmd variable*)
    else
      print_endline "You may build up to one more disrict during your turn.";
      build p s
  in helper s

let rec print_l l =
  match l with
  | [] -> ()
  | h::t -> print_endline h; print_l t

let rec is_in_build (building :build_card list) (b : string) : bool =
  match building with
   | [] -> false
   | h :: t -> let name = String.lowercase_ascii (h.name) in
              if b = name then true else is_in_build t b

(*Allows the warlord to use its power and returns an updated state*)
let power_warlord (s: state) (p:player)=
  let () = print_endline "You will get a gold coin for any red building
  cards you have.\n" in
  let updated_warlord = {p with gold = p.gold + gold_inc p } in
  let s = {s with players = (replace s updated_warlord)} in
  let () = print_endline "You may destroy one building of your choice by paying
  a number of gold one less than the cost of the building. You may not destroy
  a building in a city that has already as eight buildings. You may choose
  to destroy your own building by your choice \n" in
  (*Allows warlord to destroy a building*)
   let () = print_endline "Which actor would you like to destroy?" in

   let cmd = command () in
   if cmd = "none" then s
   else (
    let victim= type_actor cmd in
    let rec helper s victim=
      let victim_record = (find_actor victim s) in

      print_endline "The player currently has the following buildings" ;
      print_l (List.map (fun (x:build_card) -> x.name ) victim_record.buildings);
      let () = print_endline "Which building would you like to destroy?" in
      let building_name = command () in
      if building_name = "none" then s else
      if (is_in_build (victim_record.buildings) (building_name)) then
      (
        let building = find_building (building_name) victim_record in
        if (p.gold >=building.cost-1)
         then
           (
           let victim_buildings =
           List.filter (fun (x:build_card) -> x<>building) victim_record.buildings in
           let victim_points = p.points - (building.score) in
           let updated_victim = {victim_record with buildings = victim_buildings;
                                points = victim_points} in
           let victim_plist  = replace s updated_victim in
           let s' = {s with players = victim_plist} in
           let updated_player = {p with gold = p.gold - (building.cost -1)} in
           let player_plist = replace s' updated_player in
           {s' with players = player_plist}
           )
        else
          let () = print_endline "You do not have enough gold to build the
                                 building. \n "
          in helper s victim
      )
      else let () = print_endline "The player does not have this card"
           in helper s victim
    in helper s victim)

(**COMPILEEEEDD *****)
let power_player (p : player) (s: state) =
  let actor  = p.role in
  match actor with
   | Assassin  ->  power_assassin s p
   | Thief     ->  power_thief s p
   | Magician  ->  power_magician s p
   | King      ->  power_king s p
   | Bishop    ->  power_bishop s p
   | Merchant  ->  power_merchant s p
   | Architect ->  power_architect s p
   | Warlord   ->  power_warlord s p
   | Nil       ->  s

(*update_playerlist inputs player list , counter ,state
 * outputs and updated_player list based on the player's actors*)
let update_playerlist (lst:player list) (counter:int)  state =
let actor = match_actors counter in
  let rec helper lst =
    match lst with
    | [] -> failwith "no actors exist"
    | h ::t -> if h.role = actor then power_player h state
               else helper t
in helper lst

(****NEW VERSION OF INIT_PLAYERLIST***)
let init_player_list num_players deck =
let name = num_players -1 in
let rec helper tup n name=
  if n = 0 then tup
  else
  let acc = fst tup in
  let deck = snd tup in
  let rtup = n_random_cards deck 3 in
  let hand = fst rtup in
  let deck' = snd rtup in
  helper (({role = Nil; gold = 0; buildings = []; hand = hand ; name = name;
           is_ai = false ; points = 0} :: acc), deck') (n-1) (name -1)
in helper ([], deck) num_players (num_players -1)


(*Source: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(*******COMPILES **********)
(* [card_json_to_record card_json] returns a card determined by the JSON object
 * [card_json] *)
let card_json_to_record card_json=
  let card_number = card_json|>member "card_number"|>to_int in
  let name = card_json|>member "name"|>to_string in
  let color = card_json|>member "color"|>to_string in
  let score = card_json|>member "score"|>to_int in
  let cost = card_json|>member "cost"|>to_int in
  let description = card_json|>member "description"|>to_string in
  let clist = explode name in
  let nospaces = List.filter (fun x -> x<> ' ') clist in
  let name = implode nospaces in
  {card_number;name;color;score;cost;description}

(*******COMPILES **********)
(* [deck_json_to_card_list file_name] is the deck which is a list of cards as
 * determined by the JSON object from the json file named [file_name] *)
let deck_json_to_card_list file_name =
  let emptyjson_object = `Assoc [("jooho", `List [])] in
  let d =
    try (Yojson.Basic.from_file file_name) with
      |Yojson.Json_error _ ->
         print_endline("This is not a valid json file!"); emptyjson_object
      |Sys_error _ ->print_endline("Can't find this file!"); emptyjson_object
  in
  match d with
  |`Assoc [("jooho", `List [])] ->[]
  |n -> List.map card_json_to_record (n|>member "cards"|>to_list)

(**COMPILES**)
(*change the actor type to actor string*)
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

(*COMPILES*)
(*[before_king] inputs player_list king_idx and acc outputs reverse of list
 * before the king position *)
let rec before_king lst king_idx acc =
 match lst with
 | [] -> acc
 | h ::  t -> if h.name!=king_idx then before_king t king_idx (h :: acc) else acc

(*COMPILES*)
(*[after_king] inputs playerlist and king_idx and acc and outputs the order
 * of players after the king outputs playerlist *)
 let rec after_king lst king_idx acc =
  match lst with
   | [] -> acc
   | h :: t -> if h.name =king_idx then after_king t king_idx (h::acc@t)
               else after_king t king_idx acc
(*COMPILES*)

(*[order_list] inputs the player_list and outputs the order of the player_list
 * that would choose the actor cards*)
 let order_list (lst: player list)  (king:player)  =
   let before  = before_king lst king.name [] in
   let after = after_king lst king.name [] in

   after @ (List.rev before)

let actor_to_string (a:actor) =
  match a with
  | Assassin -> "Assassin"
  | Thief -> "Thief"
  | Magician -> "Magician"
  | King -> "King"
  | Warlord -> "Warlord"
  | Architect -> "Architect"
  | Merchant -> "Merchant"
  | Bishop -> "Bishop"

let rec print_list l =
  match l with
  | [] -> ()
  | h::t -> print_endline (actor_to_string h); print_list t

let rec avail_actors (ordered:player list) (leftovers :actor list) acc : player list  =
    match ordered  with
     | [] -> acc
     | h :: t ->
                 print_endline "Here are the available actors:";
                 print_list leftovers;
                  let ()  =
                 print_endline "Which actor would you like to choose? " in
                 let cmd = (type_actor (command ()))  in
                 if cmd = Nil then
                 (print_endline "This is not a valid actor. Please choose another actor." ;
                 avail_actors ordered leftovers acc)
               else
                (*  avail_actors ordered leftovers acc *)
                 (* let cmd  = type_actor (command()) in *)
                 if (List.mem cmd leftovers) then
                 let leftoverslst = List.filter (fun x -> x<>cmd) leftovers in
                 avail_actors t leftoverslst ({h with role = cmd}::acc)
                 else let ()  = print_endline "Choose another actor" in
                  avail_actors ordered leftovers acc


(*function retruns state with faceup , facedown , available_actors initiated*)
let create_leftovers (s:state)  (num_players_int : int ) =
  let actor_list = [Assassin;Thief;Magician;King;Bishop;Merchant;Architect;Warlord] in
  let facedown_tup  = (n_random_cards actor_list 1)  in
  let faceup_tup = match num_players_int with
               | 4 -> (n_random_cards (snd facedown_tup) 2)
               | 5 -> (n_random_cards (snd facedown_tup) 1)
               | 6 -> (n_random_cards (snd facedown_tup) 0)
               | 7 -> (n_random_cards (snd facedown_tup) 0)
               |  _ -> failwith "invalid number of players" in
let newcard = n_random_cards (snd faceup_tup) 1 in
let newfaceup = if (List.mem King (fst faceup_tup)) then
                      List.filter (fun (x:actor) -> x <> King ) (fst faceup_tup) @
                      (fst newcard)
                    else fst faceup_tup in
let leftovers = if (List.mem King (fst faceup_tup)) then
                 (snd newcard) @ (King ::[])
               else snd faceup_tup in
               {s with  faceup =newfaceup ; facedown = fst facedown_tup ;
                available_actors = leftovers }


let first_init king =
  {
points_list = [];
players  = [] ;
deck  =[];
king  = king;
faceup =  [];
facedown  = [] ;
killed = Nil;
robbed =Nil ; (*player number*)
robber =  Nil;
available_actors = [] ;
  }


let init_state (num_players : string)=
  let num_players_int =
    (match (int_of_string num_players) with
    |x when (4<=x) && (x<= 7) -> x
    |_ -> let () = print_endline "Because you did not choose a
                                a number between \n
                                4 to 7 we would initialize
                                the game as if there are 4 numbers of players"
                              in 4)
  in
  let deck = deck_json_to_card_list "deck.json" in
  let player_tup = init_player_list num_players_int deck in
  let player_list = fst player_tup in
  let deck' = snd player_tup in
  let king = List.nth player_list (Random.int num_players_int) in
  let first = first_init king in
  let s' = create_leftovers first num_players_int in (*faceup facedown available_actors king*)
  let ordered = order_list player_list king in
  {s' with deck = deck' ; players = avail_actors ordered s'.available_actors []}


(***COMPILES**)
(*returns the player who is the king function *)
let rec get_king lst  =
  match lst with
   | [] -> failwith "no king"
   | h::t -> if (h.role = King) then h else get_king t

(*****COOOMMPPIILLLESSS *****)
let match_counter actor =
  match actor with
   | Nil -> 0
   | Assassin  -> 1
   | Thief     -> 2
   | Magician  -> 3
   | King      -> 4
   | Bishop    -> 5
   | Merchant  -> 6
   | Architect -> 7
   | Warlord   -> 8
   | _         -> failwith "Invalid actor match counter"


  (**COMPILES update inputs counter and outputs updated state FFFIIXEED**)
 let up_state counter state =
  let killed = command () |> type_actor  in
  let robbed = command () |> type_actor in
   match counter with
    | 1 ->  let newstate = update_playerlist state.players counter state in
            {newstate with killed = killed }
    | 2 -> let newstate = update_playerlist state.players counter state in
           {newstate with robbed = robbed}
    | 4 -> let newstate = update_playerlist state.players counter state in
           {newstate with king  = get_king state.players}
    | _ -> update_playerlist state.players counter state


  (****compiles but changed it to failwith statement for error types***)
(*Returns the player based on an actor*)
let actor_to_player actor state =
  try (List.find (fun x -> x.role = actor) state.players) with
  | Not_found -> failwith"The actor you are looking for is not in the player list."

(**COMPILEES**)
(*Returns a new state if a player has been robbed*)
let robbed state counter=
  if (state.robbed = Nil) then state
  else
    let player =  actor_to_player (state.robbed) state in
    if player.role = (match_actors counter) then
      (let robber = actor_to_player (Thief) state in
      let robbed = actor_to_player (state.robbed) state in
      let players =
      replace state {robber with gold = (robber.gold + robbed.gold)} in
      let state = {state with players = players} in
      let players = replace state {robbed with gold = 0} in
      {state with players = players})
    else state

(**COMPILES **)
(*Returns true if the current player has been killed*)
let killed state counter =
  let killed_counter = state.killed |> match_counter in
  (counter = killed_counter)



let rec yes_no_build state player =
  print_endline "Would you like to build?";
  match command() with
  | "yes" -> print_hand (player.hand) ; build player state
  | "no" -> state
  | _ -> let () = print_endline "Please enter a valid command
          \"yes\" or \"no\" " in
          yes_no_build state player

 let rec yes_no_power state player =
  match command() with
  | "yes" -> power_player player state
  | "no" -> state
  | _ -> let () = print_endline "Please enter a valid command
          \"yes\" or \"no\" " in yes_no_power state player


(**compiles **)
let rec build_or_power (state : state ) (player : player) =
  let () = print_endline "Would you like to build or use one of your powers?" in
  match command () with
   | "build" -> (let () = print_endline "Would you like to use one of
                                your powers?" in yes_no_power (build player state) player)
  | "power" -> (let () = print_endline "Would you like to build?" in
                       yes_no_build (power_player player state) player)
  | _ -> (let () = print_endline "Please enter a valid command
                                        \"build\" or \"power\" " in build_or_power state player)

(**compiles**)
let rec counter_find (counter:int) (state:state) =
  try (List.find (fun x -> x.role = (match_actors counter)) state.players)
with Not_found -> counter_find (counter +1)  state


(***compiles NEW VERSION**)
(* let update_state (state:state) =
  let counter = 1 in
  let rec helper state counter =
    if counter = 9 then state
    else if (killed state counter) then helper state (counter + 1)
    else if not(List.exists (fun x -> x.role = (match_actors counter)) state.players)
      then helper state (counter + 1)
    else
      let state = robbed state counter in
      let player = counter_find counter state in
      print_endline "\n";

      print_endline (actor_to_string player.role);
      print_endline "\n";
      print_endline (string_of_int counter);

      (*action function*)
      let () = print_hand player.hand in
      let state' = action player state in
      let () = print_endline (string_of_int counter) in
      let newplayer = List.nth state'.players counter in
      let () = print_hand (newplayer.hand) in
      let statetwo = power_player player state'  in
      let () = print_hand (player.hand) in
      let statethree = yes_no_build statetwo player in

      helper statethree (counter+1)
  in helper state counter *)


let update_state (state:state) =
  let counter = 1 in
  let rec helper state counter =
    if counter = 9 then state
    else if (killed state counter) then helper state (counter + 1)
    else if not(List.exists (fun x -> x.role = (match_actors counter)) state.players)
      then helper state (counter + 1)
    else
      let state = robbed state counter in
      let player = counter_find counter state in
      print_endline "\n";

      print_endline (actor_to_string player.role);
      print_endline "\n";
      print_endline (string_of_int counter);
      print_endline "\n" ;
      print_endline (string_of_int player.name);
      (*action function*)
      let state' = action player state in
      (*power function*)
      let onestate = power_player player state'  in
      let twostate = yes_no_build onestate player in
      helper twostate (counter+1)
  in helper state counter



let rec game_loop s =
  if List.fold_left (fun acc x -> (List.length (x.buildings) = 6)&&acc ) true s.players then
  (*call gui function to signal end of game!*) ()
  else
  let s' = update_state s in print_endline "DONE WITH A ROUNDDD\n";
  let newstate = create_leftovers s' (List.length s'.players) in  (**)
  let originalking = get_king s'.players in
  let newking = try (get_king s'.players) with | _-> originalking in
  (*only works if there is a previous king if there is no previous king then if there is no king in this
  * case how to fix most of hte problmes of the course   *)
  let ordered = order_list newstate.players newking in
  let newplayers = avail_actors ordered newstate.available_actors [] in
  let newstate = {newstate with king = newking ;  killed = Nil ; robbed  = Nil ; robber = Nil
  ; players = newplayers } in
  game_loop newstate


let main num_players =
  let s = init_state num_players in
  game_loop s

