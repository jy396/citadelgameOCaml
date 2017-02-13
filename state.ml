(* Invariant: Nil is a placeholder value that represents the absence of an 
 * assigned actor. A player should have that value only before the player 
 * chooses his/her role. *)
type actor  =
 Nil | Assassin | Thief | Magician | King | Bishop | Merchant
 | Architect | Warlord

(* The building's cost and score is divided so that there might be
 * extensions of buildings with different cost/score. For example,
 * we could have a rare building card that costs 6 to build but gives
 * 8 points in the end *)
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

(* The values faceup, facedown are initialized randomly on
 * the beginning of each round.*)
type state = {
players : player list ; 
deck : build_card list  ;
king : player;
faceup :  actor list;
facedown : actor list  ;
killed : actor;
robbed : actor ; 
robber : actor;
available_actors : actor list ;
}

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
  | Nil -> failwith "no string"

(*[match_actors] matches int (0-8) with the corresponding actor type. *)
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
  | _ -> failwith "Invalid actor"

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

let is_four_colors (p:player) =
  let rec is_four_colors_helper fl pl =
    match fl with
    |[]->true
    |h::t -> (List.mem h pl) && (is_four_colors_helper t pl)
  in is_four_colors_helper ["yellow";"green";"blue";"red"]
       (List.map (fun x-> x.color) p.buildings)

let find_player num state=
  try (List.find(fun x -> x.name = num) (state.players))
  with Not_found -> failwith "player not found in find_player."

let type_actor (str:string) : actor =
  match str with
   | "assassin"  -> Assassin
   | "thief"     -> Thief
   | "king"      -> King
   | "bishop"    -> Bishop
   | "merchant"  -> Merchant
   | "architect" -> Architect
   | "warlord"   -> Warlord
   | "magician"  -> Magician
   | _          -> Nil

let gold_inc (p:player) : int =
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

let replace (state : state) (player : player) : (player list) =
  let name = player.name in
  let player_lst = List.filter (fun x -> x.name<>name) state.players in
  List.sort (fun a b-> compare (a.name) (b.name)) (player::player_lst)

(* [random_card] is used to pick a random card from a card deck.*)
let random_card (lst: 'a list) =
  (List.length lst) |> Random.int |> List.nth lst

let n_random_cards (lst: 'a list) (n:int) =
  let rec helper tup n=
    if n = 0 then tup
    else
    let lst = snd tup in
    let acc = fst tup in
    let rc = random_card lst in
    let lst' = List.filter(fun x -> x<> rc) lst in
    helper ((rc::acc), lst') (n-1)
  in
  helper ([],lst) n

let actor_to_player (a:actor) (s:state)  : player  =
  try (List.find (fun x -> x.role = a) s.players) with
  | Not_found -> failwith"The actor you are looking for is not in the player list."



