open Format
open ANSITerminal
open State
open Unix

(* To make things delay for n seconds where n is float, use
 * Unix.sleepf n to delay it.*)
let delay = 2
let delay05 = 0.5
let delay1 = 1

let print_job_list x =
  ANSITerminal.(printf [black] "1. Assassin\n");
  ANSITerminal.(printf [black] "2. Thief\n");
  ANSITerminal.(printf [black] "3. Magician\n");
  ANSITerminal.(printf [yellow] "4. King\n");
  ANSITerminal.(printf [blue] "5. Bishop\n");
  ANSITerminal.(printf [green] "6. Merchant\n");
  ANSITerminal.(printf [black] "7. Architect\n");
  ANSITerminal.(printf [red] "8. Warlord\n")

let help x =
  print_endline ("\nThe game is divided into two phases- the pick phase and "
    ^"the round phase.");
  print_endline ("During the pick phase, the players pick the job that they"
    ^" want play during this round.");
  print_endline ("During the round phase, each job is revealed, from numbers"
    ^" 1 to 8.");
  print_endline "The job numbers are as follows.";
  let () = print_job_list () in
  print_endline ("To print out help with the job, type -help job, job being the"
    ^" job number.");
  print_endline "At each round, at your turn, you can do 3 things, in that order.";
  print_endline ("1. Choose whether to receive 2 gold, or draw 2 cards and"
    ^" choose one card to keep.");
  print_endline "2. Choose to use your power.";
  print_endline "3. Build a building, if you want to.";
  print_endline "The game ends when a player builds 6 buildings.";
  print_endline ("The score calculated in the end is the sum of the cost of"
    ^" all buildings you have built.");
  print_endline ("4 points are added to whoever builds 6 buildings first,"
  ^" and then 2 for the second person, 1 for the third.");
  print_endline ("If you own at least one building of each color-red,"
  ^" yellow, green and blue, you get 4 points.\n");
  print_endline ""

let print_role a =
    match a with
    | Nil -> print_endline "No current role!"
    | Assassin -> ANSITerminal.(printf [white] "Assassin")
    | Thief -> ANSITerminal.(printf [white] "Thief")
    | Magician -> ANSITerminal.(printf [white] "Magician")
    | King -> ANSITerminal.(printf [yellow] "King")
    | Bishop -> ANSITerminal.(printf [blue] "Bishop")
    | Merchant -> ANSITerminal.(printf [green] "Merchant")
    | Architect -> ANSITerminal.(printf [white] "Architect")
    | Warlord -> ANSITerminal.(printf [red] "Warlord")

let actors_turn a =
    match a with
    | Nil -> print_endline "No current role!"
    | Assassin -> ANSITerminal.(printf [Underlined;Bold]
      "\nAssassin's turn\n\n"); sleep delay1
    | Thief -> ANSITerminal.(printf [Underlined;Bold]
      "\nThief's turn\n\n"); sleep delay1
    | Magician -> ANSITerminal.(printf [Underlined;Bold]
      "\nMagician's turn\n\n"); sleep delay1
    | King -> ANSITerminal.(printf [Underlined;Bold;yellow]
      "\nKing's turn\n\n"); sleep delay1
    | Bishop -> ANSITerminal.(printf [Underlined;Bold;blue]
      "\nBishop's turn\n\n"); sleep delay1
    | Merchant -> ANSITerminal.(printf [Underlined;Bold;green]
      "\nMerchant's turn\n\n"); sleep delay1
    | Architect -> ANSITerminal.(printf [Underlined;Bold]
      "\nArchitect's turn\n\n"); sleep delay1
    | Warlord -> ANSITerminal.(printf [Underlined;Bold;red]
      "\nWarlord's turn\n\n"); sleep delay1

let actor_help (n:int) =
    let a = match_actors n in
    match (a) with
    | Assassin -> let () = ANSITerminal.(printf [white] "\nPOWER:") in
                  ANSITerminal.(printf [black] "Choose a ");
                  ANSITerminal.(printf [Bold] "role");
                  ANSITerminal.(printf [black] " you would like to assassinate.");
                  ANSITerminal.(printf [black]
                    "\nThe person who had that role cannot play this round.\n");
                  sleep delay1
    | Thief ->  let () = ANSITerminal.(printf [white] "\nPOWER:") in
                ANSITerminal.(printf [black] " Choose a ");
                ANSITerminal.(printf [Bold] "role");
                ANSITerminal.(printf [black]
                  " you would like to steal gold from.");
                ANSITerminal.(printf [black]
                  ("\nIf that role is revealed, at the start of that player's turn,"));
                ANSITerminal.(printf [black] "steal all of their gold.\n");
                sleep delay1

    | Magician ->let () = ANSITerminal.(printf [white] "\nPOWER:") in
                ANSITerminal.(printf [black] " Choose either to ");
                ANSITerminal.(printf [Bold] "receive 4 gold ");
                ANSITerminal.(printf [black] "or to ");
                ANSITerminal.(printf [Bold] "draw 4 cards.");
                ANSITerminal.(printf [black]
                  "\nYou cannot build any buildings this round.\n");
                sleep delay1

    | King -> let () = ANSITerminal.(printf [white] "\nPOWER:") in
              ANSITerminal.(printf [black] " You get 1 extra gold for each ");
              ANSITerminal.(printf [yellow] "yellow");
              ANSITerminal.(printf [black] " buildings you own.");
              ANSITerminal.(printf [black]
                "\nAfter this round, you start the pick phase.");
              ANSITerminal.(printf [black]
                "\nThis is true even if you are assassinated this round.\n");
              sleep delay1

    | Bishop -> let () = ANSITerminal.(printf [white] "\nPOWER:") in
              ANSITerminal.(printf [black] " You get 1 extra gold for each ");
              ANSITerminal.(printf [blue] "blue");
              ANSITerminal.(printf [black] " buildings you own.");
              ANSITerminal.(printf [black]
                "\nBuildings you own cannot be destroyed by the ");
              print_role Warlord;
              ANSITerminal.(printf [black] ".\n");
              sleep delay1

    | Merchant -> let () = ANSITerminal.(printf [white] "\nPOWER:") in
              ANSITerminal.(printf [black] " You get 1 extra gold for each ");
              ANSITerminal.(printf [green] "green");
              ANSITerminal.(printf [black] " buildings you own. ");
              ANSITerminal.(printf [black]
                "\nAfter you receive gold or choose a card at the start of");
              ANSITerminal.(printf [black]
                "your turn, you receive one extra gold.\n");
              sleep delay1

    | Architect -> let () = ANSITerminal.(printf [white] "\nPOWER:") in
              ANSITerminal.(printf [black] " You can build up to ");
              ANSITerminal.(printf [Bold] "two");
              ANSITerminal.(printf [black] " buildings this turn. ");
              ANSITerminal.(printf [black]
                "\nAfter you receive gold or choose a card at the start of");
              ANSITerminal.(printf [black]
                "\nyour turn, you receive two extra building cards.");
              ANSITerminal.(printf [black] "\nWe will ask you two times.\n");
              sleep delay1

    | Warlord -> let () = ANSITerminal.(printf [white] "\nPOWER:") in
              ANSITerminal.(printf [black] " You get a gold coin for any ");
              ANSITerminal.(printf [red] "red");
              ANSITerminal.(printf [black] " buildings you own. ");
              ANSITerminal.(printf [black]
                "\nYou can choose a player and choose a building that they own to");
              ANSITerminal.(printf [black]
                "destroy, and pay the gold of that building's cost minus one.");
              sleep delay1
    | _ -> print_endline "Invalid actor!"

let print_building (b: build_card) =
  match b.color with
  | "red" -> ANSITerminal.(printf [red] "%s : worth %d points \n" b.name b.score)
  | "green" -> ANSITerminal.(printf [green]
    "%s : worth %d points \n" b.name b.score)
  | "blue" -> ANSITerminal.(printf [blue]
    "%s : worth %d points \n" b.name b.score)
  | "yellow" -> ANSITerminal.(printf [yellow]
    "%s : worth %d points \n" b.name b.score)
  | _ -> ANSITerminal.(printf [black] "%s : worth %d points \n" b.name b.score)

let display_hand (s: state) =
    let () = sleep delay1 in
    let p = (find_player 0 s) in
    ANSITerminal.(printf [black] "Your gold: %d\n" p.gold);
    ANSITerminal.(printf [black] "Your hand: \n");
    if List.length p.hand = 0
      then print_endline "You have no cards in your hand!"
    else
      List.fold_left (fun a x -> print_building x) () p.hand

let buildings_built (a: actor) (s : state) =
  let p =  List.find (fun x -> x.role = a) s.players  in
  ANSITerminal.(printf [black] "Buildings built by Player %d\n" p.name);
  List.fold_left (fun a x -> print_building x) () p.buildings

let display_buildings (s:state) =
  let l = s.players in
  List.fold_left (fun a x -> buildings_built x.role s) () l

let sort_players (pl:player list) =
  List.sort (fun a b -> compare (b.points) (a.points)) pl

let display_score (s: state) =
  let () = print_endline "Displaying current scores..." in
    List.fold_left
      (fun a x -> ANSITerminal.(printf [black]
        "Player %d : %d \n" x.name x.points))
      () (sort_players s.players)

let command (s:state) =
  let () = ANSITerminal.(printf [Bold] "> ") in
  let str = Pervasives.read_line() |> String.trim |> String.lowercase_ascii in
  match str with
  | "help" -> (help (); str)
  | "help 1" -> (actor_help 1; str)
  | "help 2" -> (actor_help 2; str)
  | "help 3" -> (actor_help 3; str)
  | "help 4" -> (actor_help 4; str)
  | "help 5" -> (actor_help 5; str)
  | "help 6" -> (actor_help 6; str)
  | "help 7" -> (actor_help 7; str)
  | "help 8" -> (actor_help 8; str)
  | "hand" ->  (display_hand s; str)
  | "buildings" -> (display_buildings s; str)
  | "score" -> (display_score s; str)
  | _ -> str

let print_player (p: player) =
  Format.printf "@[%s@ %d@]@." "Player" p.name;
  Format.printf "@[%s@ :@ %d@]@." "Gold" p.gold;
  Format.printf "@[%d@ %s@]@." (List.length p.hand) "cards in hand";
  Format.printf "@[%d@ %s@]@." (List.length p.buildings) "buildings built";
  Format.printf "@[%d@ %s@]@." p.points "points"

let rec building_in_color (b: build_card) =
  match b.color with
  | "red" -> ANSITerminal.(printf [red] "%d " b.score)
  | "green" ->ANSITerminal.(printf [green] "%d " b.score)
  | "blue" -> ANSITerminal.(printf [blue] "%d " b.score)
  | "yellow" -> ANSITerminal.(printf [yellow] "%d " b.score)
  | _ -> ANSITerminal.(printf [black] "%d " b.score)

let rec buildings_in_color (bl: build_card list) =
  match bl with
  |[]->()
  |h::t->building_in_color h; buildings_in_color t

let print_chooser (n: int) (b: bool) =
  if b then ANSITerminal.(printf [Bold] "Player %d is picking a role...\n" n)
  else ANSITerminal.(printf [Bold] "Your turn to pick!\n");
       ANSITerminal.(printf [Bold] "These are the roles that you can pick...\n")

let print_init_state x =
  print_endline "The picking phase has begun!\n";
  print_endline ("On the first phase, the first person to pick will be"
    ^" chosen randomly.\n");
  print_endline "Now, asking the players which roles they would like to pick...\n"

(*Returns the player based on an actor*)
let actor_to_player actor state =
  try (List.find (fun x -> x.role = actor) state.players) with
  | Not_found -> failwith"The actor you are looking for is not in the player list."

(*this n isn't the player name*)
let print_state (n: int) (s:state) =
  let () = sleep delay1 in
  let () =
    (if ((actor_to_player (match_actors n) s).is_ai)
      then ()
     else ANSITerminal.(printf [Bold] "YOUR TURN\n")) in
  let rec print_state_helper (pl: player list) =
    match pl with
    |[]->()
    |p::t->
       let () = ANSITerminal.(printf [black] "Player: %d, Actor: " p.name) in
        let () = if inv_match_actors p.role > n then ANSITerminal.(printf [black]
          "???")
        else print_role p.role in
        let () = ANSITerminal.(printf [black] ", Buildings: ") in
        let () = buildings_in_color p.buildings in
        let () = ANSITerminal.(printf [yellow] "Gold: ") in
        let () = ANSITerminal.(printf [black] "%d\n" p.gold)
        in print_state_helper t
  in
  print_state_helper (sort_players s.players)

let print_killed (n: int) =
  let () = print_role (match_actors n) in
  ANSITerminal.(printf [black] " has been killed!\n")

let robbed_is_dead () = print_endline "The actor you want to rob is dead!"

let thief_cant_choose_assassin () = print_endline "You cannot rob the assassin."

let robbed_yourself () = print_endline "You robbed yourself!"

(*unit n baesd on which actor you are at this point*)
let help_job (a: actor) (is_ai:bool) (ai_input:string) (s:state) =
  let () = print_endline "" in
  match a with
  | Assassin -> let () = actor_help 1 in
                if (is_ai)
                  then let () = sleep delay in print_endline ai_input;ai_input
                else
                  command s
  | Thief -> let () = actor_help 2 in
              if (is_ai)
                then let () = sleep delay in print_endline ai_input;ai_input
              else
                command s
  | Magician -> let () = actor_help 3 in
              if (is_ai)
                then let () = sleep delay in print_endline ai_input;ai_input
              else
                command s
  | Nil -> let () = print_endline "Invalid job number. Jobs are numbered 1 ~ 8."
                    in "failed"
  | _ -> let () = print_endline ("King, Bishop, Merchant,"
    ^" Architect should not be called here") in "failed"

let help_job_warlord (is_ai:bool) (ai_input:string) (is_first: bool) (s:state) =
  if (is_first)
    then
        (actor_help 8;
            ANSITerminal.(printf [black]
              "\nWhich actor would you like to destroy buildings from?\n");
            ANSITerminal.(printf [black]
              "\nIf you don't want to destroy, type \"none\".\n");
            if (is_ai)
              then let () = sleep delay in print_endline ai_input;ai_input
            else
              command s)
  else
      (ANSITerminal.(printf [black]
        "Which building would you like to destroy?\n");
      ANSITerminal.(printf [black]
        "If you don't want to or canont destroy any buildings, type \"none\"\n");
      if (is_ai)
        then let () = sleep delay in print_endline ai_input;ai_input
      else
        command s)

let warlord_show_target (s:state) (a:actor) =
  print_endline "These are the buildings you can choose to destroy...";
  buildings_built a s

let help_job_kbma (a:actor)=
  match a with
  | King -> actor_help 4
  | Bishop -> actor_help 5
  | Merchant -> actor_help 6
  | Architect -> actor_help 7
  | _ -> print_endline ("The actors that are not King , Bishop , Merchant,"
    ^" Architect should not be here.")

let error_actor (a:actor) =
   match a with
   | Assassin -> print_endline "You can't assassinate that target!"
   | Thief -> print_endline "You can't steal from that target!"
   | Magician | King | Bishop | Merchant ->
      print_endline "You can't do that action!"
   | Architect -> print_endline "You can't do that action!"
   | Warlord -> print_endline "You can't destroy that building!"
   | Nil -> failwith "Nil actor should not be an option"

let error_action (st:string) =
  match st with
  | "help" | "help 1" | "help 2" | "help 3" | "help 4" | "help 5" ->print_endline ""
  | "help 6" | "help 7" | "help 8" -> print_endline ""
  | "hand" | "buildings"  | "score"-> print_endline ""
  | _ -> print_endline (st^" is not a valid action.")


let cant_build () = print_endline ("You don't have that building in your hand "
 ^" or you don't have enough gold!")

let all_built () = print_endline "You can't build any more buildings this turn!"

let round_end (s: state) : unit =
  let () = ANSITerminal.(printf [Bold;white] "\nThe round has ended!\n\n") in
  display_score s

let choose_another_actor () =
  let () = print_endline "\nPlease choose among the roles shown as selection!\n" in
  sleepf delay05

let warlord_gold_err () =
  print_endline "You do not have enough gold to destroy that building!"

let warlord_card_err x =
  print_endline "That player does not have that building!"

let cant_build_gold () =
  print_endline ("You don't have enough gold to build that building! \n"
  ^"Choose another building or type none to not build any buildings.\n")

let cant_build_card () =
  print_endline "You don't have that card!"

let warlord_cant_target_bishop () = print_endline "You cannot target the Bishop."

let warlord_incorrect_actor () = print_endline "Cannot recognize the actor."

let warlord_actor_not_in_round () = print_endline ("The actor you chose is not"
  ^" in this round. Try again.")

let actor_not_in_round () = print_endline "MISSED TARGET! :P"

let invalid_json_file () = print_endline "This is not a valid json file!"

let cant_find_file () = print_endline "Can't find this file!"

let gui_choose_action (is_ai : bool) (ai_input:string) (s:state)=
  let () = sleep delay1 in
  print_endline ("\nACTION: Choose an action. If you want to draw two pieces"
    ^" of gold, type \"gold\" or if you want to draw cards, type \"draw\".");
  if (is_ai) then let () = sleep delay1 in print_endline ai_input;ai_input
  else command s

let choose_card (is_ai : bool) (ai_input: string) (bclist: build_card list) (s:state) =
  print_endline ("\nThese are the cards you chose. Enter the name of the"
    ^" card you want to discard.");
  let c1 = List.nth bclist 0 in
  let c2 = List.nth bclist 1 in
  print_building c1;
  print_building c2;
  if(is_ai) then let () = sleep delay1 in print_endline ai_input;ai_input
  else command s

let gui_choose_build (is_ai : bool) (ai_input:string) (s:state) =
  let () = sleep delay1 in
  let () = print_endline "\nBUILD: Choose a building you would like to build." in
  let () = sleepf delay05 in
  if (is_ai)
    then let () = sleep delay in print_endline ai_input;ai_input
  else
    let () = print_endline "If you don't wan't to build, type \"none\"." in
    let () = display_hand s in
    command s

let ai_is_choosing (n:int) =
  ANSITerminal.(printf [black] "Player %d has chosen!\n\n" n)

let rec print_available_actors (l : actor list) (s:state)=
  let () = print_endline ("It is your turn to choose an actor. Here are the"
    ^" available actors :") in
  let () = List.fold_left (fun a x ->
            print_role x; ANSITerminal.(printf [black] "\n")) () l in
  command s

let final_points_list (s:state): player list =
  let six_players = List.filter (fun p-> (List.length p.buildings)= 6) s.players in
  let first_six = List.fold_left
    (fun a p->
      if (inv_match_actors p.role) < (inv_match_actors a.role) then p else a)
    (List.hd six_players) six_players
  in
  let rec final_points_list_helper (pl:player list) =
    match pl with
    |[]->[]
    |h::t->
      ((match (h=first_six, List.mem h six_players, is_four_colors h) with
       |(true,_,true)-> {h with points = h.points + 7}
       |(true,_,false)-> {h with points = h.points + 4}
       |(false,true,true)-> {h with points = h.points + 5}
       |(false,true,false)-> {h with points = h.points + 2}
       |(false,false,true)-> {h with points = h.points + 3}
       |(false,false,false)-> h)::(final_points_list_helper t))
  in
  let final_points_l = final_points_list_helper s.players in
  sort_players final_points_l

let display_final (s: state) =
  let final_points_l = final_points_list s in
  let () = ANSITerminal.(printf [Bold]
    "Congratulations Player %d! You WON~\n" (List.hd final_points_l).name) in
  let () = print_endline "Displaying final ranking..." in
    List.fold_left (fun a x -> ANSITerminal.(printf [black]
      "Player %d : %d \n" x.name x.points)) () final_points_l

let not_enough_cards (s:state) =
  let () = ANSITerminal.(printf [Bold]
    "The game will end becayse there are only %d cards left in the game.\n"
    (List.length s.deck)) in
  display_final s

let face_up (al:actor list) =
  let () = ANSITerminal.(printf [Bold;white] "\nThe round has begun!\n\n") in
  let () = print_endline "These are the cards that are placed face up:" in
  let () = List.fold_left (fun a x ->
            print_role x; ANSITerminal.(printf [black] "\n")) () al in
  print_endline ""

let you_got_killed (a:actor) = let () = print_role a in
                               ANSITerminal.(printf [Bold] " got killed!!!!!\n")
