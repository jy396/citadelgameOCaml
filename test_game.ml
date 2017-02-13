open OUnit2
open Ai
open Aihandler
open Game
open Power
open State
open Yojson.Basic.Util





let card_json_to_record card_json=
  let card_number = card_json|>member "card_number"|>to_int in
  let name = card_json|>member "name"|>to_string in
  let color = card_json|>member "color"|>to_string in
  let score = card_json|>member "score"|>to_int in
  let cost = card_json|>member "cost"|>to_int in
  let description = card_json|>member "description"|>to_string in
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






let deck' = deck_json_to_card_list "deck.json"



let ba3 = ai_init 3
let ba4 = ai_init 4

let build0 =  List.nth deck' 0 (*green  *)
let build1 = List.nth deck' 1  (*  yellow *)
let build2 = List.nth deck' 2(*  yellow  *)
let build3 = List.nth deck' 3(* green *)
let build4= List.nth deck' 4(* green *)
let build5 = List.nth deck' 5(* red *)
let build6 = List.nth deck' 6(* red *)
let build7 = List.nth deck' 7(*  blue *)
let build8= List.nth deck' 8(*  blue *)
let build9= List.nth deck' 9(* blue  *)

let player0 = {
name =  0 ;
is_ai = false  ;
role = Assassin;
points =  0 ;
gold  =  0 ;
buildings =  [ ] ;
hand = [build0 ; build1] ;

}


let playernew = {
name =  0 ;
is_ai = true  ;
role = Assassin;
points =  0 ;
gold  =  0 ;
buildings =  [ ] ;
hand = [build0 ; build1] ;

}


let playerone = {
name =  1 ;
is_ai = true ;
role = King;
points = 1 ;
gold  =  2;
hand = [build9] ;
buildings =  [build2 ; build1] ;


}

let player1  = {
name =  1 ;
is_ai = false  ;
role =Thief;
points =  0 ;
gold  =  0 ;
hand = [build2 ; build3] ;
buildings =  [ ] ;

}

let player2 =  {
name =  2 ;
is_ai = false  ;
role = Magician;
points =  0 ;
gold  =  0 ;
buildings =  [build1 ] ;
hand = [build4 ; build5] ;
}

let player3 =
{
name =  3 ;
is_ai = false  ;
role = King ;
points =  0 ;
gold  =  0 ;
buildings =  [] ;
hand = [build6 ; build7 ]
}


let player4 =
{
name =  4 ;
is_ai = false  ;
role =Bishop;
points =  0 ;
gold  =  0 ;
buildings =  [build8] ;
hand = [build7 ; build9]
}


let player5 =
{
name =  4 ;
is_ai = false  ;
role = Merchant ;
points =  0 ;
gold  =  0 ;
buildings =  [build3] ;
hand = [build8 ; build9] ;
}


let player6 =
{
name =  4 ;
is_ai = false  ;
role = Architect  ;
points =  0 ;
gold  =  0 ;
buildings =  [] ;
hand = [build8 ; build9] ;
}

let player7 =
{
name =  4 ;
is_ai = false  ;
role = Warlord  ;
points =  0 ;
gold  =  0 ;
buildings =  [build6] ;
hand = [build8 ; build9] ;
}


let player_lst1 =  [ player0 ; player1; player2 ; playerone ]
let player_lst2 =  [player3 ; player4 ; player5 ; player6  ; player7 ]

let init = {

players = player_lst1 ;
deck = deck' ;
king = player1  ;
faceup = [];
facedown = [];
killed = Nil ;
robbed = Nil  ;
robber = Nil ;
available_actors = [] ;

}


let init2 = {

players = player_lst2 ;
deck = deck' ;
king = player1  ;
faceup = [];
facedown = [];
killed = Nil ;
robbed = Nil  ;
robber = Nil ;
available_actors = [] ;

}


let robbed_player = {
name = 7 ;
is_ai = false  ;
role = Assassin ;
points =  0 ;
gold  =  4 ;
buildings =  [build6] ;
hand = [build8 ; build9] ;

}

let robber_player = {
name = 8 ;
is_ai = false  ;
role = Thief ;
points =  0 ;
gold  =  0 ;
buildings =  [build6] ;
hand = [build8 ; build9] ;

}
let up_rob_player = {
name = 7 ;
is_ai = false  ;
role = Assassin ;
points =  0 ;
gold  =  0 ;
buildings =  [build6] ;
hand = [build8 ; build9] ;
}
let up_robber_player = {
name = 7 ;
is_ai = false  ;
role = Thief ;
points =  0 ;
gold  =  4 ;
buildings =  [build6] ;
hand = [build8 ; build9] ;
}
let init_roblist = [robbed_player; robber_player ; playernew]
let after_roblist = [up_rob_player ; up_robber_player  ; playernew]

let killed_state = {
players =init_roblist ;
deck = deck' ;
king = playernew  ;
faceup = [];
facedown = [];
killed = Assassin;
robbed = Assassin;
robber = Thief ;
available_actors = [] ;
}





let initrobass = {

players =init_roblist ;
deck = deck' ;
king = playernew  ;
faceup = [];
facedown = [];
killed = Nil ;
robbed = Assassin;
robber = Thief ;
available_actors = [] ;
}


let after_initroblist = {
players = after_roblist  ;
deck = deck' ;
king = playernew   ;
faceup = [];
facedown = [];
killed = Nil ;
robbed = Assassin ;
robber =Thief ;
available_actors = [] ;
}




(* For testing the power functionality of these functions, everything depends
 * on the user _ input there fore we need to make sure that that command is
 * especially written as followed to check the update state of the game
*)
(* let powerplayertest  = *)
(* [  "power_assassin" >:: (fun _ -> assert_equal assassinstate
   (power_assassin init player0 ba3 )) ;
   "power_thief" >:: (fun _ -> assert_equal thiefstate
   (power_thief init player1 ba3)) ;
   "power_magician" >:: (fun _ -> assert_equal magicianstate
   (power_magician init player2 ba3) ) ;
   "power_king" >:: (fun _ -> assert_equal kingstate
   (power_king init player3)) ;
   "power_bishop" >:: (fun _ -> assert_equal bishopstate
   (power_bishop init  player4)) ;
   "power_merchant" >:: (fun _ ->  assert_equal merchantstate
   (power_merchant init player5 )) ;
   "power_architect" >:: (fun _ -> assert_equal architectstate
   (power_architect init player6 ba4 )) ;
   "power_warlord" >:: (fun _ -> assert_equal warlordstate
   (power_warlord init player7  ba4 ))
 ]

 *)
let replace_test = [
"replace_test" >:: (fun _ ->
  assert_equal[playernew;player1;player2;playerone] (replace init playernew))

]

let gold_inc_test = [
"king">::(fun _ -> assert_equal 2
                   (playerone |> gold_inc  )) ;
"bishop" >::(fun _ -> assert_equal 1
                   (player4 |> gold_inc )) ;
"merchant " >::(fun _ -> assert_equal 1
                   (player5 |> gold_inc)) ;
"warlord"  >::(fun _ -> assert_equal 1
                   (player7 |> gold_inc ))
]
(*check whether the player is being listed baesd on the actor of the player
 let order_list (lst: player list)  (king:player)  =
   let before  = before_king lst king.name [] in
   let after = after_king lst king.name [] in

   after @ (List.rev before)
*)
 let order_list_test = [
"king_order_list " >:: (fun _ -> assert_equal [player4 ; player1 ; player2 ]
 (order_list [player1; player2; player4] player4)) ;
"king_order_repvious " >:: (fun _ -> assert_equal [player3 ; player4; player1 ]
 (order_list [player1; player3; player4] player3))

]

(*creating functions for getting the previous king of the state *)

(* let rec get_king lst (s : state)  =
  match lst with
   | [] -> s.king
   | h::t -> if (h.role = King) then h else get_king t s *)

let get_king_test = [
"getking_maintain" >:: ( fun  _ -> assert_equal playerone
  (get_king player_lst1 init));
"getking_change" >:: ( fun  _ -> assert_equal player3
 (get_king player_lst2 init2));

]
let is_in_build_test = [
"true" >::  ( fun  _ -> assert_equal  true (is_in_build [build1; build2; build3] "manor"));
"false" >:: ( fun  _ -> assert_equal  false (is_in_build [build5; build3; build4] "manor"));
"false" >:: ( fun  _ -> assert_equal  false (is_in_build [build5; build3; build4] "mAnOr")) ;
"false" >:: ( fun  _ -> assert_equal  false (is_in_build [build5; build3; build4] "Manor")) ;
]
let actor_to_string_test = [
"actor1" >::  ( fun  _ -> assert_equal  "Assassin" (actor_to_string Assassin));
"actor2" >::  ( fun  _ -> assert_equal  "Thief" (actor_to_string Thief));
"actor3" >::  ( fun  _ -> assert_equal  "Magician" (actor_to_string  Magician));
"actor4" >::  ( fun  _ -> assert_equal "King" (actor_to_string King ));
"actor5" >::  (fun  _ -> assert_equal  "Warlord" (actor_to_string Warlord));

]

let match_counter_test = [
"match1" >::  ( fun  _ -> assert_equal  1 (actor_to_int Assassin));
"match2" >::  ( fun  _ -> assert_equal  2 (actor_to_int Thief));
"match3" >::  ( fun  _ -> assert_equal  3 (actor_to_int Magician));
"match4" >::  ( fun  _ -> assert_equal  4 (actor_to_int King));
"match5" >::  ( fun  _ -> assert_equal  5 (actor_to_int Bishop));
"match6" >::  ( fun  _ -> assert_equal  6 (actor_to_int Merchant));
"match7" >::  ( fun  _ -> assert_equal  7 (actor_to_int Architect));
 "match8" >:: ( fun  _ -> assert_equal  8 (actor_to_int Warlord));
]
let actor_to_player = [
"actor_to_player" >::  ( fun  _ -> assert_equal  player0 (actor_to_player Assassin init));
"actor_to_player" >::  ( fun  _ -> assert_equal  player1 (actor_to_player Thief  init));
"actor_to_player" >::  ( fun  _ -> assert_equal  player2 (actor_to_player Magician init));
"actor_to_player" >::  ( fun  _ -> assert_equal  player3 (actor_to_player King init2));
"actor_to_player" >::  ( fun  _ -> assert_equal  player4 (actor_to_player Bishop init2));
"actor_to_player" >::  ( fun  _ -> assert_equal  player5 (actor_to_player Merchant init2)) ;
"actor_to_player" >::  ( fun  _ -> assert_equal  player6 (actor_to_player Architect init2));
"actor_to_player" >::  ( fun  _ -> assert_equal  player7 (actor_to_player Warlord init2));
]

let state_to_player_test = [

"state_to_playertwo" >:: (fun _ -> assert_equal player1
 (state_to_player init  player1));
"state_to_playerthree" >:: (fun _ -> assert_equal player2
(state_to_player init player2 )) ;
"state_to_playerthree" >:: (fun _ -> assert_equal player0
(state_to_player init player0 )) ;

]
(*Testing whether the is_robbed function is accurate and correct*)
let isrobbed_test = [
"is_robbed_Nil" >:: (fun _ -> assert_equal init
  (is_robbed init 1)) ;

 "is_robbed_pointdecrease" >:: (fun _ -> assert_equal init
  (is_robbed init 2))  ;
]

let killed_test =[
"killed_true" >:: (fun _ -> assert_equal true (killed killed_state 1)) ;
"killed_false" >::(fun _ -> assert_equal false ( killed killed_state 2)) ;
]



let counter_find_test = [
"counter_find" >:: (fun _ -> assert_equal player0 (counter_find 1 init));
"counter_findtwo" >:: (fun _ -> assert_equal player1 (counter_find 2 init));
]




let suite =
  "CitadelGame test suite" >::: replace_test  @ gold_inc_test
                                @ is_in_build_test @order_list_test
                                @ get_king_test @actor_to_string_test
                                @ match_counter_test @actor_to_player
                                @state_to_player_test @state_to_player_test
                                @ isrobbed_test @killed_test @counter_find_test



  (* @ gold_inc_test
                               @ order_list_test  @get_king_test *)

let _ = run_test_tt_main suite