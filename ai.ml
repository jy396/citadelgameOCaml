open State
(* The personality type denotes how much the AI player is likely to pick the corresponding
 * role when the turn to pick comes. *)
type personality =
{
  assassin :int;
  thief : int;
  magician : int;
  king : int;
  bishop : int;
  merchant : int;
  architect : int;
  warlord : int;
}


type bot =
{
  ai_name : int;
  personality : personality;
  actors_before : actor list;
  actors_after : actor list
}

(* [best_player] returns player with highest points in [l] *)
let best_player l =
  let first = List.hd l in
  List.fold_left (fun a x -> if x.points > a.points then x else a) first l

(* [worst_player] returns player with lowest points in [l] *)  
let worst_player l =
  let first = List.hd l in
  List.fold_left (fun a x -> if x.points < a.points then x else a) first l

(* [better_than] returns the player who is just better than the current player 
 * represented by n in terms of points. If there is no better player, 
 * choose the worst player currently playing. *)
  let better_than (s:state) (n:int) =
  let curr = (find_player n s).points in
  let newl = List.filter (fun x -> x.points > curr) s.players in
  if List.length newl = 0 
    then worst_player s.players
  else worst_player newl

(* [worse_than] returns the player who is just worse than the current player 
 * represented by n in terms of points. If there is no worse player, 
 * choose the best player currently playing. *)
  let worse_than (s:state) (n:int) =
  let curr = (find_player n s).points in
  let newl = List.filter (fun x -> x.points < curr) s.players in
  if List.length newl = 0 
    then best_player s.players
  else best_player newl

(* [royalty_buildings] returns a four-tuple associated with the number of 
 * the four royalty colored buildings the player [p] has.
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

(* [get_rank] returns n for nth place of player [p]*)
 let get_rank (s:state) (n:int) =
   let l = List.sort (fun x y -> compare y.points x.points) s.players in
     let rec rank_num l acc =
       match l with
       |[]->9
       |h::t-> (if (h.name=n) 
                  then acc
                else rank_num t acc)
   in rank_num l 1

(* [personality_to_list] returns an int list out of the given personality record
 * input. index 0 is the assassin, 1 is the thief, 2 is magician etc... 
 * in the order of turns defined *)
let personality_to_list (p: personality) =
  [p.assassin; p.thief; p.magician; p.king;
   p.bishop; p.merchant; p.architect; p.warlord]

(* [list_diff] returns list of elements that are in [a] but not in [b]
 * mathematically : a - b *)
let rec list_diff a b =
  match b with
  |[] -> a
  |h::t -> list_diff (List.filter (fun x-> (x<>h)) a) t

(* [set_actors_before_after] returns a bot with actors_before and actors_after
 * fields updated as described in the mli file *)
let set_actors_before_after (s:state) (b:bot)=
  let actorlist = List.map match_actors [1;2;3;4;5;6;7;8] in
  let actors_before' = 
    list_diff (list_diff actorlist s.faceup) s.available_actors in
  {b with actors_before = actors_before'; actors_after = s.available_actors}

(* [set_probability] returns a personality record with probabilities of each
 * actors as targets according to the data stored in state [s] and 
 * b [bot]. The higher the int in the personality record, the higher the chance
 * that the respective actor will be chosen as the target. The significance of
 * this function is that:
     1) the target actor should never be itself or the face up actor
 *   2) the target actor probability is proportional to number of players
        and inversely proportional to number of cards. For example, if there
        are two players left to choose after you, and you pass three actor
        cards to the next player, each actor card has a 2/3 chance of being
        chosen.
   840 is the LCD of 1 to 8 and is multiplied to keep the values integers *)
let set_probability (s:state) (b:bot) =
  let actorlist = List.map match_actors [1;2;3;4;5;6;7;8] in

  let actors_before = b.actors_before in
  let actors_after =
    List.filter (fun x->x<>(find_player b.ai_name s).role) b.actors_after in
  let turn_number = List.length (actors_before) - List.length (s.facedown) + 1 in
  let prob_assign h =
    if (List.mem h actors_before)
      then ((turn_number-1)*840)/(List.length actors_before)
    else if (List.mem h actors_after)
      then ((List.length s.players -turn_number)*840)/(List.length actors_after)
    else 0
  in
   {assassin=prob_assign (List.nth actorlist 0);
    thief=prob_assign (List.nth actorlist 1);
    magician=prob_assign (List.nth actorlist 2);
    king=prob_assign (List.nth actorlist 3);
    bishop=prob_assign (List.nth actorlist 4);
    merchant=prob_assign (List.nth actorlist 5);
    architect=prob_assign (List.nth actorlist 6);
    warlord=prob_assign (List.nth actorlist 7)
   }

(* [turn_number_a] returns the turn number that the player with name [n] 
 * drew actor card *)
let turn_number_a (s:state) n =
  let k = (s.king).name in
  if (k<=n)
    then n-k+1
  else
    n-k+1+(List.length s.players)

(*[nth_acc] returns accumulative sum in int list [l] from element 0 to nth 
 * element plus acc, which we input as 0 in practice *)
let rec nth_acc n l acc=
   match n with
   |0 -> acc+List.nth l 0
   |x -> nth_acc (x-1) l (acc+(List.nth l x))


(* [get_role] returns the actor with the highest preference from the personality
 * record. Precondition: [p] must have at least one non-zero field *)
let get_role (p: personality) =
  let pl = personality_to_list p in
    match (Random.int (List.fold_left (fun x y->  x + y) 0 pl)) with
    |a when (0<=a) && (a< (List.nth pl 0)) -> Assassin
    |a when ((List.nth pl 0)<=a) && (a<(nth_acc 1 pl 0)) -> Thief
    |a when ((nth_acc 1 pl 0)<=a) && (a<(nth_acc 2 pl 0)) -> Magician
    |a when ((nth_acc 2 pl 0)<=a) && (a<(nth_acc 3 pl 0)) -> King
    |a when ((nth_acc 3 pl 0)<=a) && (a<(nth_acc 4 pl 0)) -> Bishop
    |a when ((nth_acc 4 pl 0)<=a) && (a<(nth_acc 5 pl 0)) -> Merchant
    |a when ((nth_acc 5 pl 0)<=a) && (a<(nth_acc 6 pl 0)) -> Architect
    |a when ((nth_acc 6 pl 0)<=a) && (a<(nth_acc 7 pl 0)) -> Warlord
    |_->Nil


(* [choose_from_available] returns an actor from the available cards and 
 * personality *)
let choose_from_available (b:bot) (s:state) =
  let rec cfa_helper p av_actors =
    match av_actors with
    |[]-> p
    |h::t->
      (match h with
       |Assassin -> cfa_helper {p with assassin = 0} t
       |Thief -> cfa_helper {p with thief = 0} t
       |Magician -> cfa_helper {p with magician = 0} t
       |King -> cfa_helper {p with king = 0} t
       |Bishop -> cfa_helper {p with bishop = 0} t
       |Merchant -> cfa_helper {p with merchant = 0} t
       |Architect -> cfa_helper {p with architect = 0} t
       |Warlord-> cfa_helper {p with warlord = 0} t
       |Nil -> failwith "Should not be called on Nil")
  in
  let av_per = cfa_helper b.personality
    (list_diff (List.map match_actors [1;2;3;4;5;6;7;8]) s.available_actors)
  in
  get_role av_per


(* [check_gold] returns new personality with updated values dependent on how
 *  much gold [player] has*)
let check_gold player p =
  if player.gold > 2
    then {p with assassin = p.assassin + 2; architect = p.architect + 2;
                  warlord = p.warlord + 1}
  else {p with thief = p.thief + 2; merchant = p.merchant + 2}

(* [check_hand] returns new personality with updated values dependent on how
 *  many cards [player] has in hand*)
let check_hand player p =
  if List.length player.hand < 2
   then {p with architect = p.architect + 2; magician = p.magician + 2}
  else p

(* [check_royalty] returns new personality with updated values dependent on how
 *  many respective royalty cards [player] has*)
let check_royalty player p=
  let (a,b,c,d) = royalty_buildings player in
  {p with king = p.king + a*2;
        bishop = p.bishop + a*2;
        merchant = p.merchant + a*2;
        warlord = p.warlord + a*2}

(* [check_standings] returns new personality with updated values dependent on
 *  the standings of the player*)
let check_standings n s p =
  if (best_player s.players).name = n
   then {p with assassin = p.assassin + 5;
          king = p.king + 5;
          bishop = p.bishop + 5;
          warlord = p.warlord + 5}
  else if (worst_player s.players).name = n
   then {p with thief = p.thief + 5;
          magician = p.magician + 5;
          merchant = p.merchant + 5;
          architect = p.architect + 5}
  else p

(*[change_personality] returns new personality record that is updated *)
let change_personality (n: int)  (s: state) (p: personality): personality =
  let curr_player = find_player n s in
  p |> check_gold curr_player |> check_hand curr_player 
    |> check_royalty curr_player |> check_standings n s

(* [choose_action] returns action choice "gold" or "draw" *)
let choose_action (n: int)  (s: state) =
  let curr_player = find_player n s in
  if curr_player.gold < 2 then
    match Random.int 4 with
    | 0 -> "gold"
    | 1 -> "gold"
    | 2 -> "gold"
    | 3 -> "draw"
    | _ -> failwith "impossible"
  else if curr_player.gold > 4 then
    match Random.int 4 with
    | 0 -> "gold"
    | 1 -> "draw"
    | 2 -> "draw"
    | 3 -> "draw"
    | _ -> failwith "impossible"
  else
    match Random.int 2 with
    | 0 -> "gold"
    | 1 -> "draw"
    | _ -> failwith "impossible"

(* [get_best_build] returns the building that gives the highest score in [l] *)
let get_best_build l =
  if l = [] then None
  else let first = List.nth l 0 in
  Some (List.fold_left (fun a x -> if x.score > a.score then x else a) first l)

(* [no_color_yet] returns the building that player does not have colors for yet.
 *)
let rec no_color_yet l acc =
  match l with
  |[]->[]
  |h::t->
     if (fst h)=0
       then no_color_yet t ((snd h)@acc)
     else no_color_yet t acc


(*[cards_with_needed_colors] returns build_cards from [cards] that are of color 
 * that are not present in [bl]*)
let cards_with_needed_colors bl cards=
  let rec colors_not_there fl bl cards acc =
    match fl with
    |[]->acc
    |h::t ->
       if (List.mem h (List.map (fun x->x.color) bl))
         then (colors_not_there t bl cards acc)
       else
         colors_not_there t bl cards ((List.filter (fun x->x.color=h) cards)@acc)
  in colors_not_there ["yellow";"green";"blue";"red"] bl cards []

(*[choose_discard] returns build_card to discard*)
let choose_discard (s:state) (n:int) (cards:build_card list) =
  let p = find_player n s in
  let pcards = p.buildings @ p.hand in

  let card1= List.nth cards 0 in
  let card2= List.nth cards 1 in

  match (List.mem card1 (cards_with_needed_colors pcards cards),
    List.mem card2 (cards_with_needed_colors pcards cards)) with
  |(true,false)->card1
  |(false,true)->card2
  |_ ->
     (let bigcard =
       (if (card1.cost>=card2.cost)
          then card1
        else card2) in

     let smallcard =
       (if (card1.cost>=card2.cost)
          then card2
        else card1) in

     (if (List.length p.buildings<5)
        then bigcard
      else
        (if bigcard.cost<=p.gold
           then bigcard
         else smallcard)))

(* [is_opponent_five] returns true if any of the opponents have five or more 
 * buildings*)
let rec is_opponent_five plist =
  match plist with
  |[] -> false
  |h::t->if (List.length h.buildings>=5) then true else is_opponent_five t

(* [choose_build] returns build_card option that is None when no building is
 * chosen to be built and Some build_card otherwise *)
let choose_build (pl: player) (plist: player list): build_card option =

  let bds = List.filter (fun x -> x.cost <= pl.gold) pl.hand in
  let ybds = List.filter (fun x -> x.color = "yellow") bds in
  let gbds = List.filter (fun x -> x.color = "green") bds in
  let bbds = List.filter (fun x -> x.color = "blue") bds in
  let rbds = List.filter (fun x -> x.color = "red") bds in

  let choose_build_helper () =
    let (y,b,g,r) = royalty_buildings pl in
    let bds2 = no_color_yet [(y,ybds);(b,bbds);(g,gbds);(r,rbds)] [] in
    match bds2 with
    |[]->
       (match (get_best_build bds) with
       |None -> None
       |Some x ->
         if (x.score<=3)&&(not (is_opponent_five plist))
           then
             (match (Random.int 4) with
              |a when (0<=a) && (a< x.score) -> Some x
              |_ -> None)
           else
             Some x)
    |_::_->get_best_build bds2
  in
  match pl.role with
  | King -> if List.length ybds > 0 then get_best_build ybds
        else choose_build_helper ()
  | Bishop -> if List.length bbds > 0 then get_best_build bbds
        else choose_build_helper ()
  | Warlord -> if List.length rbds > 0 then get_best_build rbds
        else choose_build_helper ()
  | Merchant -> if List.length gbds > 0 then get_best_build gbds
        else choose_build_helper ()
  | _ -> choose_build_helper ()


(* [weighting_by_turn] returns new personality record that is updated based on
 * whether [oppturn] is before or after [myturn]. Since the ai tracks the actor
 * cards before and after it in [b], we need to make sure that the probability
 * of choosing the actors that the opponent player could have been choosing from
 * is significantly greater than those that were not available to the opponent.
 *)
let weighting_by_turn (b:bot) (p:personality) (myturn:int) (oppturn:int) =
  let rec wbt_helper p al =
    match al with
    |[]-> p
    |h::t->
      (match h with
       |Assassin -> wbt_helper {p with assassin = p.assassin * 2} t
       |Thief -> wbt_helper {p with thief = p.thief * 2} t
       |Magician -> wbt_helper {p with magician = p.magician * 2} t
       |King -> wbt_helper {p with king = p.king * 2} t
       |Bishop -> wbt_helper {p with bishop = p.bishop * 2} t
       |Merchant -> wbt_helper {p with merchant = p.merchant * 2} t
       |Architect -> wbt_helper {p with architect = p.architect * 2} t
       |Warlord-> wbt_helper {p with warlord = p.warlord * 2} t
       |Nil -> failwith "Should not be called on Nil")
  in
  if (myturn<=oppturn)
    then wbt_helper p b.actors_after
  else wbt_helper p b.actors_before


(* [weighting_by_rank] returns new personality record that is updated based on
 * whether player with name [n] is ranked in top half or bottom half. The actors
 * that the players in top half and bottom half would likely choose are
 * different and therefore weighted differently *)
let weighting_by_rank (s: state) (p:personality) (n:int) =
  let top_bottom =
    (float_of_int (get_rank s n)) /. (float_of_int (List.length s.players))
  in
  (*top half*)
  if (top_bottom<=0.5)
    then
      {p with assassin = 0;
       king = p.king * 2;
       bishop = p.bishop * 2;
       warlord = p.warlord * 2}
  (*bottom half*)
  else
    {p with assassin = 0;
     thief = p.thief * 2;
     magician = p.magician * 2;
     merchant = p.merchant * 2;
     architect = p.architect * 2}

(* [assassin_target] returns the target actor to assassin *)
let assassin_target (b:bot) (s:state) (me:int)=
  let myturn = turn_number_a s me in
  let worse = worse_than s me in
  let worse_diff = (abs ((find_player me s).points- worse.points))+1 in
  let better = better_than s me in
  let better_diff = (abs (better.points-(find_player me s).points))+1 in
  let pw1 = weighting_by_rank s (set_probability s b) worse.name in
  let pb1 = weighting_by_rank s (set_probability s b) better.name in
  let pw = weighting_by_turn b pw1 myturn (turn_number_a s worse.name) in
  let pb =
    weighting_by_turn b pb1 myturn (turn_number_a s better.name) in
  let pnew =
    {assassin = 0;
     thief = (pw.thief*better_diff + pb.thief*worse_diff)/2;
     magician = (pw.magician*better_diff + pb.magician*worse_diff)/2;
     king = (pw.king*better_diff + pb.king*worse_diff)/2;
     bishop = (pw.bishop*better_diff + pb.bishop*worse_diff)/2;
     merchant = (pw.merchant*better_diff + pb.merchant*worse_diff)/2;
     architect = (pw.architect*better_diff + pb.architect*worse_diff)/2;
     warlord = (pw.warlord*better_diff + pb.warlord*worse_diff)/2}
  in
  get_role pnew

(* [thief_target] returns the target actor to steal *)
let thief_target (s:state) (me:int) (b:bot) =
  let myturn = turn_number_a s me in
  let avg_gold =
    (List.fold_left (fun x y -> x+y.gold) 0 s.players)/(List.length s.players) in
  let worse = worse_than s me in
  let better = better_than s me in
  let prob = set_probability s b in
  let prob2 =
    (match ((worse.gold>=avg_gold)&&(worse.role<>Assassin),
       (better.gold>=avg_gold)&&(better.role<>Assassin)) with
    |(true,true)->
      weighting_by_turn b
        (weighting_by_turn b prob myturn (turn_number_a s worse.name))
          myturn (turn_number_a s better.name)
    |(true,false)->weighting_by_turn b prob myturn (turn_number_a s worse.name)
    |(false,true)->weighting_by_turn b prob myturn (turn_number_a s better.name)
    |(false,false)->prob)
  in

  let prob3 =
    (match s.killed with
    |Assassin -> failwith "assassin cannot be killed"
    |Thief -> failwith "function should not be called when thief is killed"
    |Magician ->
      {prob2 with assassin=0; magician=0; thief = 0;
       architect=prob2.architect*2; warlord = prob2.warlord*2}
    |King ->
      {prob2 with assassin=0; king=0;thief=0;
       architect=prob2.architect*2; warlord = prob2.warlord*2}
    |Bishop ->
      {prob2 with assassin=0; bishop=0;thief=0;
       architect=prob2.architect*2; warlord = prob2.warlord*2}
    |Merchant ->
      {prob2 with assassin=0; merchant=0;thief=0;
       architect=prob2.architect*2; warlord = prob2.warlord*2}
    |Architect->
      {prob2 with assassin=0; king=0;architect=0; warlord = prob2.warlord*2}
    |Warlord ->
      {prob2 with assassin=0; king=0;warlord=0; architect = prob2.architect*2}
    |Nil ->
      {prob2 with assassin=0; thief = 0;
        architect=prob2.architect*2; warlord = prob2.warlord*2})
  in
  get_role prob3

(* [magician_power] returns "draw" or "gold" by calling choose_action *)
let magician_power (n:int) (s:state) =
  choose_action n s

(*[possible_buildings] returns buildings that [p] can destry with the gold it
 * has in building list [l] *)
let possible_buildings p l =
  List.filter (fun x -> x.cost-1 <= p.gold) l

(*[can_destroy] true if there exists a building that p can destroy in building
 * list [bl] otherwise false *)
let rec can_destroy p bl =
  match bl with
  |[]->false
  |h::t -> if p.gold>=h.cost-1 then true else can_destroy p t

(* [most_buildings] returns player with most buildings excluding player [me] 
 * that I can destroy with gold I have. Else None*)
let rec most_buildings (me:player) players =
  let rec most_buildings_helper players (most:player)=
      match players with
      |[]-> most
      |h::t-> if
                List.length h.buildings>List.length most.buildings
                then most_buildings_helper t h
              else
                most_buildings_helper t most
    in
  if players = []
    then None
  else
    (
   let most_player = most_buildings_helper players (List.hd players) in

    if (can_destroy me most_player.buildings) && (most_player.role <> Bishop)
      then Some most_player
    else most_buildings me (List.filter (fun x-> x<> most_player) players)
    )

(* [warlord_target] returns the player option to target. None means there is no
 * desired player to be targeted. *)
let warlord_target (n: int) (s: state) : player option =
  let pl = find_player n s in
  let worse = worse_than s n in
  let worse_diff = (abs (pl.points- worse.points))+1 in
  let better = better_than s n in
  let better_diff = (abs (better.points-pl.points))+1 in

  match ((worse.role <> Bishop) && (can_destroy pl worse.buildings),
         (better.role <> Bishop) && (can_destroy pl better.buildings)) with
  |(true,false) -> Some worse
  |(false,true) -> Some better
  |(true,true) ->
     (match (is_four_colors worse, is_four_colors better) with
      |(true,false)->Some worse
      |(false,true)->Some better
      | _ ->
         (match (get_best_build (possible_buildings pl better.buildings),
            get_best_build (possible_buildings pl worse.buildings)) with
          |(None,None)->
             most_buildings pl (List.filter (fun x-> x<>pl) s.players)
          |(None, _)-> Some better
          |(_, None)-> Some worse
          |(Some w,Some b)->
             (match (Random.int ((b.score)*worse_diff+ (w.score)*better_diff)) with
              |x when (0<=x) && (x< (b.score)*worse_diff) -> Some better
              |_ -> Some worse)))
  |(false,false) -> most_buildings pl (List.filter (fun x-> x<>pl) s.players)

(* [building_to_destroy] returns build_card option that is the target to be
 * destroyed, None if there is no building to be destroyed. *)
let building_to_destroy (p : player option) (n: int) (s: state) =
  match p with
  |None -> None
  |Some pp ->
     (match pp with
      |w when w = worse_than s n->
         get_best_build (possible_buildings (find_player n s) w.buildings)
      |b when b = better_than s n->
         get_best_build (possible_buildings (find_player n s) b.buildings)
      |aa ->
         get_best_build (possible_buildings (find_player n s) aa.buildings)
      )
(* [architect_power] calls choose_build function to returns building to build 
 * in build_card option form. *)
let architect_power (n: int) (s: state) =
  choose_build (find_player n s) s.players