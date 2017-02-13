open State

(* [personality] is a record that has each eight actors as the field name. All
 * fields are type int, and hold the integer values correlating to the
 * probability that the actor will be chosen *)
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

(* [bot] is a record that holds the ai's name (int), personality (personality),
 * actors_before (actor list), and actors_after (actor_list), which holds
 * data for each ai to be used in making decisions for game play *)
type bot =
{
  ai_name : int;
  personality : personality;
  actors_before : actor list;
  actors_after : actor list
}

(* [set_actors_before_after] returns a bot with actors_before and actors_after
 * fields as defined below:
 * actors_before is the list of actors chosen before player with name
 * [b].name's turn or are faced down, essentially the actors that are unknown
 * to the player.
 * actors_after is the list of actors that are available for the player to
 * choose. Since at this point the actor that the player chooses is uknown,
 * it is included in this list but will be later removed *)
val set_actors_before_after : state -> bot -> bot

(* [choose_from_available] returns the actor with the highest preference 
 * from the personality field of [b] given the available actors from [s].
 * This function ensures that only available actors are chosen *)
val choose_from_available : bot -> state -> actor

(* [choose_action] returns action choice "gold" or "draw" based on the amount
 * of gold the ai has. *)
val choose_action : int -> state -> string

(* [choose_build] returns build_card option that is None when no building is
 * chosen to be built and Some build_card otherwise. Building priority is given
 * to the respective role that receives a bonus for the color building, for
 * example when the ai is king, it will build a yellow building if it is one
 * of the options. The other factors considered are the most valuable
 * building able to be built, and colors that have not yet been built. *)
val choose_build : player -> player list -> build_card option

(* [assassin_target] returns the target actor to assassin, according to the data
 * stored in bot [b]. The ai [me] tries to target the players that are better
 * and worse as defined by the better_than and worse_than functions, while
 * ensuring that actors that are faceup and chosen by itself is not selected.
 * Weights are readjusted by weighting_by_rank and weighting_by_turn, and
 * finally the points difference between the players. The Ai should try to
 * target the player that is closer in terms of points *)
val assassin_target : bot ->  state ->  int  -> actor

(* [thief_target] returns the target actor to steal, according to the data
 * stored in bot [b]. The ai [me] tries to target the players that are better
 * and worse as defined by the better_than and worse_than functions, while
 * ensuring that actors that are faceup and chosen by itself is not selected.
 * If better and worse players are the assassin or have less gold than
 * the average player, it will not be considered as the target. The weights are
 * readjusted accordingly, similar to in assassin_target, using weight_by_turn.
 * Finally, since assassin cannot target killed, and actors like architect,
 * warlord are more likely to be chosen by players with a lot of gold, weights
 * are again readjusted *)
val thief_target : state -> int -> bot -> actor

(* [magician_power] returns power decision "draw" or "gold" to either get
 * 4 golds or draw 4 build_cards *)
val magician_power :int -> state -> string

(* [architect_power] returns building to build in build_card option form. None
 * if there is no desired building to be built, Some build_card otherwise*)
val architect_power :int -> state -> build_card option

(* [warlord_target] returns the player option to target. None means there is no
 * desired player to be targeted. Actor and player are synonymous at this point
 * since all of the roles of players have been revealed. Player option is
 * is returned for code effeciency. As always, the ai tries to target the
 * better and worse players, however if they are either Bishop or have no
 * buildings that ai can destroy, target moves to player with most buildings and
 * having a building that ai can destroy *)
val warlord_target : int -> state -> player option

(* [building_to_destroy] returns build_card option that is the target to be
 * destroyed, None if there is no building to be destroyed. This building is
 * the most expensive building that ai can destroy with the gold it has *)
val building_to_destroy : player option -> int -> state -> build_card option


(* [change_personality] returns new personality record according to following:
 * [change_personality] has two parts.
 * The first part is assessing self, where weights only change by their own 
 * situation.
 * The second part is assessing others, where weights change depending on the 
 * other players. *)

(* PART 1: Personal evaluation *)
(* Assassin, architect, warlord get extra weight if there's > 2 gold.
 * Smaller weight increase for warlord because there are multiple ways for a 
 * weight increase *)


(* Thief, merchant get extra weight if <= 2 gold*)

(* Architect, magician get extra weight if hand is smaller *)

(* King, Bishop, Warlord and merchant get extra weight if there is any buildings
 * of their royalty color *)

(* PART 2: Overall evaluation *)
(* Attack the player that has closer score to the current AI controlled player's 
* point.
 * Attack types are assassin, thief and warlord. Increases weight for them. *)

(* If the player is 1st place, increase stable job weights.
 * stable jobs are Assassin, King, Bishop, Warlord. *)
(* If player is last place, increase risky job weights.
 * risky jobs are Thief, Magician, Merchant, Architect. *)
val change_personality : int -> state -> personality -> personality

(*[choose_discard] returns build_card to discard based primarily on whether the
 * ai has the color of the build_card in hand or built. The next factor is then
 * which build_card has the higher cost. If the ai has less than 5 buildings
 * built, it choses the card with higher cost, but if it has more than or 
 * equal to 5 buildings, it chooses the card it can build, or is closer to
 * being able to build *)
val choose_discard : state -> int -> build_card list -> build_card