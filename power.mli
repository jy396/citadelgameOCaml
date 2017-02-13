open State
open Gui
open Aihandler
open Ai
open Str

(*[power_player] returns an updated state based on the actor's role
 * the state depends on the the specific powers of the actor role of the player
 *)
val power_player : player -> state -> bot array -> state
(*[build] returns the updated state when the *)
val build : player -> state -> state
(*[is_in_build] checks whether the name of the building is within the building
 * card list. Returns true when string name is in build_card list *)
val is_in_build : build_card list -> string -> bool

(*[power_thief] returns the updated state of when the thief uses its powers*)
val power_thief : state-> player -> bot array -> state

(*[power_assassin]returns the updated state of when the assassin uses powers*)
val power_assassin : state -> player -> bot array -> state

(*[power_magician]returns the updated state of when the magician uses powers*)
val power_magician : state -> player -> bot array -> state

(*[power_king]returns the updated state of when the king uses  powers*)
val power_king : state -> player -> state

(*[power_merchant]returns the updated state of when the merchant uses  powers*)
val power_merchant : state -> player -> state

(*[power_bishop]returns the updated state of when the bishop uses  powers*)
val power_bishop : state -> player -> state

(*[power_architect]returns the updated state of when the architect uses powers*)
val power_architect : state -> player -> bot array -> state

(*[power_warlord]returns the updated state of when the warlord uses  powers*)
val power_warlord : state -> player -> bot array -> state