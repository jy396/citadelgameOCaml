open State
open Gui
open Aihandler
open Ai
open Str

val power_player : player -> state -> bot array -> state
val build : player -> state -> state
val is_in_build : build_card list -> string -> bool
val power_thief : state-> player -> bot array -> state
val power_assassin : state -> player -> bot array -> state
val power_magician : state -> player -> bot array -> state
val power_king : state -> player -> state
val power_merchant : state -> player -> state
val power_bishop : state -> player -> state
val power_architect : state -> player -> bot array -> state
val power_warlord : state -> player -> bot array -> state