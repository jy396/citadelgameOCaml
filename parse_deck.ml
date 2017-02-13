open Yojson.Basic.Util

type card = {
  card_number: int;
  name: string;
  color: string;
  score: int;
  cost: int;
  description: string;
  }

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
    try (Yojson.Basic.from_file file_name) with 
      |Yojson.Json_error _ -> 
         print_endline("This is not a valid json file!"); emptyjson_object
      |Sys_error _ ->print_endline("Can't find this file!"); emptyjson_object
  in
  match d with 
  |`Assoc [("jooho", `List [])] ->[]
  |n -> List.map card_json_to_record (n|>member "cards"|>to_list)