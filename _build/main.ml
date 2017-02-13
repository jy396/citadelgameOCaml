open Game

let rid_blank = Str.global_replace (Str.regexp " ") ""


let () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the Citadels Game engine.\n");
  print_endline "Please enter the number of players.\n";
  print_string  "> ";
(*   let num_players = read_line () in
  Game.main num_players *)
   let rec num_players_loop () =
    let num_players = read_line () in
    let num_players_int =
      (try (int_of_string(rid_blank(num_players))) with
      | Failure "int_of_string" ->0)
    in

    if (num_players_int <4) || (num_players_int > 7)
      then let () = print_endline "Please enter a number between 4 and 7." in num_players_loop ()
    else
      Game.main num_players_int
  in num_players_loop ()


