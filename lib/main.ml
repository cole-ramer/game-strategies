open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let x_in_the_corners =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 0; column = 2 }, X);
      ({ row = 2; column = 0 }, X);
      ({ row = 2; column = 2 }, X);
      ({ row = 1; column = 1 }, O);
    ]
(*
let x_and_o_both_about_to_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 0; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 2 }, O);
    ]

let x_and_o_competing_for_same_spot =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 2; column = 2 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 1; column = 2 }, O);
    ] *)

let print_game (game : Game.t) =
  let break_line =
    List.init
      (Game_kind.board_length game.game_kind
      + ((Game_kind.board_length game.game_kind - 1) * 3))
      ~f:(fun _ -> "-")
    |> String.concat
  in
  let row_placeholder_list =
    List.init (Game_kind.board_length game.game_kind) ~f:(fun i -> i)
  in
  let board_placeholder_list =
    List.map row_placeholder_list ~f:(fun _ -> row_placeholder_list)
  in
  let rows_of_printed_game_board =
    List.mapi board_placeholder_list ~f:(fun row_index row_list ->
        let row_with_piecies =
          List.mapi row_list ~f:(fun col_index _ ->
              match
                Map.find game.board
                  { Position.row = row_index; column = col_index }
              with
              | None -> " "
              | Some X -> "X"
              | Some O -> "O")
        in
        String.concat ~sep:" | " row_with_piecies)
  in
  let board_as_one_string =
    String.concat ~sep:("\n" ^ break_line ^ "\n") rows_of_printed_game_board
  in
  print_endline board_as_one_string

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  let list_of_nums_in_board_length =
    List.init (Game_kind.board_length game.game_kind) ~f:(fun i -> i)
  in
  let (all_possible_board_positions : Position.t list) =
    List.cartesian_product list_of_nums_in_board_length
      list_of_nums_in_board_length
    |> List.map ~f:(fun (row, column) -> { Position.row; column })
  in
  List.filter all_possible_board_positions ~f:(fun position ->
      not (Map.mem game.board position))

let%expect_test "availble moves" =
  print_endline
    (available_moves win_for_x
    |> List.map ~f:(fun position -> Position.to_string position)
    |> String.concat |> String.strip);
  print_endline
    (available_moves non_win
    |> List.map ~f:(fun position -> Position.to_string position)
    |> String.concat |> String.strip);
  [%expect
    {|((row 0) (column 1))((row 0) (column 2))((row 1) (column 1))((row 1) (column 2))((row 2) (column 1))|}];
  return ()

(* Exercise 2 *)
let check_for_winner position peice (game : Game.t) =
  let winning_streak_length = Game_kind.win_length game.game_kind in
  let rec check_direction (direction_function : Position.t -> Position.t)
      (position : Position.t) peice (game : Game.t) streak : int =
    match Map.find game.board (direction_function position) with
    | None -> streak
    | Some next_piece -> (
        match Piece.equal next_piece peice with
        | true ->
            check_direction direction_function
              (direction_function position)
              next_piece game (streak + 1)
        | false -> streak)
  in

  let dimension_functions =
    [
      (Position.left, Position.right);
      (Position.up, Position.down);
      (Position.down_left, Position.up_right);
      (Position.down_right, Position.up_left);
    ]
  in
  let are_dimensions_winner =
    List.map dimension_functions ~f:(fun (direction_fun1, direction_fun2) ->
        check_direction direction_fun1 position peice game 0
        + check_direction direction_fun2 position peice game 0
        + 1
        >= winning_streak_length)
  in
  List.exists are_dimensions_winner ~f:(fun is_dimension_winner ->
      is_dimension_winner)

let is_pos_in_bounds (position : Position.t) (game : Game.t) =
  let board_length = Game_kind.board_length game.game_kind in
  position.row < board_length && position.column < board_length

let evaluate_assuming_no_tie (game : Game.t) =
  Map.fold game.board ~init:Evaluation.Game_continues
    ~f:(fun ~key ~data game_state ->
      match game_state with
      | Game_continues -> (
          match is_pos_in_bounds key game with
          | true -> (
              match check_for_winner key data game with
              | true -> Game_over { winner = Some data }
              | false -> game_state)
          | false -> Illegal_move)
      | Illegal_move | Game_over _ -> game_state)

let is_board_full (game : Game.t) = available_moves game |> List.is_empty

let evaluate (game : Game.t) : Evaluation.t =
  let state_if_not_a_tie = evaluate_assuming_no_tie game in
  match (state_if_not_a_tie, is_board_full game) with
  | Evaluation.Game_continues, true -> Game_over { winner = None }
  | _, _ -> state_if_not_a_tie

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  List.filter (available_moves game) ~f:(fun pos ->
      check_for_winner pos me game)

let%expect_test "winning moves test one winning move right vertical for x" =
  let x_winning_moves = winning_moves ~me:X non_win in
  let o_winning_moves = winning_moves ~me:O non_win in
  print_s
    [%message
      (x_winning_moves : Position.t list) (o_winning_moves : Position.t list)];
  [%expect
    {|
    ((x_winning_moves (((row 1) (column 1)))) (o_winning_moves ()))
    |}];
  return ()

let%expect_test "winning moves for x in the corners and o in the middle" =
  let x_winning_moves = winning_moves ~me:X x_in_the_corners in
  let o_winning_moves = winning_moves ~me:O x_in_the_corners in
  print_s
    [%message
      (x_winning_moves : Position.t list) (o_winning_moves : Position.t list)];
  [%expect
    {|
    ((x_winning_moves (((row 0) (column 1)) ((row 1) (column 0)) ((row 1) (column 2)) ((row 2) (column 1)))) (o_winning_moves ()))
    |}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  match me with X -> winning_moves ~me:O game | O -> winning_moves ~me:X game

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate non_win in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  ignore game;
  ignore you_play;
  failwith "Implement me!"
