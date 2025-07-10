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

let x_and_o_both_about_to_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 0; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 2 }, O);
    ]
(*
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

let is_pos_in_bounds (position : Position.t) (game : Game.t) =
  let board_length = Game_kind.board_length game.game_kind in
  position.row < board_length
  && position.column < board_length
  && position.row >= 0 && position.column >= 0

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.Set.t =
  let availble_moves_set = Position.Set.empty in
  Map.key_set game.board
  |> Set.fold ~init:availble_moves_set
       ~f:(fun availble_moves_cur pos_in_board ->
         List.fold Position.all_offsets ~init:availble_moves_cur
           ~f:(fun availble_moves_cur pos_func ->
             let possible_position = pos_func pos_in_board in
             match is_pos_in_bounds possible_position game with
             | true -> Set.add availble_moves_cur possible_position
             | false -> availble_moves_cur))
(* |> List.map ~f:(fun position_in_board ->
         let offset_positions_functions = Position.all_offsets in
         List.map offset_positions_functions ~f:(fun offset_position ->
             offset_position position_in_board)
         |> List.filter ~f:(fun pos ->
                is_pos_in_bounds pos game && not (Map.mem game.board pos))
         |> Position.Set.of_list)
  |> Position.Set.union_list |> Set.to_list *)

let%expect_test "availble moves win_for_x" =
  let moves_availble = available_moves win_for_x in
  print_s [%message (moves_availble : Position.Set.t)];
  [%expect {||}];
  return ()

let%expect_test "availble moves non_win" =
  let moves_availble = available_moves non_win in
  print_s [%message (moves_availble : Position.Set.t)];
  [%expect
    {|(moves_availble (((row 0) (column 1)) ((row 1) (column 1)) ((row 1) (column 2)) ((row 2) (column 1))))|}];
  return ()

let%expect_test "availble moves x_in_the_corners" =
  let moves_availble = available_moves x_in_the_corners in
  print_s [%message (moves_availble : Position.Set.t)];
  [%expect
    {|(moves_availble (((row 0) (column 1)) ((row 1) (column 1)) ((row 1) (column 2)) ((row 2) (column 1))))|}];
  return ()

let%expect_test "availble moves x_and_o_about_to_win" =
  let moves_availble = available_moves x_and_o_both_about_to_win in
  print_s [%message (moves_availble : Position.Set.t)];
  [%expect
    {|(moves_availble (((row 0) (column 1)) ((row 1) (column 1)) ((row 1) (column 1)) ((row 1) (column 2)) ((row 2) (column 1))))|}];
  return ()

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

(* Exercise 2 *)
let check_for_winner position peice (game : Game.t) =
  let winning_streak_length = Game_kind.win_length game.game_kind in

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

(*could use fold_until*)
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

let is_board_full (game : Game.t) = available_moves game |> Set.is_empty

let evaluate (game : Game.t) : Evaluation.t =
  let state_if_not_a_tie = evaluate_assuming_no_tie game in
  match (state_if_not_a_tie, is_board_full game) with
  | Evaluation.Game_continues, true -> Game_over { winner = None }
  | _, _ -> state_if_not_a_tie

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.Set.t =
  Set.filter (available_moves game) ~f:(fun pos -> check_for_winner pos me game)

(* let%expect_test "winning moves test one winning move right vertical for x" =
  let x_winning_moves = winning_moves ~me:X non_win in
  let o_winning_moves = winning_moves ~me:O non_win in
  print_s
    [%message
      (x_winning_moves : Position.t list) (o_winning_moves : Position.t list)];
  [%expect
    {|
    ((x_winning_moves (((row 1) (column 1)))) (o_winning_moves ()))
    |}];
  return () *)

(* let%expect_test "winning moves for x in the corners and o in the middle" =
  let x_winning_moves = winning_moves ~me:X x_in_the_corners in
  let o_winning_moves = winning_moves ~me:O x_in_the_corners in
  print_s
    [%message
      (x_winning_moves : Position.t list) (o_winning_moves : Position.t list)];
  [%expect
    {|
    ((x_winning_moves
      (((row 0) (column 1)) ((row 1) (column 0)) ((row 1) (column 2))
       ((row 2) (column 1))))
     (o_winning_moves ()))
    |}];
  return ()

let%expect_test "both x and o about to win seperatly" =
  let x_winning_moves = winning_moves ~me:X x_and_o_both_about_to_win in
  let o_winning_moves = winning_moves ~me:O x_and_o_both_about_to_win in
  print_s
    [%message
      (x_winning_moves : Position.t list) (o_winning_moves : Position.t list)];
  [%expect
    {|
  ((x_winning_moves (((row 0) (column 1))))
   (o_winning_moves (((row 2) (column 1)))))
  |}];
  return () *)

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.Set.t =
  let blocking_moves = winning_moves ~me:(Piece.flip me) game in
  match Set.is_empty blocking_moves with
  | true -> blocking_moves
  | _ ->
      Set.filter (available_moves game) ~f:(fun possible_move ->
          not (Set.mem blocking_moves possible_move))

(* let%expect_test "losing move for non-win" =
  let x_losing_moves = losing_moves ~me:X non_win in
  let o_losing_moves = losing_moves ~me:O non_win in
  print_s
    [%message
      (x_losing_moves : Position.t list) (o_losing_moves : Position.t list)];
  [%expect
    {|
  ((x_losing_moves ())
   (o_losing_moves
    (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2))
     ((row 2) (column 1)))))
  |}];
  return () *)

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.Set.t)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.Set.t)];
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
       print_s [%sexp (winning_moves : Position.Set.t)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.Set.t)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* let moves_that_dont_immediatly_lose ~(me : Piece.t) game =
  let blocking_moves = winning_moves ~me:(Piece.flip me) game in
  match blocking_moves with [] -> available_moves game | _ -> blocking_moves *)

let rec minimax game depth ~you_play : float * Position.t =
  match (depth = 0, evaluate game) with
  | false, Game_continues ->
      let inital_value, comparison_function =
        match you_play with
        | Piece.X -> (Float.neg_infinity, Float.( > ))
        | O -> (Float.infinity, Float.( < ))
      in
      Set.fold
        ~init:(inital_value, { Position.row = 0; column = 0 })
        (available_moves game)
        ~f:(fun (cur_val, cur_pos) possible_position ->
          let temp_val, _ =
            minimax
              (Game.set_piece game possible_position you_play)
              (depth - 1) ~you_play:(Piece.flip you_play)
          in
          match comparison_function temp_val cur_val with
          | true -> (temp_val, possible_position)
          | false -> (cur_val, cur_pos))
  | _, Game_over { winner = Some player } -> (
      match player with
      | X -> (Float.infinity, { Position.row = 0; column = 0 })
      | O -> (Float.neg_infinity, { row = 0; column = 0 }))
  | _, _ -> (0., { row = 0; column = 0 })

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  (* let moves_that_win = winning_moves ~me:you_play game in
  match moves_that_win with
  | [] ->
      List.random_element_exn
        (moves_that_dont_immediatly_lose ~me:you_play game)
  | _ -> List.random_element_exn moves_that_win *)
  match game.game_kind with
  | Omok ->
      let _, move = minimax game 3 ~you_play in
      move
  | Tic_tac_toe ->
      let _, move = minimax game 9 ~you_play in
      move

let%expect_test "need to play middle to win/not lose" =
  let x_move = make_move ~game:non_win ~you_play:X in
  let o_move = make_move ~game:non_win ~you_play:O in
  print_s [%message (x_move : Position.t) (o_move : Position.t)];
  [%expect {| ((x_move ((row 1) (column 1))) (o_move ((row 1) (column 1)))) |}];
  return ()
