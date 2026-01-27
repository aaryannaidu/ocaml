
(* Convert value to character for output *)
let value_to_char v = 
  if v >= 1 && v <= 9 then
    char_of_int (int_of_char '0' + v)
  else if v >= 10 && v <= 15 then
    char_of_int (int_of_char 'A' + (v - 10))
  else if v = 16 then
    '0'  
  else
    '?';;

(* Read Z3 output and extract positive literals *)
let read_solution filename = 
  let ic = open_in filename in
  let rec read_lines acc =
    match input_line ic with
    | exception End_of_file -> 
        close_in ic; 
        List.rev acc
    | line when String.length line >= 15 && String.sub line 0 15 = "s UNSATISFIABLE" ->
        close_in ic;
        []
    | line when String.length line > 2 && line.[0] = 'v' && line.[1] = ' ' ->
        let tokens = String.split_on_char ' ' (String.sub line 2 (String.length line - 2)) in
        let nums = List.filter_map (fun s ->
          match int_of_string_opt s with
          | Some n when n > 0 -> Some n
          | _ -> None
        ) tokens in
        read_lines (List.rev_append nums acc)
    | _ -> 
        read_lines acc
  in
  read_lines [];;

(* Decode variable and place in grid *)
let decode_and_place grid size var =
  let var_idx = var - 1 in
  let i = var_idx / (size * size) in
  let remainder = var_idx mod (size * size) in
  let j = remainder / size in
  let v = remainder mod size in
  let sudoku_value = v + 1 in
  if i >= 0 && i < size && j >= 0 && j < size && sudoku_value >= 1 && sudoku_value <= size then
    grid.(i).(j) <- sudoku_value;;

(* Build and print grid from solution *)
let solve_and_print solution =
  if List.length solution = 0 then begin
    Printf.printf "No solution\n";
    exit 0
  end;
  
  (* Infer size from max variable *)
  let max_var = List.fold_left max 0 solution in
  let size = 
    if max_var <= 729 then 9      
    else if max_var <= 4096 then 16 
    else 9 in
  
  (* Build grid *)
  let grid = Array.make_matrix size size 0 in
  List.iter (decode_and_place grid size) solution;
  
  (* Print grid *)
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      print_char (value_to_char grid.(i).(j))
    done;
    print_newline ()
  done;;


let () = 
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <sat_output_file>\n" Sys.argv.(0);
    exit 1
  end;
  
  let filename = Sys.argv.(1) in
  let solution = read_solution filename in
  solve_and_print solution;;
