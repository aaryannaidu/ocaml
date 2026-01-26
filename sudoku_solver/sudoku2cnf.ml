type lit = int;;
type clause = lit list;;
type cnf = clause list;;

(* Mutable references for grid size and block size *)
let size = ref 9;;
let block_size = ref 3;;

let encoder i j v = i * !size * !size + j * !size + v;;

(* constraints 
Each square must contain at least one digit (no square can be left blank)
Each square can contain at most one digit 
No digit can appear more than once in any row
Each digit appears at least once in each row
Each digit appears at least once in each column
No digit appears more than once in any column
The grid is partitioned into  n^2 (i.e., 9) non-intersecting blocks of nxn squares (3x3) squares.   In each of these blocks

Each digit appears in each block.
No digit appears more than once in a block 
given values are fixed *)

let min_one_value i j = 
  let clause = ref [] in
  for v = 1 to !size do
    clause := (encoder i j v) :: !clause;
  done;
  !clause;;

let max_one_value i j = 
  let clauses = ref [] in 
  for v1 = 1 to !size do
    for v2 = v1 + 1 to !size do
      clauses := [-(encoder i j v1); -(encoder i j v2)] :: !clauses;
    done;
  done;
  !clauses;;

let value_in_row i v = 
  let clause = ref [] in
  for j = 0 to !size - 1 do
    clause := (encoder i j v) :: !clause;
  done;
  !clause;;

let value_in_col j v = 
  let clause = ref [] in
  for i = 0 to !size - 1 do
    clause := (encoder i j v) :: !clause;
  done;
  !clause;;

let value_in_block i j v = 
  let clause = ref [] in
  let bs = !block_size in
  (* Find the top-left corner of the block containing (i,j) *)
  let block_row = (i / bs) * bs in
  let block_col = (j / bs) * bs in
  (* Iterate over all cells in this block *)
  for di = 0 to bs - 1 do
    for dj = 0 to bs - 1 do
      let row = block_row + di in
      let col = block_col + dj in
      clause := (encoder row col v) :: !clause;
    done;
  done;
  !clause;;

let no_dup_row i v = 
  let clauses = ref [] in 
  for j1 = 0 to !size - 1 do
    for j2 = j1 + 1 to !size - 1 do
      clauses := [-(encoder i j1 v); -(encoder i j2 v)] :: !clauses;
    done;
  done;
  !clauses;;

let no_dup_col j v = 
  let clauses = ref [] in 
  for i1 = 0 to !size - 1 do
    for i2 = i1 + 1 to !size - 1 do
      clauses := [-(encoder i1 j v); -(encoder i2 j v)] :: !clauses;
    done;
  done;
  !clauses;; 

let no_dup_block i j v = 
  let clauses = ref [] in 
  let bs = !block_size in
  (* Find the top-left corner of the block containing (i,j) *)
  let block_row = (i / bs) * bs in
  let block_col = (j / bs) * bs in
  (* Compare all pairs of cells in this block *)
  for di1 = 0 to bs - 1 do
    for dj1 = 0 to bs - 1 do
      for di2 = di1 to bs - 1 do
        let start_j = if di2 = di1 then dj1 + 1 else 0 in
        for dj2 = start_j to bs - 1 do
          let row1 = block_row + di1 in
          let col1 = block_col + dj1 in
          let row2 = block_row + di2 in
          let col2 = block_col + dj2 in
          clauses := [-(encoder row1 col1 v); -(encoder row2 col2 v)] :: !clauses;
        done;
      done;
    done;
  done;
  !clauses;;

(* For pre-filled values: creates a unit clause that fixes the value *)
let given_value i j v = 
  [encoder i j v];;


(* Helper function to convert character to value, returns -1 for empty cells *)
let char_to_value c = 
  match c with
  | '.' | '0' -> -1  (* Empty cell *)
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' | 'a' -> 10
  | 'B' | 'b' -> 11
  | 'C' | 'c' -> 12
  | 'D' | 'd' -> 13
  | 'E' | 'e' -> 14
  | 'F' | 'f' -> 15
  | _ -> -1;;

(* Read grid from file using tail recursion *)
let read_grid filename = 
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      if String.length line > 0 then
        read_lines (line :: acc)
      else
        read_lines acc
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines [];;

(* Detect grid size from first row *)
let detect_size grid_rows = 
  if List.length grid_rows = 0 then 9
  else String.length (List.hd grid_rows);;

let generate_cnf grid_rows = 
  let cnf = ref [] in
  (* 1. Each cell must have at least one value *)
  for i = 0 to !size - 1 do
    for j = 0 to !size - 1 do
      cnf := (min_one_value i j) :: !cnf;
    done;
  done;
  
  (* 2. Each cell can have at most one value *)
  for i = 0 to !size - 1 do
    for j = 0 to !size - 1 do
      cnf := !cnf @ (max_one_value i j);
    done;
  done;
  
  (* 3. Each value appears at least once in each row *)
  for i = 0 to !size - 1 do
    for v = 1 to !size do
      cnf := (value_in_row i v) :: !cnf;
    done;
  done;
  
  (* 4. No duplicate values in any row *)
  for i = 0 to !size - 1 do
    for v = 1 to !size do
      cnf := !cnf @ (no_dup_row i v);
    done;
  done;
  
  (* 5. Each value appears at least once in each column *)
  for j = 0 to !size - 1 do
    for v = 1 to !size do
      cnf := (value_in_col j v) :: !cnf;
    done;
  done;
  
  (* 6. No duplicate values in any column *)
  for j = 0 to !size - 1 do
    for v = 1 to !size do
      cnf := !cnf @ (no_dup_col j v);
    done;
  done;
  
  (* 7. Each value appears at least once in each block *)
  let bs = !block_size in
  for block_i = 0 to bs - 1 do
    for block_j = 0 to bs - 1 do
      let i = block_i * bs in
      let j = block_j * bs in
      for v = 1 to !size do
        cnf := (value_in_block i j v) :: !cnf;
      done;
    done;
  done;
  
  (* 8. No duplicate values in any block *)
  let bs = !block_size in
  for block_i = 0 to bs - 1 do
    for block_j = 0 to bs - 1 do
      let i = block_i * bs in
      let j = block_j * bs in
      for v = 1 to !size do
        cnf := !cnf @ (no_dup_block i j v);
      done;
    done;
  done;
  
  (* 9. Add given values as unit clauses *)
  for i = 0 to (List.length grid_rows) - 1 do
    let row = List.nth grid_rows i in
    for j = 0 to (String.length row) - 1 do
      let cell = String.get row j in
      let v = char_to_value cell in
      if v <> -1 then
        cnf := (given_value i j v) :: !cnf
    done;
  done;
  
  !cnf;;

(* ===== DIMACS OUTPUT ===== *)

let print_dimacs cnf = 
  let num_vars = !size * !size * !size in
  let num_clauses = List.length cnf in
  
  (* Print header *)
  Printf.printf "p cnf %d %d\n" num_vars num_clauses;
  
  (* Print each clause *)
  List.iter (fun clause ->
    List.iter (fun lit -> Printf.printf "%d " lit) clause;
    Printf.printf "0\n"
  ) cnf

(* ===== MAIN ===== *)

let () = 
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input_file>\n" Sys.argv.(0);
    exit 1
  end;
  
  let filename = Sys.argv.(1) in
  let grid_rows = read_grid filename in
  
  (* Detect grid size and set block size *)
  let detected_size = detect_size grid_rows in
  size := detected_size;
  block_size := int_of_float (sqrt (float_of_int detected_size));
  
  (* Generate CNF *)
  let cnf = generate_cnf grid_rows in
  print_dimacs cnf