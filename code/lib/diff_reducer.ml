open Expression
open Diff

let is_open_bracket = function
  | '{' | '[' | '(' -> true
  | _ -> false

(* Helper function to determine if a character is a closing bracket *)
let is_close_bracket = function
  | '}' | ']' | ')' -> true
  | _ -> false

(* Helper function to match opening and closing brackets *)
let is_matching open_bracket close_bracket =
  match (open_bracket, close_bracket) with
  | ('{', '}') | ('[', ']') | ('(', ')') -> true
  | _ -> false
    
let get_expression diff =
  let sp = String.concat ""  in
  let rev_before_context, rev_after_context = (List.rev diff.before_context), (List.rev diff.after_context) in
  let original_expression = sp (rev_before_context @ diff.original_lines @ rev_after_context) in
  let modified_expression = sp (rev_before_context @ diff.modified_lines @ rev_after_context) in
  create_expression
    ~file_name: diff.file_name
    ~line_no: diff.line_no
    ~before_context: ""
    ~after_context: ""
    ~original_expression
    ~modified_expression
    ()

(* Recursive helper function that alternates between two accumulators (buffers and lists) and two pointers *)
let rec parse_helper original_expression left right buf_left list_right is_left_turn =
  if left > right then (
    (* Base case: return the left buffer contents and the reversed right list as strings *)
    Buffer.contents buf_left, String.concat "" (List.rev list_right)
  )
  else
    let left_char = original_expression.[left] in
    let right_char = original_expression.[right] in

    (* Append to the correct buffer based on whether it's the left's turn or right's turn *)
    if is_left_turn && (left_char = '{' || left_char = '(' || left_char = '[') then (
      Buffer.add_char buf_left left_char;  (* Add context-relevant character to left buffer *)
      parse_helper original_expression (left + 1) right buf_left list_right false  (* Switch to right turn *)
    )
    else if not is_left_turn && (right_char = '}' || right_char = ')' || right_char = ']') then (
      let right_string = String.make 1 right_char in
      parse_helper original_expression left (right - 1) buf_left (right_string :: list_right) true  (* Prepend context-relevant character to right list *)
    )
    else (
      (* If the character is not part of the context, just move on *)
      if is_left_turn then
        parse_helper original_expression (left + 1) right buf_left list_right false
      else
        parse_helper original_expression left (right - 1) buf_left list_right true
    )

(* Main function that updates the expression object with before_context and after_context *)
let parse_max_expression (expression: expression) =
  let original_expression = expression.original_expression in  (* Extract the string content from the expression *)
  let length = String.length original_expression in
  let buf_left = Buffer.create (length / 2) in  (* Buffer for the left side *)
  let list_right = [] in  (* List for the right side *)
  
  (* Start the recursion with left turn being true *)
  let before_context, after_context = parse_helper original_expression 0 (length - 1) buf_left list_right true in
  
  (* Update the expression object with the new before_context and after_context *)
  { expression with before_context; after_context }

let parse_min_expression expression =
  expression

let reduce_diff_to_expression (diff: diff) = function
  | "max" -> parse_max_expression (get_expression diff) 
  | "min" -> parse_min_expression (get_expression diff)
  | _ -> failwith "Unknown Operation"
