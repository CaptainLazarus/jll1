open Tree_sitter_bindings.Tree_sitter_output_t
open Printf
open Code

let extend_indent s =
  if String.length s mod 4 = 0 then
    s ^ "| "
  else
    s ^ "  "

let string_of_node_kind (kind : node_kind) =
  match kind with
  | Name s -> s
  | Literal s -> sprintf "%S" s
  | Error -> "!ERROR!"

let to_buf buf nodes =
  let rec print indent nodes =
    List.iter (print_node indent) nodes
  and print_node indent node =
    match node.children with
    | None | Some [] ->
      bprintf buf "%s%s\n" indent (string_of_node_kind node.kind);
    | Some children ->
      bprintf buf "%s%s:\n" indent (string_of_node_kind node.kind);
      print (extend_indent indent) children;
  in
  print "" nodes

let string_of_position pos =
  Printf.sprintf "(row: %d, column: %d)" pos.row pos.column

let rec drop n lst = 
  match lst with
  | [] -> ""  (* If the list is empty, return an empty list *)
  | _ :: tail when n > 0 -> drop (n - 1) tail  (* Skip the head and decrement n *)
  | l :: _ -> l  (* If n is zero or negative, return the remaining list *)

let get_identifier code node_kind start_pos end_pos =
  if (start_pos.row = end_pos.row && node_kind = "identifier") then
    let line = drop start_pos.row (String.split_on_char '\n' code) in
    let len = end_pos.column - start_pos.column in
    String.sub line start_pos.column len
  else ""

let to_buf_with_positions buf nodes code =
  let rec print indent nodes =
    List.iter (print_node indent) nodes
  and print_node indent node =
    let start_pos_str = string_of_position node.start_pos in
    let end_pos_str = string_of_position node.end_pos in
    match node.children with
    | None | Some [] ->
      bprintf buf "%s%s - %s \t[%s - %s]\n" indent (string_of_node_kind node.kind) (get_identifier code (string_of_node_kind node.kind) node.start_pos node.end_pos) start_pos_str end_pos_str
    | Some children ->
      bprintf buf "%s%s - %s [%s - %s]:\n" indent (string_of_node_kind node.kind) (get_identifier code (string_of_node_kind node.kind) node.start_pos node.end_pos) start_pos_str end_pos_str;
      print (extend_indent indent) children;
  in
  print "" nodes

let to_string code nodes =
  let buf = Buffer.create 1000 in
  to_buf_with_positions buf nodes code;
  Buffer.contents buf
 
(* let to_stdout nodes = *)
(*   print_string (to_string nodes); *)
(*   flush stdout *)

let parse_code code = function
  | C -> to_string code [(Tree_sitter_c.Parse.parse_source_string code).root]
  | Rust -> to_string code [(Tree_sitter_rust.Parse.parse_source_string code).root]
  | Cpp -> to_string code [(Tree_sitter_cpp.Parse.parse_source_string code).root]

let extract_code (before:string list) (lines:string list) (after: string list) : string =
  (String.concat "\n" ((List.rev before) @ lines @ (List.rev after)))

let get_lang (filename : string) : language =
  match List.rev (String.split_on_char '.' filename) with
  | "rs" :: _ -> Rust
  | "cpp" :: _ -> Cpp
  | "c" :: _ -> C
  | _ -> C  (* Default to C if no matching extension is found *)
