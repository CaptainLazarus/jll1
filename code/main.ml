type token =
  | Identifier of string
  | Keyword of string
  | Operator of string
  | IntLiteral of int
  | StringLiteral of string
  | LParen
  | RParen
  | EOF
  | Unknown of string

let is_digit = function '0' .. '9' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_identifier_char c = is_letter c || is_digit c || c = '_'

let rec lex input =
  let rec consume_number chars =
    let rec aux acc = function
      | c :: cs when is_digit c -> aux (c :: acc) cs
      | cs -> (List.rev acc, cs)
    in aux [] chars
  in
  let rec consume_identifier chars =
    let rec aux acc = function
      | c :: cs when is_identifier_char c -> aux (c :: acc) cs
      | cs -> (List.rev acc, cs)
    in aux [] chars
  in
  match input with
  | [] -> [EOF]
  | ' ' :: rest -> lex rest  (* Skip whitespace *)
  | '(' :: rest -> LParen :: lex rest
  | ')' :: rest -> RParen :: lex rest
  | c :: rest when is_digit c ->
      let digits, rest' = consume_number (c :: rest) in
      IntLiteral (int_of_string (String.concat "" (List.map (String.make 1) digits))) :: lex rest'
  | c :: rest when is_letter c ->
      let id, rest' = consume_identifier (c :: rest) in
      let id_str = String.concat "" (List.map (String.make 1) id) in
      let token =
        match id_str with
        | "if" | "else" -> Keyword id_str
        | _ -> Identifier id_str
      in
      token :: lex rest'
  | c :: rest -> Unknown (String.make 1 c) :: lex rest

let input = "(if 123 else foo)"
let tokens = lex (input |> String.to_seq |> List.of_seq)
let () = List.iter (fun token -> Printf.printf "%s\n" (match token with
  | Identifier s -> "Identifier: " ^ s
  | Keyword s -> "Keyword: " ^ s
  | Operator s -> "Operator: " ^ s
  | IntLiteral n -> "IntLiteral: " ^ string_of_int n
  | StringLiteral s -> "StringLiteral: " ^ s
  | LParen -> "LParen"
  | RParen -> "RParen"
  | EOF -> "EOF"
  | Unknown s -> "Unknown: " ^ s
)) tokens
