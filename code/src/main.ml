module Token = struct
  type token =
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBracket
    | RBracket
    | Whitespace of string
    | StringLiteral of string
    | Comment of string
    | AnyToken of string
    | EOF
end

open Token

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let rec lex input =
  let rec consume_whitespace chars =
    match chars with
    | c :: cs when is_whitespace c ->
        let whitespace, rest = consume_whitespace cs in
        (c :: whitespace, rest)
    | cs -> ([], cs)
  in

  let rec consume_string chars =
    match chars with
    | '"' :: rest -> ([], rest)
    | '\\' :: c :: rest ->
        let string, remaining = consume_string rest in
        (c :: string, remaining)
    | c :: rest ->
        let string, remaining = consume_string rest in
        (c :: string, remaining)
    | [] -> ([], [])
  in

  let rec consume_comment chars =
    match chars with
    | '*' :: '/' :: rest -> ([], rest)
    | c :: rest ->
        let comment, remaining = consume_comment rest in
        (c :: comment, remaining)
    | [] -> ([], [])
  in

  let rec consume_any_token chars =
    match chars with
    | c :: cs when not (is_whitespace c || c = '"' || c = '(' || c = ')' || c = '{' || c = '}' || c = '[' || c = ']') ->
        let token, rest = consume_any_token cs in
        (c :: token, rest)
    | cs -> ([], cs)
  in

  match input with
  | [] -> [EOF]
  | '(' :: rest -> LParen :: lex rest
  | ')' :: rest -> RParen :: lex rest
  | '{' :: rest -> LBrace :: lex rest
  | '}' :: rest -> RBrace :: lex rest
  | '[' :: rest -> LBracket :: lex rest
  | ']' :: rest -> RBracket :: lex rest
  | '"' :: rest ->
      let string_chars, remaining = consume_string rest in
      StringLiteral (String.of_seq (List.to_seq string_chars)) :: lex remaining
  | '/' :: '*' :: rest ->
      let comment_chars, remaining = consume_comment rest in
      Comment (String.of_seq (List.to_seq comment_chars)) :: lex remaining
  | c :: rest when is_whitespace c ->
      let whitespace_chars, remaining = consume_whitespace (c :: rest) in
      Whitespace (String.of_seq (List.to_seq whitespace_chars)) :: lex remaining
  | chars ->
      let token_chars, remaining = consume_any_token chars in
      if token_chars <> [] then
        AnyToken (String.of_seq (List.to_seq token_chars)) :: lex remaining
      else
        lex remaining

let read_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let input = read_file filename in
    let tokens = lex (input |> String.to_seq |> List.of_seq) in
    List.iter (fun token ->
      match token with
      | LParen -> Printf.printf "LParen\t"
      | RParen -> Printf.printf "RParen\t"
      | LBrace -> Printf.printf "LBrace\t"
      | RBrace -> Printf.printf "RBrace\t"
      | LBracket -> Printf.printf "LBracket\t"
      | RBracket -> Printf.printf "RBracket\t"
      | Whitespace s -> Printf.printf "Whitespace: %s\t" s
      | StringLiteral s -> Printf.printf "StringLiteral: %s\t" s
      | Comment s -> Printf.printf "Comment: %s\t" s
      | AnyToken s -> Printf.printf "AnyToken: %s\t" s
      | EOF -> Printf.printf "EOF\t"
    ) tokens