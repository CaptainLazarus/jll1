open Token

let rec tokenize input =
  match input with
  | [] -> [EOF]
  | '(' :: xs -> LParen :: tokenize xs
  | ')' :: xs -> RParen :: tokenize xs
  | '{' :: xs -> LBrace :: tokenize xs
  | '}' :: xs -> RBrace :: tokenize xs
  | '[' :: xs -> LBracket :: tokenize xs
  | ']' :: xs -> RBracket :: tokenize xs
  | '"' :: xs -> tokenize_string [] xs
  | '/' :: '*' :: xs -> tokenize_multiline_comment xs
  | '/' :: '/' :: xs -> tokenize_singleline_comment xs
  | c :: xs when is_whitespace c -> tokenize_whitespace (c :: xs)
  | c :: xs when is_digit c -> tokenize_number (c :: xs)
  | c :: xs when is_alpha c -> tokenize_identifier (c :: xs)
  | c :: xs when is_operator c -> tokenize_operator (c :: xs)
  | c :: xs when is_special_char c -> SpecialChar c :: tokenize xs
  | c :: xs ->
      Printf.printf "Unexpected character: %c\n" c;
      tokenize xs

and tokenize_string acc input =
  match input with
  | [] -> failwith "Unterminated string literal"
  | '"' :: xs -> StringLiteral (String.of_seq (List.to_seq (List.rev acc))) :: tokenize xs
  | '\\' :: '"' :: xs -> tokenize_string ('"' :: '\\' :: acc) xs  (* Handle escaped quotes *)
  | c :: xs -> tokenize_string (c :: acc) xs

and tokenize_multiline_comment acc =
  match acc with
  | '*' :: '/' :: xs -> Comment (String.of_seq (List.to_seq (List.rev acc))) :: tokenize xs
  | [] -> failwith "Unterminated multiline comment"
  | c :: xs -> tokenize_multiline_comment (c :: acc)

and tokenize_singleline_comment acc =
  match acc with
  | '\n' :: xs -> Comment (String.of_seq (List.to_seq (List.rev acc))) :: tokenize xs
  | [] -> Comment (String.of_seq (List.to_seq (List.rev acc))) :: tokenize []
  | c :: xs -> tokenize_singleline_comment (c :: acc)

and tokenize_whitespace acc =
  match acc with
  | c :: xs when is_whitespace c -> tokenize_whitespace (c :: acc)
  | _ -> Whitespace (String.of_seq (List.to_seq (List.rev acc))) :: tokenize acc

and tokenize_number acc =
  match acc with
  | c :: xs when is_digit c || c = '.' -> tokenize_number (c :: acc)
  | _ -> NumberLiteral (String.of_seq (List.to_seq (List.rev acc))) :: tokenize acc

and tokenize_identifier acc =
  match acc with
  | c :: xs when is_alnum c -> tokenize_identifier (c :: acc)
  | _ ->
      let id = String.of_seq (List.to_seq (List.rev acc)) in
      if List.mem id keywords then
        Keyword id :: tokenize acc
      else
        Identifier id :: tokenize acc

and tokenize_operator acc =
  match acc with
  | c :: xs when is_operator c -> tokenize_operator (c :: acc)
  | _ -> Operator (String.of_seq (List.to_seq (List.rev acc))) :: tokenize acc