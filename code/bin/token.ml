type token =
  | LParen | RParen | LBrace | RBrace | LBracket | RBracket
  | Identifier of string
  | Keyword of string
  | StringLiteral of string
  | NumberLiteral of string
  | Operator of string
  | SpecialChar of char
  | Comment of string
  | Whitespace of string
  | EOF


  let is_whitespace c = match c with ' ' | '\n' | '\t' | '\r' -> true | _ -> false
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_alpha c || is_digit c
let is_operator c = match c with '+' | '-' | '*' | '/' | '%' | '=' | '!' | '<' | '>' | '&' | '|' | '^' | '~' -> true | _ -> false
let is_special_char c = match c with ',' | ';' | ':' | '.' | '?' | '#' | '@' | '$' | '`' -> true | _ -> false
