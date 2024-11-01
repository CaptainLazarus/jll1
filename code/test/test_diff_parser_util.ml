open Patch_processor.Diff_parser_util
open Tree_sitter_bindings.Tree_sitter_output_t

let test_extend_indent () =
  (* Expected passing cases *)
  Alcotest.(check string) "Divisible by 4" "abcd| " (extend_indent "abcd");
  Alcotest.(check string) "Not divisible by 4" "abc  " (extend_indent "abc")

let test_string_of_node_kind () =
  (* Expected passing cases *)
  Alcotest.(check string) "Name node kind" "name" (string_of_node_kind (Name "name"));
  Alcotest.(check string) "Literal node kind" "\"literal\"" (string_of_node_kind (Literal "literal"));
  Alcotest.(check string) "Error node kind" "!ERROR!" (string_of_node_kind Error)

let test_string_of_position () =
  let pos = { row = 1; column = 2 } in
  (* Expected passing case *)
  Alcotest.(check string) "Position formatting" "(row: 1, column: 2)" (string_of_position pos)

(* Test suite *)

let tests = [
  Alcotest.test_case "Test extend_indent" `Quick test_extend_indent;
  Alcotest.test_case "Test string_of_node_kind" `Quick test_string_of_node_kind;
  Alcotest.test_case "Test string_of_position" `Quick test_string_of_position;
]
