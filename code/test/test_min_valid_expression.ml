(* open Test_preprocess *)
open Patch_processor.Diff
open Patch_processor.Diff_reducer
open Patch_processor.Min_valid_expression

let min_ve_testable =
  let pp_min_ve fmt min_ve =
    Format.fprintf fmt "{ file_name = %s; line_no = %d; before_context = %s; after_context = %s; original_expression = %s; modified_expression = %s }"
      min_ve.file_name
      min_ve.line_no
      (String.concat "\n" (List.rev min_ve.before_context))
      (String.concat "\n" (List.rev min_ve.after_context))
      min_ve.original_expression
      min_ve.modified_expression
  in
  Alcotest.testable pp_min_ve (=)

let test_diff ~file_name ~line_no ~before_context ~after_context ~original_lines ~modified_lines ~original_expression ~modified_expression expected_name =
  let diff = {
    file_name;
    line_no;
    before_context;
    after_context;
    original_lines;
    modified_lines;
  } in
  let expected = {
    file_name;
    line_no;
    before_context;
    after_context;
    original_expression;
    modified_expression;
  } in
  let result = reduce_diff_to_min_expression diff in
  Alcotest.(check min_ve_testable) expected_name expected result

(* Test cases *)

(* Single Bracket Pairs *)
let test_single_body_with_curly_braces () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:10
    ~before_context:["{"]
    ~after_context:["}"]
    ~original_lines:["do_something;"]
    ~modified_lines:["do_something_else;"]
    ~original_expression:"{do_something;}"
    ~modified_expression:"{do_something_else;}"
    "Single Body with {}"

let test_single_body_with_curly_braces_and_text () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:20
    ~before_context:["int x = 0;"; "{"]
    ~after_context:["}"; "return x;"]
    ~original_lines:["do_something;"]
    ~modified_lines:["do_something_else;"]
    ~original_expression:"{do_something;}"
    ~modified_expression:"{do_something_else;}"
    "Single Body with {} and surrounding text"

let test_single_body_with_parentheses () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:30
    ~before_context:["("]
    ~after_context:[")"]
    ~original_lines:["check_value_1;"]
    ~modified_lines:["check_value_2;"]
    ~original_expression:"(check_value_1;)"
    ~modified_expression:"(check_value_2;)"
    "Single Body with ()"

let test_single_body_with_parentheses_and_text () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:40
    ~before_context:["int res = "; "("]
    ~after_context:[")"; ";"]
    ~original_lines:["calculate_1;"]
    ~modified_lines:["calculate_2;"]
    ~original_expression:"(calculate_1;)"
    ~modified_expression:"(calculate_2;)"
    "Single Body with () and surrounding text"

let test_single_body_with_square_brackets () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:50
    ~before_context:["["]
    ~after_context:["]"]
    ~original_lines:["original_array_element"]
    ~modified_lines:["modified_array_element"]
    ~original_expression:"[original_array_element]"
    ~modified_expression:"[modified_array_element]"
    "Single Body with []"

let test_single_body_with_square_brackets_and_text () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:60
    ~before_context:["int arr[] = "; "["]
    ~after_context:["]"; ";"]
    ~original_lines:["original_array_element"]
    ~modified_lines:["modified_array_element"]
    ~original_expression:"[original_array_element]"
    ~modified_expression:"[modified_array_element]"
    "Single Body with [] and surrounding text"

(* (\* No Bracket Pairs *\) *)
(* let test_no_bracket_body () = *)
(*   test_diff  *)
(*     ~file_name:"example.cpp" *)
(*     ~line_no:70 *)
(*     ~before_context:[] *)
(*     ~after_context:[] *)
(*     ~original_lines:["do_something();"] *)
(*     ~modified_lines:["do_something_else();"] *)
(*     "Body with no brackets" *)

(* let test_body_with_single_curly_brace () = *)
(*   test_diff  *)
(*     ~file_name:"example.cpp" *)
(*     ~line_no:80 *)
(*     ~before_context:["{"] *)
(*     ~after_context:[] *)
(*     ~original_lines:["incomplete_statement();"] *)
(*     ~modified_lines:["new_statement();"] *)
(*     "Body with single {" *)

(* let test_body_with_single_square_bracket () = *)
(*   test_diff  *)
(*     ~file_name:"example.cpp" *)
(*     ~line_no:90 *)
(*     ~before_context:["["] *)
(*     ~after_context:[] *)
(*     ~original_lines:["array[0];"] *)
(*     ~modified_lines:["array[1];"] *)
(*     "Body with single [" *)

(* let test_body_with_single_parenthesis () = *)
(*   test_diff  *)
(*     ~file_name:"example.cpp" *)
(*     ~line_no:100 *)
(*     ~before_context:["("] *)
(*     ~after_context:[] *)
(*     ~original_lines:["function_call();"] *)
(*     ~modified_lines:["function_call_modified();"] *)
(*     "Body with single (" *)

let test_multi_curly_brace_body () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:110
    ~before_context:["{"; "{"]
    ~after_context:["}"; "}"]
    ~original_lines:["do_something;"]
    ~modified_lines:["do_something_else;"]
    ~original_expression:"{{do_something;}}"
    ~modified_expression:"{{do_something_else;}}"
    "Multi Body with {} balanced"

let test_multi_parenthesis_body () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:120
    ~before_context:["("]
    ~after_context:[")"]
    ~original_lines:["check_value(max(x, y));"]
    ~modified_lines:["check_value(max(a, b));"]
    ~original_expression:"(check_value(max(x, y));)"
    ~modified_expression:"(check_value(max(a, b));)"
    "Multi Body with () balanced"

let test_multi_square_bracket_body () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:130
    ~before_context:["["]
    ~after_context:["]"]
    ~original_lines:["matrix[0][1];"]
    ~modified_lines:["matrix[1][1];"]
    ~original_expression:"[matrix[0][1];]"
    ~modified_expression:"[matrix[1][1];]"
    "Multi Body with [] balanced"

let test_multi_mixed_bracket_body () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:140
    ~before_context:["{"; "("]
    ~after_context:[")"; "}"]
    ~original_lines:["process(array[0]);"]
    ~modified_lines:["process(array[1]);"]
    ~original_expression:"{(process(array[0]);)}"
    ~modified_expression:"{(process(array[1]);)}"
    "Multi Body with mixed {} and () balanced"

let test_multi_mixed_bracket_body_2 () =
  test_diff 
    ~file_name:"example.cpp"
    ~line_no:110
    ~before_context:["{"; "{"]
    ~after_context:["}"; "}"]
    ~original_lines:["do_something();"]
    ~modified_lines:["do_something_else();"]
    ~original_expression:"{{do_something();}}"
    ~modified_expression:"{{do_something_else();}}"
    "Multi Body with {} balanced"

(* Test suite *)

let tests = [
  Alcotest.test_case "Single Body with {}" `Quick test_single_body_with_curly_braces;
  Alcotest.test_case "Single Body with {} and surrounding text" `Quick test_single_body_with_curly_braces_and_text;
  Alcotest.test_case "Single Body with ()" `Quick test_single_body_with_parentheses;
  Alcotest.test_case "Single Body with () and surrounding text" `Quick test_single_body_with_parentheses_and_text;
  Alcotest.test_case "Single Body with []" `Quick test_single_body_with_square_brackets;
  Alcotest.test_case "Single Body with [] and surrounding text" `Quick test_single_body_with_square_brackets_and_text;
  (* Alcotest.test_case "Body with no brackets" `Quick test_no_bracket_body; *)
  (* Alcotest.test_case "Body with single {" `Quick test_body_with_single_curly_brace; *)
  (* Alcotest.test_case "Body with single [" `Quick test_body_with_single_square_bracket; *)
  (* Alcotest.test_case "Body with single (" `Quick test_body_with_single_parenthesis; *)
  Alcotest.test_case "Multi Body with {} balanced" `Quick test_multi_curly_brace_body;
  Alcotest.test_case "Multi Body with () balanced" `Quick test_multi_parenthesis_body;
  Alcotest.test_case "Multi Body with [] balanced" `Quick test_multi_square_bracket_body;
  Alcotest.test_case "Multi Body with mixed {} and () balanced" `Quick test_multi_mixed_bracket_body;
  Alcotest.test_case "Multi Body with mixed {} and () balanced" `Quick test_multi_mixed_bracket_body_2;
]
