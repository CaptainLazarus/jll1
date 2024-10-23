open Patch_processor.Diff
open Patch_processor.Preprocess
open Patch_processor.Preprocess_util
open Patch_processor.Exceptions

let test_read_empty_patch_file () =
  let fixture_path = "./patches/empty.patch" in
  Alcotest.check_raises "raises exception on empty file" (Empty_file fixture_path)
    (fun () -> ignore (read_patch_file fixture_path))

let test_read_empty_path () =
  Alcotest.check_raises "raises exception on empty path" Empty_path
    (fun () -> ignore (read_patch_file ""))

let diff_testable =
  let pp_diff fmt diff =
    Format.fprintf fmt "{ file_name = %s; line_no = %d; before_context = %s; after_context = %s; original_lines = %s; modified_lines = %s }"
      diff.file_name
      diff.line_no
      (String.concat "\n" (List.rev diff.before_context))
      (String.concat "\n" (List.rev diff.after_context))
      (String.concat "\n" (List.rev diff.original_lines))
      (String.concat "\n" (List.rev diff.modified_lines))
  in
  Alcotest.testable pp_diff (=)

let test_parse_header_only_diff () =
  let patch_file = "./patches/header_only.patch" in
  let context = read_patch_file patch_file in
  let expected = [] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "header only diff parsed" expected result

let test_parse_empty_diff () =
  let context = "" in
  let expected = [] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "empty diff parsed" expected result

let test_parse_single_diff () =
  let patch_file = "./patches/single_diff.patch" in
  let context = read_patch_file patch_file in
  let expected = [
    create_diff
      ~file_name:"file1.txt"
      ~line_no:1
      ~before_context:[]
      ~after_context:[]
      ~original_lines:["old line;"]
      ~modified_lines:["new line;"]
      ()
  ] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "single diff parsed" expected result

let test_parse_single_diff_with_context () =
  let patch_file = "./patches/single_diff_with_context.patch" in
  let context = read_patch_file patch_file in
  let expected = [
    create_diff
      ~file_name:"file1.txt"
      ~line_no:1
      ~before_context:["{"]
      ~after_context:["}"]
      ~original_lines:["old line;"]
      ~modified_lines:["new line;"]
      ()
  ] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "single diff with context parsed" expected result

let test_parse_single_diff_multi_line_context () =
  let patch_file = "./patches/single_diff_multi_line_context.patch" in
  let context = read_patch_file patch_file in
  let expected = [
    create_diff
      ~file_name:"file1.txt"
      ~line_no:1
      ~before_context:[ "sadas" ; "{"]
      ~after_context:["}" ; "(ss)as"]
      ~original_lines:["old line;"]
      ~modified_lines:["new line;"]
      ()
  ] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "single diff with context parsed" expected result


let test_parse_single_diff_only_additions () =
  let patch_file = "./patches/single_diff_only_additions.patch" in
  let context = read_patch_file patch_file in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list int)) "single diff parsed" [26] (List.map (fun x -> List.length x.modified_lines) result)

let test_parse_multi_diff_extract_correct_number_of_diffs () =
  let patch_file = "./patches/multi_diff.patch" in
  let context = read_patch_file patch_file in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check int) "multi diff file has 4 diffs" 5 (List.length result)

let test_parse_multi_diff_extracts_correct_after_context () =
  let patch_file = "./patches/multi_diff.patch" in
  let expected = [
    "}"
    ]
  in
  let context = read_patch_file patch_file in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list string)) "multi diff file has after context" expected (List.hd result).after_context

let tests = [
  Alcotest.test_case "Test read_patch_file with empty file" `Quick test_read_empty_patch_file;
  Alcotest.test_case "Test read_patch_file with empty path" `Quick test_read_empty_path;
  Alcotest.test_case "Test empty diff parsing" `Quick test_parse_empty_diff;
  Alcotest.test_case "Test header only diff parsing" `Quick test_parse_header_only_diff;
  Alcotest.test_case "Test single diff parsing" `Quick test_parse_single_diff;
  Alcotest.test_case "Test single diff parsing" `Quick test_parse_single_diff_with_context;
  Alcotest.test_case "Test single diff parsing" `Quick test_parse_single_diff_multi_line_context;
  Alcotest.test_case "Test single diff parsing with only additions" `Quick test_parse_single_diff_only_additions;
  Alcotest.test_case "Test multi diff parsing" `Quick test_parse_multi_diff_extract_correct_number_of_diffs;
  Alcotest.test_case "Test multi diff parsing" `Quick test_parse_multi_diff_extracts_correct_after_context;
]
