Printf.printf "Current directory: %s\n" (Sys.getcwd())
open Patch_processor.Preprocess

let test_split_lines () =
  let input = "line1\nline2\nline3" in
  let expected_output = ["line1"; "line2"; "line3"] in
  Alcotest.(check (list string)) "splits on newlines" expected_output (split_lines input)

let test_read_patch_file () =
  let fixture_path = "./patches/rust_pr.patch" in
  (* Read the expected content directly from the fixture file *)
  let expected_content = 
    let ic = open_in fixture_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in
  
  (* Test the read_patch_file function *)
  let result = read_patch_file fixture_path in
  Alcotest.(check string) "reads file content correctly" expected_content result

let tests = [
  Alcotest.test_case "Test split_lines function" `Quick test_split_lines;
  Alcotest.test_case "Test read_patch_file function" `Quick test_read_patch_file;
]
