open Alcotest
open Patch_processor.Preprocess_util

let test_split_and_strip () =
  let test_cases = [
    ("empty string", "", []);
    ("single line", "line", ["line"]);
    ("multiple lines", "line1\nline2\nline3", ["line1"; "line2"; "line3"]);
    ("lines with spaces", " line1 \n line2 \n line3 ", ["line1"; "line2"; "line3"]);
    ("lines with empty lines", "line1\n\nline3", ["line1"; "line3"]);
    ("only newlines", "\n\n", []);
  ] in
  List.iter (fun (desc, input, expected) ->
      check (list string) desc expected (split_and_strip input)
    ) test_cases

let test_extract_line_number () =
  let test_cases = [
    ("valid line number", "@@ -1,3 +4,5 @@", 4);
    ("single line addition", "@@ -2 +5 @@", 5);
    ("no line number", "@@ invalid header @@", 0);
    ("empty string", "", 0);
    ("only newlines", "\n\n", 0);
  ] in
  List.iter (fun (desc, input, expected) ->
      check int desc expected (extract_line_number input)
    ) test_cases

let test_clean_line () =
  let test_cases = [
    ("prefix present", "+line1", "+", "line1");
    ("prefix absent", "line2", "+", "line2");
    ("empty line", "", "+", "");
    ("longer prefix than line", "x", "xxx", "x");
    ("prefix with spaces", "   line3", "   ", "line3");
  ] in
  List.iter (fun (desc, input, prefix, expected) ->
      check string desc expected (clean_line prefix input)
    ) test_cases

let test_span () =
  let test_cases = [
    ("empty list", (fun _ -> true), [], ([], []));
    ("all match", (fun x -> x < 10), [1; 2; 3], ([1; 2; 3], []));
    ("none match", (fun x -> x > 10), [1; 2; 3], ([], [1; 2; 3]));
    ("partial match", (fun x -> x < 2), [1; 2; 3], ([1], [2; 3]));
  ] in
  List.iter (fun (desc, predicate, input, (expected_prefix, expected_rest)) ->
      let (prefix, rest) = span predicate input in
      check (list int) (desc ^ " - prefix") expected_prefix prefix;
      check (list int) (desc ^ " - rest") expected_rest rest
    ) test_cases

let test_extract_filename () =
  let test_cases = [
    ("valid diff header", "diff --git a/file.txt b/file.txt", "file.txt");
    ("no b/ prefix", "diff --git a/file.txt file.txt", "file.txt");
    ("nonstandard format", "some invalid diff line", "line");
    ("empty string", "", "");
  ] in
  List.iter (fun (desc, input, expected) ->
      check string desc expected (extract_filename input)
    ) test_cases

let test_skip () =
  let test_cases = [
    ("skip none", (fun _ -> true), [1; 2; 3], [1; 2; 3]);
    ("skip all", (fun _ -> false), [1; 2; 3], []);
    ("skip some", (fun x -> x < 2), [1; 2; 3], [1; 2; 3]); (* The full list since the first element satisfies the predicate *)
    ("empty list", (fun _ -> true), [], []);
    ("skip after first", (fun x -> x > 1), [1; 2; 3], [2; 3]); (* Starts at the first element that satisfies predicate *)
  ] in
  List.iter (fun (desc, predicate, input, expected) ->
      check (list int) desc expected (skip predicate input)
    ) test_cases

let tests = [
  Alcotest.test_case "Test split_and_strip" `Quick test_split_and_strip;
  Alcotest.test_case "Test extract_line_number" `Quick test_extract_line_number;
  Alcotest.test_case "Test clean_line" `Quick test_clean_line;
  Alcotest.test_case "Test span" `Quick test_span;
  Alcotest.test_case "Test extract_filename" `Quick test_extract_filename;
  Alcotest.test_case "Test skip" `Quick test_skip;
]
