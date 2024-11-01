let () =
  Alcotest.run "All tests" [
    "Preprocess Tests", Test_preprocess.tests;
    "Preprocess Util Tests", Test_preprocess_util.tests;
    "Diff Parser Util Tests", Test_diff_parser_util.tests;
  ]
