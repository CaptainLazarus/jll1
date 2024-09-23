let () =
  Alcotest.run "All tests" [
      "Preprocess Tests", Test_preprocess.tests;
      "Expression Reducer Tests", Test_max_valid_expression.tests;
  ]
