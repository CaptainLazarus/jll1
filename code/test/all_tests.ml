let () =
  Alcotest.run "All tests" [
    "Preprocess Tests", Test_preprocess.tests;
  ]
