let () =
  Alcotest.run "All tests" [
    "Preprocess Tests", Test_preprocess.tests;
    "Preprocess Util Tests", Test_preprocess_util.tests;
  ]
