open Max_valid_expression
open Min_valid_expression

let reduce_diff_to_max_expression _diff =
  create_max_ve ~file_name:"temp.txt" ()

let reduce_diff_to_min_expression _diff =
  create_min_ve ~file_name:"temp.txt" ()
