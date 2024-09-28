type min_ve = {
  file_name: string;
  line_no: int;
  before_context: string list;
  after_context: string list;
  original_expression: string;
  modified_expression: string;
}

let create_min_ve
      ?(line_no=0)
      ?(before_context=[])
      ?(after_context=[])
      ?(original_expression="")
      ?(modified_expression="")
      ~file_name
      ()
  = { file_name; line_no; before_context; after_context; original_expression; modified_expression }

let print_min_ve p =
  Printf.printf
    "\n\n\n---min_ve\n\nFileName: %s\nLineNo: %d\nBeforeContext:\n%s\nOriginalExpression:\n%s\nModifiedExpression:\n%s\nAfterContext:\n%s\n\n---min_ve\n\n\n"
    p.file_name
    p.line_no
    (String.concat "\n" p.before_context)
    p.original_expression
    p.modified_expression
    (String.concat "\n" p.after_context)
