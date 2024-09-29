type expression = {
  file_name: string;
  line_no: int;
  before_context: string;
  after_context: string;
  original_expression: string;
  modified_expression: string;
}

let create_expression
      ?(line_no=0)
      ?(before_context="")
      ?(after_context="")
      ?(original_expression="")
      ?(modified_expression="")
      ~file_name
      ()
  = { file_name; line_no; before_context; after_context; original_expression; modified_expression }

let print_expression expr =
  Printf.printf
    "\n\n\n---min_ve\n\nFileName: %s\nLineNo: %d\nBeforeContext:\n%s\nOriginalExpression:\n%s\nModifiedExpression:\n%s\nAfterContext:\n%s\n\n---min_ve\n\n\n"
    expr.file_name
    expr.line_no
    expr.before_context
    expr.original_expression
    expr.modified_expression
    expr.after_context
