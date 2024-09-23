type diff = {
  file_name: string;
  line_no: int;
  before_context: string list;
  after_context: string list;
  original_lines: string list;
  modified_lines: string list;
}

let create_diff
      ?(line_no=0)
      ?(before_context=[])
      ?(after_context=[])
      ?(original_lines=[])
      ?(modified_lines=[])
      ~file_name
      ()
  = { file_name; line_no; before_context; after_context; original_lines; modified_lines }

let print_diff p =
  Printf.printf
    "\n\n\n---diff\n\nFileName: %s\nLineNo: %d\nBeforeContext:\n%s\nOriginalLines:\n%s\nModifiedLines:\n%s\nAfterContext:\n%s\n\n---diff\n\n\n"
    p.file_name
    p.line_no
    (String.concat "\n" p.before_context)
    (String.concat "\n" p.original_lines)
    (String.concat "\n" p.modified_lines)
    (String.concat "\n" p.after_context)
