type diff = {
    file_name: string;
    line_no: int;
    before_context: string;
    after_context: string;
    added_lines: string;
    removed_lines: string;
  }

let create_diff
      ~file_name
      ~line_no
      ~before_context
      ~after_context
      ~added_lines
      ~removed_lines
  =   { file_name; line_no; before_context; after_context; added_lines; removed_lines }

let print_diff p =
  Printf.printf
    "---dif\n\nFileName: %s\nLineNo: %d\nBeforeContext: %s\nAfterContext: %s\nAddedLines: %s\nRemovedLines: %s\n\n---dif\n"
    p.file_name
    p.line_no
    p.before_context
    p.after_context
    p.added_lines
    p.removed_lines

