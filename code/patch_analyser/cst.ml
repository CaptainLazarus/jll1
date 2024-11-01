type cst = {
  file_name: string;
  line_no: int;
  original_code: string;
  modified_code: string;
  original_tree: string;
  modified_tree: string;
}

let create_cst
    ?(line_no=0)
    ?(original_code="")
    ?(modified_code="")
    ?(original_tree="")
    ?(modified_tree="")
    ?(file_name="")
    ()
  = { file_name; line_no; original_code ; modified_code; original_tree; modified_tree }

let print_cst p =
  Printf.printf
    "\n\n\n---CST\n\n
FileName: %s\n
LineNo: %d\n
OriginalCode: %s\n
ModifiedCode: %s\n
OriginalTree: %s\n
ModifiedTree: %s\n
\n---CST\n\n\n"
    p.file_name
    p.line_no
    p.original_code
    p.modified_code
    p.original_tree
    p.modified_tree
