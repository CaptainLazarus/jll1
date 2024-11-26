type line_range = (int*int)

let print_range (pair: line_range) =
  let (a, b) = pair in
  Printf.printf "\n(%d, %d)\n" a b

type cst = {
  file_name: string;
  line_no: int;
  original_code: string;
  modified_code: string;
  original_tree: string;
  modified_tree: string;
  original_line_nos: line_range;
  modified_line_nos: line_range
}

let create_cst
    ?(line_no=0)
    ?(original_code="")
    ?(modified_code="")
    ?(original_tree="")
    ?(modified_tree="")
    ?(file_name="")
    ?(original_line_nos=(0,0))
    ?(modified_line_nos=(0,0))
    ()
  = { file_name; line_no; original_code ; modified_code; original_tree; modified_tree; original_line_nos; modified_line_nos }

let print_cst p =
  Printf.printf
    "\n\n\n---CST\n\n
FileName: %s\n
LineNo: %d\n
OriginalCode: %s\n
ModifiedCode: %s\n
OriginalTree: %s\n
ModifiedTree: %s\n
"
    p.file_name
    p.line_no
    p.original_code
    p.modified_code
    ""(* p.original_tree *)
    ""(* p.modified_tree *)
  ;
  print_range p.modified_line_nos;
  print_range p.original_line_nos
