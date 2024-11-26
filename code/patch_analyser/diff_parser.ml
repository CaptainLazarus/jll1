open Diff
open Cst
open Diff_parser_util

let get_line_nos xs ys =
  let lx = List.length xs in
  let ly = List.length ys in
  (lx,lx+ly)

let parse_diff (d: diff) : cst =
  let original_code = (extract_code d.before_context d.original_lines d.after_context) in
  let modified_code = (extract_code d.before_context d.modified_lines d.after_context) in
  let lang = (get_lang d.file_name) in
  create_cst
    ~file_name:d.file_name
    ~line_no:d.line_no
    ~original_code
    ~modified_code
    ~original_tree: (parse_code original_code lang)
    ~modified_tree: (parse_code modified_code lang)
    ~original_line_nos: (get_line_nos d.before_context d.original_lines)
    ~modified_line_nos: (get_line_nos d.before_context d.modified_lines)
    ()
  
let parse (diffs: diff list) : (cst list) =
  List.map parse_diff diffs
