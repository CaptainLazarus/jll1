open Expression
open Diff

let sp = String.concat " "

let get_expression diff =
  let rev_before_context, rev_after_context = (List.rev diff.before_context), (List.rev diff.after_context) in
  let original_expression = sp (rev_before_context @ diff.original_lines @ rev_after_context) in
  let modified_expression = sp (rev_before_context @ diff.modified_lines @ rev_after_context) in
  create_expression
    ~file_name: diff.file_name
    ~line_no: diff.line_no
    ~before_context: (sp rev_before_context)
    ~after_context: (sp rev_after_context)
    ~original_expression
    ~modified_expression
    ()

let parse_max_expression expression =
  expression

let parse_min_expression expression =
  expression
                                      
let reduce_diff_to_expression (diff: diff) = function
  | "max" -> parse_max_expression (get_expression diff) 
  | "min" -> parse_min_expression (get_expression diff)
  | _ -> failwith "Unknown Operation"
