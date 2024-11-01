open Patch_processor

(* let reduce_to_minimal_expression (changes: string list) : string list = *)
(*   changes *)

(* let comby (expressions: string list) : string list = *)
(*   expressions *)

(* let gumtree (changes: string list) : unit = *)
(*   List.iter print_endline changes *)

let analyse_patch (file_path: string) =
  let patch_content = Preprocess.read_patch_file file_path in
  (* print_endline "Original Patch Content:"; *)

  let changes = Preprocess.preprocess_patch patch_content in
  (* print_endline "\nProcessed Changes with Context:"; *)
  (* List.iter Diff.print_diff changes; *)

  let csts = (Diff_parser.parse changes) in
  print_endline "\n Parse diffs";
  List.iter (Cst.print_cst) csts

  (* let comby_result = comby minimal_changes in *)
  (* print_endline "\nComby Processed Changes:"; *)
  (* List.iter print_endline comby_result; *)

  (* print_endline "\nGumTree Integration Output:"; *)
  (* gumtree comby_result *)

let () =
  match Sys.argv with
  | [| _; file_path |] -> analyse_patch file_path
  | _ ->
    Printf.eprintf "Usage: %s <patch_file_path>\n" Sys.argv.(0);
    exit 1
