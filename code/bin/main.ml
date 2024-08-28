open Patch_parser
open Diff

let read_patch_file (file_path: string) : string =
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

(* let reduce_to_minimal_expression (changes: string list) : string list = *)
(*   changes *)

(* let comby (expressions: string list) : string list = *)
(*   expressions *)

(* let gumtree (changes: string list) : unit = *)
(*   List.iter print_endline changes *)

let analyse_patch (file_path: string) =
  let patch_content = read_patch_file file_path in
  print_endline "Original Patch Content:";
  (* print_endline patch_content; *)

  let changes = preprocess_patch patch_content in
  print_endline "\nProcessed Changes with Context:";
  List.iter print_diff changes

  (* let minimal_changes = reduce_to_minimal_expression changes in *)
  (* print_endline "\nMinimal Changes:"; *)
  (* List.iter print_endline minimal_changes; *)

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
