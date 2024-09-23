open Diff
open Exceptions
open Preprocess_util

let read_patch_file (file_path: string) : string =
  if String.length file_path = 0 then raise Empty_path;
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  if String.length content = 0 then raise (Empty_file file_path)
  else content

(** The main function to extract diffs from a list of lines. *)
let extract_diffs lines =
  let rec handleHunks diffs current_diff lines =
    let rec handleHunksHelper diffs current_diff before_context after_context lines = match lines with
      | [] -> (current_diff :: diffs)
      | line :: rest ->
        match line with
        | line when (String.starts_with ~prefix: "diff" line || String.starts_with ~prefix: "From" line) ->
          aux (current_diff :: diffs) None (line :: rest)
        | line when String.starts_with ~prefix: "@@" line ->
          let new_line_no = extract_line_number line in
          if current_diff.line_no = 0 then
            let updated_diff = {current_diff with line_no=new_line_no} in
            handleHunksHelper diffs updated_diff before_context after_context rest
          else
            let new_diff = (create_diff ~file_name: current_diff.file_name ~line_no: new_line_no ()) in
            handleHunksHelper (current_diff::diffs) new_diff before_context after_context rest
        | line when (String.starts_with ~prefix:"+" line || String.starts_with ~prefix:"-" line) &&
                    not (String.starts_with ~prefix:"+++" line || String.starts_with ~prefix:"---" line) ->
          let (original_lines, remaining_after_first_pass) = span (fun l -> l <> "" && l.[0] = '-') (line :: rest) in
          let original_lines = List.map (clean_line "-") original_lines in
          (match original_lines with
           | [] ->
             let (modified_lines, remaining_after_second_pass) =
               span (fun l -> l <> "" && l.[0] = '+') (remaining_after_first_pass) in
             let (original_second, remaining_after_third_pass) =
               span (fun l -> l <> "" && l.[0] = '-') (remaining_after_second_pass) in
             let original_second = List.map (clean_line "-") original_second in
             let modified_lines = List.map (clean_line "+") modified_lines in
             let updated_diff = {current_diff with original_lines=original_second ;  modified_lines} in
             handleHunksHelper diffs updated_diff before_context after_context remaining_after_third_pass
           | _ ->
             let (modified_lines, remaining_after_second_pass) =
               span (fun l -> l <> "" && l.[0] = '+') (remaining_after_first_pass)
             in
             let modified_lines = List.map (clean_line "+") modified_lines in
             let updated_diff = {current_diff with original_lines ;  modified_lines} in
             handleHunksHelper diffs updated_diff before_context after_context remaining_after_second_pass)
        | _ ->
          if current_diff.original_lines <> [] || current_diff.modified_lines <> [] then
            let updated_diff = {current_diff with after_context = (line :: current_diff.after_context)}
            in
            handleHunksHelper diffs updated_diff before_context (line :: after_context) rest
          else
            let updated_diff = {current_diff with before_context = (line :: current_diff.before_context)}
            in
            handleHunksHelper diffs updated_diff (line :: before_context) after_context rest
    in
    handleHunksHelper diffs current_diff [] [] lines

  and aux (diffs: diff list) (current_diff: diff option) (context: string list) =
    let handleHeader diffs current_diff context =
      let remaining = skip (fun l -> l = "---") context in
      (match current_diff with
       | Some diff -> aux (diff :: diffs) None remaining
       | None -> aux diffs None remaining)
    in
    match context with
    | [] ->
      (match current_diff with
       | Some d -> d :: diffs
       | None -> diffs)
    | line :: rest as lines ->
      match line with
      | "" -> aux diffs current_diff rest
      | line when (String.starts_with ~prefix: "From" line) -> handleHeader diffs current_diff rest
      | line when String.starts_with ~prefix: "diff" line ->
        let file_name = extract_filename (List.hd lines) in
        let new_diff = (create_diff ~file_name ()) in
        let updated_diffs = match current_diff with
          | Some d -> (d :: diffs)
          | None -> diffs
        in
        handleHunks updated_diffs new_diff rest
      | _ -> aux diffs current_diff rest
  in
  aux [] None lines

(** Preprocesses a patch string and extracts diffs. *)
let preprocess_patch (context: string) : diff list =
  let lines = split_and_strip context in
  extract_diffs lines
