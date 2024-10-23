open Diff
open Exceptions
open Preprocess_util

let not_empty (diff: diff) : bool =
  (diff.original_lines <> [] || diff.modified_lines <> [])

let add_context d c =
  match d with
  | d when not_empty d -> {d with after_context=c}
  | _ -> {d with before_context=c}

let append ds = function
  | d when not_empty d -> d :: ds
  | _ -> ds

let empty_diff = create_diff ()

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
    let rec handleHunksHelper diffs current_diff context = function
      | [] -> append diffs (add_context current_diff context)
      | line :: rest ->
        match line with
        | line when (String.starts_with ~prefix: "diff" line || String.starts_with ~prefix: "From" line) ->
          aux (append diffs (add_context current_diff context)) empty_diff (line :: rest)
        | line when String.starts_with ~prefix: "@@" line -> (*Can have multiple @@ under a single diff*)
          let new_line_no = extract_line_number line in
          if not_empty current_diff then
            let new_diff = (create_diff ~file_name: current_diff.file_name ~line_no: new_line_no ()) in
            handleHunksHelper (append diffs (add_context current_diff context)) new_diff context rest
          else
            let updated_diff = {current_diff with line_no=new_line_no} in
            handleHunksHelper diffs updated_diff context rest
        | line when (String.starts_with ~prefix:"+" line || String.starts_with ~prefix:"-" line) &&
                    not (String.starts_with ~prefix:"+++" line || String.starts_with ~prefix:"---" line) ->
          let (original_lines, remaining_lines) =
            span (fun l -> l <> "" && l.[0] = '-') (line :: rest)
          in
          let (modified_lines, remaining_context) =
            span (fun l -> l <> "" && l.[0] = '+') (remaining_lines)
          in
          let original_lines = List.map (clean_line "-") original_lines in
          let modified_lines = List.map (clean_line "+") modified_lines in
          if not_empty current_diff then
            let updated_diff = (add_context current_diff context) in
            let new_diff = {current_diff with original_lines ; modified_lines; before_context=context} in
            handleHunksHelper (append diffs updated_diff) new_diff [] remaining_context
          else
            let updated_diff = {current_diff with original_lines ; modified_lines; before_context=context} in
            handleHunksHelper diffs updated_diff [] remaining_context
        | _ ->
          handleHunksHelper diffs current_diff (line::context) rest
    in
    handleHunksHelper diffs current_diff [] lines

  and aux (diffs: diff list) (current_diff: diff) (context: string list) =
    let handleHeader diffs current_diff context =
      let remaining = skip (fun l -> l = "---") context in
      (match current_diff with
       | d when (not_empty d) -> aux (d :: diffs) empty_diff remaining
       | _ -> aux diffs current_diff remaining)
    in
    match context with
    | [] ->
      (match current_diff with
       | d when (not_empty d) -> d :: diffs
       | _ -> diffs)
    | line :: rest as lines ->
      match line with
      | "" -> aux diffs current_diff rest
      | line when (String.starts_with ~prefix: "From" line) -> handleHeader diffs current_diff rest
      | line when String.starts_with ~prefix: "diff" line ->
        let file_name = extract_filename (List.hd lines) in
        if not_empty current_diff then
          let new_diff = create_diff ~file_name () in
          handleHunks (current_diff :: diffs) new_diff rest
        else
          let updated_diff = {current_diff with file_name} in
          handleHunks diffs updated_diff rest
      | _ -> aux diffs current_diff rest
  in
  aux [] empty_diff lines

(** Preprocesses a patch string and extracts diffs. *)
let preprocess_patch (context: string) : diff list =
  let lines = split_and_strip context in
  extract_diffs lines
