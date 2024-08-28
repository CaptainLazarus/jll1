open Diff

let split_lines (s: string) : string list = String.split_on_char '\n' s

let extract_line_number line =
  let parts = String.split_on_char ' ' line in
  List.find_map (fun part ->
    try Some (int_of_string (String.sub part 1 (String.length part - 1)))
    with _ -> None
  ) parts |> Option.value ~default:0

let rec span pred lst =
  match lst with
  | [] -> ([], [])
  | x :: xs ->
      if pred x then
        let (ys, zs) = span pred xs in
        (x :: ys, zs)
      else
        ([], lst)

let extract_diffs (lines: string list) : diff list =
  let rec aux acc current_diff file_name line_no context = function
    | [] -> List.rev (Option.to_list current_diff @ acc)
    | line :: rest ->
        if line = "" then aux acc current_diff file_name line_no (context @ [line]) rest
        else match line.[0] with
        | '+' when not (String.starts_with ~prefix:"+++ " line) ->
            let (added, remaining) = span (fun l -> l <> "" && l.[0] = '+') (line :: rest) in
            let added_content = String.concat "\n" added ^ "\n" in
            let updated_diff = match current_diff with
              | Some d when d.file_name = file_name ->
                  Some { d with 
                    added_lines = d.added_lines ^ added_content;
                    before_context = String.concat "\n" context ^ "\n" }
              | _ ->
                  Some { file_name; line_no; 
                         before_context = String.concat "\n" context ^ "\n";
                         after_context = ""; added_lines = added_content; removed_lines = "" }
            in
            aux acc updated_diff file_name (line_no + List.length added) [] remaining
        | '-' when not (String.starts_with ~prefix:"--- " line) ->
            let (removed, remaining) = span (fun l -> l <> "" && l.[0] = '-') (line :: rest) in
            let removed_content = String.concat "\n" removed ^ "\n" in
            let updated_diff = match current_diff with
              | Some d when d.file_name = file_name ->
                  Some { d with 
                    removed_lines = d.removed_lines ^ removed_content;
                    before_context = String.concat "\n" context ^ "\n" }
              | _ ->
                  Some { file_name; line_no; 
                         before_context = String.concat "\n" context ^ "\n";
                         after_context = ""; added_lines = ""; removed_lines = removed_content }
            in
            aux acc updated_diff file_name (line_no + List.length removed) [] remaining
        | '@' ->
            let new_line_no = extract_line_number line in
            aux (Option.to_list current_diff @ acc) None file_name new_line_no [] rest
        | 'd' when String.length line > 4 && String.sub line 0 4 = "diff" ->
            let new_file = List.rev (String.split_on_char ' ' line) |> List.hd in
            aux (Option.to_list current_diff @ acc) None new_file 1 [] rest
        | _ ->
            aux acc (Option.map (fun d -> 
              if d.added_lines <> "" || d.removed_lines <> "" then
                { d with after_context = d.after_context ^ line ^ "\n" }
              else
                d
            ) current_diff) file_name (line_no + 1) (context @ [line]) rest
  in
  aux [] None "" 0 [] lines

let preprocess_patch (context: string) : diff list =
  extract_diffs (split_lines context)
