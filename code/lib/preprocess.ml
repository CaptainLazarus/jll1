open Diff

let read_patch_file (file_path: string) : string =
  let ic = open_in file_path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let split_and_strip (s: string) : string list =
  s
  |> String.trim  (* Remove leading and trailing whitespace, including newlines *)
  |> String.split_on_char '\n'  (* Split the cleaned string into lines *)
  |> List.filter (fun line -> line <> "")
  |> List.map (fun line -> String.trim line)

let extract_line_number line =
  let parts = String.split_on_char ' ' line in
  List.find_map (fun part ->
      if part.[0] = '+' then
        (* Split the part on the comma and take the first element as the line number *)
        let line_number_part = String.sub part 1 (String.length part - 1) in
        let line_number = String.split_on_char ',' line_number_part |> List.hd in
        try Some (int_of_string line_number)
        with _ -> None
      else
        None
    ) parts |> Option.value ~default:0


let rec span predicate xs =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
     if predicate x then
       let (ys, zs) = span predicate xs in
       (x :: ys, zs)
     else
       ([], x::xs)

let rec skip predicate xs =
  match xs with
  | [] -> []
  | x :: xs ->
     if predicate x then
       xs
     else
       skip predicate xs

let remove_leading_char s =
  if String.length s > 0 then
    String.sub s 1 (String.length s - 1)
  else
    s

let extract_diffs (lines: string list) : diff list =
  let rec aux acc current_diff file_name line_no context = function
    | [] -> List.rev (Option.to_list current_diff @ acc)
    | line :: rest ->
       if line = "" then aux acc current_diff file_name line_no context rest
       else match line.[0] with
            | 'F' when (String.starts_with ~prefix: "From" line) ->
               let remaining = skip (fun l -> l = "---") rest in
               (match current_diff with
                 | Some d -> aux ([d] @ acc) None "" 0 [] remaining
                 | None -> aux acc None "" 0 [] remaining)
            | 'd' when String.length line > 4 && String.sub line 0 4 = "diff" ->
               let filename_with_prefix = List.rev (String.split_on_char ' ' line) |> List.hd in
               let file_name =
                 if String.starts_with ~prefix:"b/" filename_with_prefix then
                   String.sub filename_with_prefix 2 (String.length filename_with_prefix - 2)
                 else
                   filename_with_prefix
               in
               let new_diff  = Some {
                                   file_name;
                                   line_no = 0;
                                   before_context = "";
                                   after_context = "";
                                   added_lines = "";
                                   removed_lines = "";
                                 } in
               aux (Option.to_list current_diff @ acc) new_diff file_name 0 [] rest
            | '@' ->
               let new_line_no = extract_line_number line in
               print_endline (string_of_int new_line_no);
               let updated_diff = match current_diff with
                 | Some d -> Some {d with line_no=new_line_no}
                 | None -> failwith "wtf" in (*Is there a point to optional diff ? It'll always have to be present anyway*)
               aux acc updated_diff file_name new_line_no [] rest (*Is there actually any point to sending the line number ?*)
            | '+' when not (String.starts_with ~prefix:"+++ " line) ->
               let (added, remaining) = span (fun l -> l <> "" && l.[0] = '+') (line :: rest) in
               let added_content = String.concat "\n" added in
               let updated_diff = match current_diff with
                 | Some d when d.file_name = file_name ->
                    Some { d with
                        added_lines = if d.added_lines <> "" then d.added_lines ^ "\n" ^ added_content else added_content;
                        before_context = String.concat "\n" context ^ "\n"}
                 | _ ->
                    failwith "How'd you get here?"
               in
               aux acc updated_diff file_name line_no [] remaining
            | '-' when not (String.starts_with ~prefix:"--- " line) ->
               let (removed, remaining) = span (fun l -> l <> "" && l.[0] = '-') (line :: rest) in
               let removed_content = String.concat "\n" removed in
               let updated_diff = match current_diff with
                 | Some d when d.file_name = file_name ->
                    Some { d with 
                        removed_lines = d.removed_lines ^ removed_content;
                        before_context = String.concat "\n" context ^ "\n"}
                 | _ ->
                    failwith "How'd you get here?"
               in
               aux acc updated_diff file_name line_no [] remaining
            | _ ->
               aux acc (Option.map (fun d -> 
                            if d.added_lines <> "" || d.removed_lines <> "" then
                              { d with after_context = d.after_context ^ "\n" ^ line }
                            else
                              d
                          ) current_diff) file_name (line_no) (context @ [line]) rest
  in
  aux [] None "" 0 [] lines

let preprocess_patch (context: string) : diff list =
  extract_diffs (split_and_strip context)
