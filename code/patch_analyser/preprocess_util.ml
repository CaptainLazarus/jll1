(** Splits a string into a list of trimmed, non-empty lines. *)
let split_and_strip (s: string) : string list =
  s
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line -> line <> "")

(** Extracts the starting line number from a hunk header line. *)
let extract_line_number (line: string) : int =
  try
    let parts = String.split_on_char ' ' line in
    let new_file_part = List.find (fun s -> String.starts_with ~prefix:"+" s) parts in
    let line_info = String.sub new_file_part 1 (String.length new_file_part - 1) in
    let line_no_str = List.hd (String.split_on_char ',' line_info) in
    int_of_string line_no_str
  with _ -> 0

(** Removes the specified prefix from a line, if present. *)
let clean_line (prefix: string) (line: string) : string =
  let prefix_len = String.length prefix in
  let line_len = String.length line in
    let cleaned_line =
    if line_len >= prefix_len && String.sub line 0 prefix_len = prefix then
      String.sub line prefix_len (line_len - prefix_len)
    else
      line
  in
  String.trim cleaned_line

(** Splits a list into the longest prefix satisfying a predicate and the rest. *)
let rec span (predicate: 'a -> bool) (xs: 'a list) : 'a list * 'a list =
  match xs with
  | [] -> ([], [])
  | x :: xs' ->
    if predicate x then
      let (ys, zs) = span predicate xs' in
      (x :: ys, zs)
    else
      ([], xs)

(** Parses the filename from a diff header line. *)
let extract_filename (line: string) : string =
  let parts = String.split_on_char ' ' line in
  let filename_with_prefix = (List.hd (List.rev parts)) in
  let file_name =
    if String.starts_with ~prefix:"b/" filename_with_prefix then
      String.sub filename_with_prefix 2 (String.length filename_with_prefix - 2)
    else
      filename_with_prefix
  in
  file_name

let rec skip predicate xs =
  match xs with
  | [] -> []
  | x :: xs ->
     if predicate x then
       (x :: xs)
     else
       skip predicate xs
