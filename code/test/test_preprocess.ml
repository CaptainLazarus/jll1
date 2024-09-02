open Patch_processor.Diff
open Patch_processor.Preprocess

let test_split_and_strip () =
  let input = "\nline1\n\nline2\nline3\n" in
  let expected_output = ["line1"; "line2"; "line3"] in
  Alcotest.(check (list string)) "splits on newlines" expected_output (split_and_strip input)

let test_read_patch_file () =
  let fixture_path = "./patches/rust_pr.patch" in
  (* Read the expected content directly from the fixture file *)
  let expected_content = 
    let ic = open_in fixture_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in
  
  (* Test the read_patch_file function *)
  let result = read_patch_file fixture_path in
  Alcotest.(check string) "reads file content correctly" expected_content result

let diff_testable =
  let pp_diff fmt diff =
    Format.fprintf fmt "{ file_name = %s; line_no = %d; before_context = %s; after_context = %s; added_lines = %s; removed_lines = %s }"
      diff.file_name diff.line_no diff.before_context diff.after_context diff.added_lines diff.removed_lines
  in
  Alcotest.testable pp_diff (=)

let test_parse_empty_diff () =
  let context = "From 63bdcaa2d92bf5dba4dab23abb4e2b811e565438 Mon Sep 17 00:00:00 2001
From: Ralf Jung <post@ralfj.de>
Date: Wed, 12 Jun 2024 15:19:40 +0200
Subject: [PATCH 1/2] add is_none_or

---
 library/core/src/option.rs | 26 ++++++++++++++++++++++++++
 1 file changed, 26 insertions(+)
" in
  let expected = [] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "empty diff parsed" expected result

let test_parse_header_only_diff () =
  let context = "" in
  let expected = [] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "empty diff parsed" expected result
  
let test_parse_single_diff () =
  let lines = [
    "diff --git a/file1.txt b/file1.txt";
    "@@ -1,3 +1,9 @@";
    "-old line";
    "+new line";
  ] in
  let expected = [
    create_diff
      ~file_name:"file1.txt"
      ~line_no:1
      ~before_context:"\n"
      ~after_context:""
      ~added_lines:"+new line"
      ~removed_lines:"-old line"
  ] in
  let result = extract_diffs lines in
  Alcotest.(check (list diff_testable)) "single diff parsed" expected result

let test_parse_single_diff_only_additions () =
let context = {|From 63bdcaa2d92bf5dba4dab23abb4e2b811e565438 Mon Sep 17 00:00:00 2001
From: Ralf Jung <post@ralfj.de>
Date: Wed, 12 Jun 2024 15:19:40 +0200
Subject: [PATCH 1/2] add is_none_or

---
 library/core/src/option.rs | 26 ++++++++++++++++++++++++++
 1 file changed, 26 insertions(+)

diff --git a/library/core/src/option.rs b/library/core/src/option.rs
index e253cfd2822be..d90b9cdb4b5c8 100644
--- a/library/core/src/option.rs
+++ b/library/core/src/option.rs
@@ -654,6 +654,32 @@ impl<T> Option<T> {
         !self.is_some()
     }
 
+    /// Returns `true` if the option is a [`None`] or the value inside of it matches a predicate.
+    ///
+    /// # Examples
+    ///
+    /// ```
+    /// #![feature(is_none_or)]
+    ///
+    /// let x: Option<u32> = Some(2);
+    /// assert_eq!(x.is_none_or(|x| x > 1), true);
+    ///
+    /// let x: Option<u32> = Some(0);
+    /// assert_eq!(x.is_none_or(|x| x > 1), false);
+    ///
+    /// let x: Option<u32> = None;
+    /// assert_eq!(x.is_none_or(|x| x > 1), true);
+    /// ```
+    #[must_use]
+    #[inline]
+    #[unstable(feature = "is_none_or", issue = "none")]
+    pub fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
+        match self {
+            None => true,
+            Some(x) => f(x),
+        }
+    }
+
     /////////////////////////////////////////////////////////////////////////
     // Adapter for working with references
     /////////////////////////////////////////////////////////////////////////

From 4c208ac233df3b57f48298d5a911354fe8189320 Mon Sep 17 00:00:00 2001
From: Ralf Jung <post@ralfj.de>
Date: Wed, 12 Jun 2024 15:22:23 +0200
Subject: [PATCH 2/2] use is_none_or in some places in the compiler

---|}
    in
  let expected = [
    create_diff
      ~file_name:"library/core/src/option.rs"
      ~line_no:654
      ~before_context:"!self.is_some()\n}\n"
      ~after_context:"
/////////////////////////////////////////////////////////////////////////
// Adapter for working with references
/////////////////////////////////////////////////////////////////////////"
      ~added_lines:{|+    /// Returns `true` if the option is a [`None`] or the value inside of it matches a predicate.
+    ///
+    /// # Examples
+    ///
+    /// ```
+    /// #![feature(is_none_or)]
+    ///
+    /// let x: Option<u32> = Some(2);
+    /// assert_eq!(x.is_none_or(|x| x > 1), true);
+    ///
+    /// let x: Option<u32> = Some(0);
+    /// assert_eq!(x.is_none_or(|x| x > 1), false);
+    ///
+    /// let x: Option<u32> = None;
+    /// assert_eq!(x.is_none_or(|x| x > 1), true);
+    /// ```
+    #[must_use]
+    #[inline]
+    #[unstable(feature = "is_none_or", issue = "none")]
+    pub fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
+        match self {
+            None => true,
+            Some(x) => f(x),
+        }
+    }
+|}
      ~removed_lines:""
  ] in
  let result = extract_diffs (split_and_strip context) in
  Alcotest.(check (list diff_testable)) "single diff parsed" expected result

  

let tests = [
  Alcotest.test_case "Test split_and_strip function" `Quick test_split_and_strip;
  Alcotest.test_case "Test read_patch_file function" `Quick test_read_patch_file;
  Alcotest.test_case "Test empty diff parsing" `Quick test_parse_empty_diff;
  Alcotest.test_case "Test header only diff parsing" `Quick test_parse_header_only_diff;
  Alcotest.test_case "Test single diff parsing" `Quick test_parse_single_diff;
  Alcotest.test_case "Test single diff parsing with only addiitons" `Quick test_parse_single_diff_only_additions;
]
