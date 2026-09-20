; Highlights for the Simplex (Simple LaTeX) document format.

; --- Global declarations -----------------------------------------------------
(property_name) @keyword.directive

; --- Headings ----------------------------------------------------------------
(heading (heading_marker) @punctuation.special)
(heading title: (inline_text) @markup.heading)

; --- Structural markers ------------------------------------------------------
(arrow_marker) @punctuation.special
(item_marker) @markup.list
(table_marker) @punctuation.special
(table_rule_marker) @punctuation.special
(rule_marker) @punctuation.special

(define term: (term) @markup.strong)
(remark term: (term) @markup.strong)

; --- Verbatim / special blocks ----------------------------------------------
(verbatim_marker) @keyword
(verbatim body: (block) @markup.raw.block)
(include_directive) @keyword.import

; --- Commands ----------------------------------------------------------------
(command name: (_) @function.builtin)
(command_args) @variable.parameter
(inline_args) @variable.parameter

; --- Inline markup -----------------------------------------------------------
(bold) @markup.strong
(italic) @markup.italic
(underline) @markup.underline
(smallcaps) @markup.italic
(math (math_content) @markup.math)
(inline_verb (verb_content) @markup.raw)

(link (link_content) @markup.link.url)
(reference (ref_label) @markup.link.label)
(footnote) @comment.note

(symbol) @constant.builtin
(escape) @string.escape

; --- Delimiters --------------------------------------------------------------
[
  "**"
  "*"
  "_"
  "//"
  "$"
  "@"
  "#"
  "!"
] @punctuation.delimiter

[
  "\\@"
  "\\#"
  "\\!"
  "\\["
  "\\{"
  "\\<"
  "\\("
  "\\^"
  "\\^^"
] @punctuation.bracket
