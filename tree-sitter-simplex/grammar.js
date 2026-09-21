/**
 * @file Tree-sitter grammar for the Simplex (Simple LaTeX) document format.
 *
 * Simplex is line-oriented and indentation-based. This grammar mirrors the
 * lexer/parser in src/Simplex/Parser.hs:
 *
 *   - A line whose first column holds a non-alphabetic character starts a
 *     CONTROL token (e.g. `.`, `=`, `==`, `!!`, `*`, `+`, `-`, `:`, `>@`,
 *     `@title`, `#include`). Control commands live in column 0; the text that
 *     belongs to them is indented on the following lines (a BLOCK).
 *   - A line whose first column is alphabetic is a COMMAND (e.g. `newpage`,
 *     `lipsum 2`, `image zebra.jpg`).
 *   - A BLOCK is the indented text gathered up to the next blank line.
 *   - `@name` + block  -> a global declaration / document property.
 *   - `.name`          -> a special/verbatim block (`.code`, `.math`, ...).
 *   - `>` prefixed     -> table rows and cells.
 *
 * Indentation and blank-line boundaries are resolved by the external scanner
 * in src/scanner.c, which produces the tokens declared in `externals`.
 */

module.exports = grammar({
  name: 'simplex',

  externals: $ => [
    $._block_text,   // an indented text block (one or more indented lines)
    $._block_end,    // blank line / dedent terminating a block
    $._newline,      // significant end of a control/command line
    $._error_sentinel,
  ],

  extras: $ => [/[ \t]/],

  word: $ => $._command_name,

  conflicts: $ => [],

  // The lexer prefers longer anonymous tokens, so `**` beats `*`, `::` beats
  // `:`, `:=`/`:-` beat `:`, `->` beats `-`/`--`, `//` etc. Block-owning rules
  // are right-associative so a trailing block attaches to its introducer.

  rules: {
    document: $ => repeat(
      choice(
        $.property,
        $._block,
        $._blank_line,
      ),
    ),

    _blank_line: $ => $._block_end,

    // ---------------------------------------------------------------------
    // Global declarations: `@name` followed by an indented block.
    // e.g.  @title / @authors / @preamble / @margins 3cm 4cm ...
    // ---------------------------------------------------------------------
    property: $ => prec.right(seq(
      field('name', $.property_name),
      optional(field('args', $.inline_args)),
      $._newline,
      optional(field('value', $.block)),
    )),

    property_name: $ => /@[A-Za-z][A-Za-z0-9_-]*/,

    // Arguments that trail a declaration/control marker on the same line,
    // e.g. `@pagestyle fancy`, `@margins 3cm 4.5cm 3cm 4.5cm`, `= Heading`.
    inline_args: $ => /[^\n]+/,

    // ---------------------------------------------------------------------
    // Blocks introduced by a control marker or a bare command.
    // ---------------------------------------------------------------------
    _block: $ => choice(
      $.heading,
      $.paragraph_arrow,
      $.define,
      $.remark,
      $.advise,
      $.itemize,
      $.enumerate,
      $.description,
      $.describe_items,
      $.verbatim,
      $.table,
      $.horizontal_rule,
      $.include,
      $.command,
      $.paragraph,
    ),

    // Headings: = / == / === (sections) and !! / !!! (chapter / part).
    heading: $ => prec.right(seq(
      field('marker', alias(
        choice('=', '==', '===', '!!', '!!!'),
        $.heading_marker,
      )),
      field('title', $.inline_text),
      $._newline,
      optional($._block_end),
    )),

    // Special arrow paragraphs: => <= <=> =!> <!= <!>
    paragraph_arrow: $ => prec.right(seq(
      field('marker', alias(
        choice('=>', '<=', '<=>', '=!>', '<!=', '<!>'),
        $.arrow_marker,
      )),
      field('text', $.inline_text),
      $._newline,
      optional($.block),
    )),

    // `:=` define paragraph (word: text) and `:-` remark paragraph.
    define: $ => prec.right(seq(
      ':=',
      field('term', $.term),
      optional(seq(':', field('text', $.inline_text))),
      $._newline,
      optional($.block),
    )),

    remark: $ => prec.right(seq(
      ':-',
      field('term', $.term),
      optional(seq(':', field('text', $.inline_text))),
      $._newline,
      optional($.block),
    )),

    term: $ => /(\\:|[^:\n])+/,

    // Advise items: one or more `->` lines.
    advise: $ => prec.right(repeat1($.advise_item)),
    advise_item: $ => seq(
      '->',
      field('text', $.block_or_line),
    ),

    // Itemize: `*` (level 1) and `**` (level 2). Also `-` continues enumerate.
    itemize: $ => prec.right(repeat1($.item_bullet)),
    item_bullet: $ => seq(
      field('marker', alias(choice('*', '**'), $.item_marker)),
      field('text', $.block_or_line),
    ),

    // Enumerate: `+` / `++`, and `-` / `--`.
    enumerate: $ => prec.right(repeat1($.item_number)),
    item_number: $ => seq(
      field('marker', alias(choice('+', '++', '-', '--'), $.item_marker)),
      field('text', $.block_or_line),
    ),

    // Description lists: `:` (single) and `::` (describe items).
    description: $ => prec.right(repeat1($.description_item)),
    description_item: $ => seq(
      ':',
      field('text', $.block_or_line),
    ),

    describe_items: $ => prec.right(repeat1($.describe_item)),
    describe_item: $ => seq(
      '::',
      field('text', $.block_or_line),
    ),

    // ---------------------------------------------------------------------
    // Verbatim / special blocks: `.name` + indented block.
    // Also `.#`, `.@`, `.%`, `.$`, `.!` shortcuts and the bare `.` paragraph.
    // The hashbang form `#name file` loads a file and is treated the same.
    // ---------------------------------------------------------------------
    verbatim: $ => prec.right(seq(
      field('kind', $.verbatim_marker),
      optional(field('args', $.inline_args)),
      $._newline,
      optional(field('body', $.block)),
    )),

    verbatim_marker: $ => token(choice(
      seq('.', /[A-Za-z][A-Za-z0-9$]*/),   // .code, .math, .digraph, .code$ ...
      '.#',
      '.@',
      '.%',
      '.$',
      '.!',
    )),

    // Hashbang include-like directives: #include, #image, #code file ...
    include: $ => prec.right(seq(
      field('directive', $.include_directive),
      optional(field('target', $.inline_args)),
      $._newline,
      optional($.block),
    )),
    include_directive: $ => /#[A-Za-z][A-Za-z0-9$]*/,

    // ---------------------------------------------------------------------
    // Tables. All lines start with `>`.
    // ---------------------------------------------------------------------
    table: $ => prec.right(repeat1($._table_line)),

    _table_line: $ => choice(
      $.table_def,        // >@ / >X
      $.table_caption,    // >^ / >_
      $.table_rule,       // >+ / >- / >=
      $.table_cell,       // > / >! / >$ / >#  and cell specifiers
    ),

    table_def: $ => seq(
      field('marker', alias(choice('>@', '>X'), $.table_marker)),
      field('format', $.inline_text),
      $._newline,
    ),

    table_caption: $ => seq(
      field('marker', alias(choice('>^', '>_'), $.table_marker)),
      field('text', $.inline_text),
      $._newline,
    ),

    table_rule: $ => seq(
      field('marker', alias(choice('>+', '>-', '>='), $.table_rule_marker)),
      $._newline,
    ),

    // A cell: `>` optionally followed by a spec such as `!`, `$`, `#`, `L`,
    // `R`, `C`, digits, colours (`red`), spans (`2,3`), and border bars `|`.
    table_cell: $ => seq(
      '>',
      optional(field('spec', $.cell_spec)),
      field('content', $.block_or_line),
    ),

    // e.g. |2L|, ,4, red, 2yellow|, $, #, !, C
    // Higher precedence than the inline `$`/`#`/`!` delimiters so that a spec
    // immediately after `>` (e.g. `>$`, `>!`, `>#`) is taken as the cell spec
    // rather than opening inline math/verbatim, which otherwise produces a
    // parse error for `>$` cells.
    cell_spec: $ => token.immediate(prec(1, /[^\s\n][^\s\n]*/)),

    // ---------------------------------------------------------------------
    // Plain paragraph: `.` marker or bare indented text.
    // ---------------------------------------------------------------------
    paragraph: $ => prec.right(choice(
      seq('.', optional($.inline_text), $._newline, optional(field('text', $.block))),
      $.block,
    )),

    horizontal_rule: $ => seq(
      alias(token(prec(1, '----')), $.rule_marker),
      $._newline,
    ),

    // ---------------------------------------------------------------------
    // Bare commands (column-0 alphabetic line): `newpage`, `lipsum 2`, ...
    // ---------------------------------------------------------------------
    command: $ => seq(
      field('name', alias($._command_name, $.command_name)),
      optional(field('args', $.command_args)),
      $._newline,
    ),

    _command_name: $ => /[A-Za-z][A-Za-z0-9_-]*/,
    command_args: $ => /[^\n]+/,

    // ---------------------------------------------------------------------
    // Block text (indented, possibly multi-line) vs. a single inline line.
    // ---------------------------------------------------------------------
    block_or_line: $ => prec.right(choice(
      seq(field('inline', $.inline_text), $._newline, optional($.block)),
      $.block,
    )),

    // An indented text block. The external scanner consumes the indented
    // lines as a single opaque token; inline markup inside a block is not
    // sub-parsed (only same-line `inline_text` after a marker is).
    block: $ => prec.right(seq(
      $._block_text,
      optional($._block_end),
    )),

    // ---------------------------------------------------------------------
    // Inline markup used inside text and blocks.
    // ---------------------------------------------------------------------
    inline_text: $ => repeat1($.inline),

    inline: $ => choice(
      $.bold,          // **...**
      $.italic,        // *...*
      $.underline,     // _..._
      $.smallcaps,     // //...//
      $.math,          // $...$
      $.inline_verb,   // \@...@  \#...#  \!...!
      $.escape,        // \x
      $.link,          // \[url desc]  \{url desc}
      $.reference,     // \<label>  \(label)
      $.footnote,      // \^...^  \^^...^
      $.symbol,        // \alpha etc.
      $.text,
      $._stray,        // a lone special char that starts no construct
    ),

    bold: $ => seq('**', alias(/[^*\n]+/, $.text_content), '**'),
    italic: $ => seq('*', alias(/[^*\n]+/, $.text_content), '*'),
    underline: $ => seq('_', alias(/[^_\n]+/, $.text_content), '_'),
    smallcaps: $ => seq('//', alias(/[^\/\n]+/, $.text_content), '//'),
    math: $ => seq('$', alias(/[^$\n]*/, $.math_content), '$'),

    inline_verb: $ => choice(
      seq('\\@', alias(/[^@\n]*/, $.verb_content), '@'),
      seq('\\#', alias(/[^#\n]*/, $.verb_content), '#'),
      seq('\\!', alias(/[^!\n]*/, $.verb_content), '!'),
    ),

    // Footnotes: \^text^  and  \^^url text^
    footnote: $ => choice(
      seq('\\^^', alias(/[^^\n]*/, $.footnote_content), '^'),
      seq('\\^', alias(/[^^\n]*/, $.footnote_content), '^'),
    ),

    // Hyperlinks: \[url desc]  and  \{url desc}
    link: $ => choice(
      seq('\\[', alias(/[^\]\n]*/, $.link_content), ']'),
      seq('\\{', alias(/[^}\n]*/, $.link_content), '}'),
    ),

    // References: \<label>  (\ref) and  \(label)  (\pageref)
    reference: $ => choice(
      seq('\\<', alias(/[^>\n]*/, $.ref_label), '>'),
      seq('\\(', alias(/[^)\n]*/, $.ref_label), ')'),
    ),

    // A LaTeX-style symbol command like \alpha, \Rightarrow, \sum.
    symbol: $ => token(seq('\\', /[A-Za-z]+/)),

    // A backslash escape of a special character. Excludes the characters that
    // open inline constructs (@ # ! [ { < ( ^) so those rules win, and excludes
    // letters (which form `symbol`). Matches e.g. \$ \* \_ \\ \. \, and \ space.
    escape: $ => token(prec(1, seq('\\', /[^A-Za-z@#!\[{<(^\n]/))),

    // A run of ordinary characters that carry no inline meaning. Stops before
    // any character that could begin another inline construct so those rules
    // get a chance to match.
    text: $ => token(prec(-1, /[^*_\/$\\\n]+/)),

    // A single special character that did not begin a valid inline construct
    // (e.g. a stray `*` or `/`). Lowest priority so real constructs win.
    _stray: $ => alias(token(prec(-2, /[*_\/$\\]/)), $.text),
  },
});
