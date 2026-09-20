# tree-sitter-simplex

A [tree-sitter](https://tree-sitter.github.io/) grammar for the **Simplex**
(Simple LaTeX) document format used by this repository.

The grammar was derived directly from the reference lexer and parser in
`../src/Simplex/Parser.hs`, together with the inline-markup rules in
`../src/Simplex/EscapeTeX.hs` and the command/declaration tables in
`../src/Simplex/Config.hs`.

## The Simplex model

Simplex is line-oriented and indentation-based. A line whose **first column**
holds a control marker introduces a construct; the text that belongs to it is
**indented** on the following lines and gathered up to the next blank line.

| Syntax | Meaning | Grammar node |
| --- | --- | --- |
| `@name` + block | global declaration / document property | `property` |
| `=`, `==`, `===` | section / subsection / subsubsection | `heading` |
| `!!`, `!!!` | chapter / part | `heading` |
| `=>` `<=` `<=>` `=!>` `<!=` `<!>` | arrow paragraphs | `paragraph_arrow` |
| `:=` | defining paragraph (`word: text`) | `define` |
| `:-` | remark paragraph | `remark` |
| `->` | advise item | `advise` |
| `*`, `**` | itemize (nested) | `itemize` |
| `+`, `++`, `-`, `--` | enumerate | `enumerate` |
| `:` | description list | `description` |
| `::` | describe-items list | `describe_items` |
| `.name` + block | verbatim / special block (`.code`, `.math`, `.digraph`, ...) | `verbatim` |
| `.#` `.@` `.%` `.$` `.!` | verbatim shortcuts | `verbatim` |
| `#include`, `#image`, `#code file` | hashbang include directives | `include` |
| `>`, `>@`, `>X`, `>+`, `>-`, `>=`, `>^`, `>_` | table cells / rows / defs | `table` |
| `.` or bare indented text | paragraph | `paragraph` |
| alphabetic column-0 line | command (`newpage`, `lipsum 2`, `image z.jpg`) | `command` |

### Inline markup

| Syntax | Node |
| --- | --- |
| `**bold**` | `bold` |
| `*italic*` | `italic` |
| `_underline_` | `underline` |
| `//SmallCaps//` | `smallcaps` |
| `$math$` | `math` |
| `\@verb@`, `\#verb#`, `\!verb!` | `inline_verb` |
| `\[url desc]`, `\{url desc}` | `link` |
| `\<label>`, `\(label)` | `reference` |
| `\^note^`, `\^^url note^` | `footnote` |
| `\alpha`, `\Rightarrow`, ... | `symbol` |
| `\$`, `\*`, `\\`, ... | `escape` |

Indentation and blank-line boundaries are handled by the external scanner in
`src/scanner.c`, which emits `BLOCK_TEXT`, `BLOCK_END`, and `NEWLINE` tokens.

## Building

This grammar ships as `grammar.js` plus a C external scanner. Generating the
parser requires the tree-sitter CLI (and a C compiler for the scanner):

```sh
npm install            # installs the tree-sitter CLI locally
npx tree-sitter generate
npx tree-sitter test
npx tree-sitter parse ../examples/fancy-page.simplex
```

Or with a globally installed CLI:

```sh
tree-sitter generate
tree-sitter test
```

> Note: the generated `src/parser.c` and `src/grammar.json` are not checked in;
> run `tree-sitter generate` to produce them.
