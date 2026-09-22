;;; simplex-ts-mode.el --- Tree-sitter major mode for Simplex documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Simplex contributors

;; Author: Simplex contributors
;; Keywords: languages, tex, wp
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;; This file is part of Simplex.

;;; Commentary:

;; A major mode for editing Simplex (Simple LaTeX) `.simplex' documents,
;; built on Emacs' built-in tree-sitter support (`treesit', Emacs 29+) and
;; the grammar defined in `tree-sitter-simplex/'.
;;
;; Features:
;;   - Syntax highlighting driven by the tree-sitter grammar, mirroring the
;;     captures in tree-sitter-simplex/queries/highlights.scm.
;;   - Imenu entries for headings, chapters, parts and document properties.
;;   - `outline-minor-mode' integration keyed off Simplex heading markers.
;;   - A comment syntax matching Simplex's `.comment' / `.%' verbatim blocks
;;     is not applicable line-by-line; instead `%'-prefixed lines inside the
;;     source are treated as syntactic comments where it makes sense.
;;   - `M-q' (and `M-x simplex-ts-mode-format-paragraph-or-region') reflow
;;     prose paragraphs, list items, and description items to `fill-column',
;;     while leaving verbatim block bodies, table rows, and declaration
;;     values completely untouched.
;;
;; Installation:
;;
;;   1. Ensure the grammar is installed where Emacs can find it. The
;;      simplest way is to build it from this repository and let Emacs'
;;      default `treesit' lookup path pick it up:
;;
;;        (require 'simplex-ts-mode)
;;        (simplex-ts-mode-install-grammar)
;;
;;      This runs "make install-emacs" in `simplex-ts-mode-source-dir'
;;      (tree-sitter-simplex/ next to this file by default) and copies the
;;      resulting shared library into `simplex-ts-mode-grammar-dir'
;;      (~/.emacs.d/tree-sitter/ by default).
;;
;;      Re-run `simplex-ts-mode-install-grammar' whenever grammar.js or
;;      src/scanner.c change -- Emacs does not rebuild or refresh the
;;      installed grammar on its own, and a stale .so can silently keep old
;;      bugs around (this has previously caused multi-minute hangs and
;;      unbounded memory growth on certain input, including an empty
;;      buffer). `simplex-ts-mode' warns once per buffer if the installed
;;      grammar looks older than the checked-out source.
;;
;;      Alternatively, add an entry to `treesit-language-source-alist' and
;;      run `treesit-install-language-grammar':
;;
;;        (add-to-list
;;         'treesit-language-source-alist
;;         '(simplex "https://example.invalid/tree-sitter-simplex"))
;;
;;      For a local checkout you can instead pass the directory:
;;
;;        (treesit-install-language-grammar 'simplex)
;;
;;      and answer the prompts with the path to `tree-sitter-simplex/'.
;;
;;   2. Load this file and the mode auto-associates with `.simplex' files:
;;
;;        (require 'simplex-ts-mode)

;;; Code:

(require 'treesit)
(require 'subr-x)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup simplex nil
  "Editing support for Simplex (Simple LaTeX) documents."
  :group 'text
  :prefix "simplex-")

(defcustom simplex-ts-mode-grammar-dir
  (expand-file-name "tree-sitter" user-emacs-directory)
  "Directory Emacs' built-in `treesit' loads the Simplex grammar from.

This is where `simplex-ts-mode-install-grammar' installs the shared
library, and where `simplex-ts-mode' looks to warn about a stale grammar.
Matches the default `treesit' lookup path and the `EMACS_TS_DIR' used by
`tree-sitter-simplex/Makefile's `install-emacs' target."
  :type 'directory
  :group 'simplex)

(defcustom simplex-ts-mode-source-dir
  (let* ((this-file (or load-file-name buffer-file-name default-directory))
         ;; Package managers such as straight.el byte-compile this file in
         ;; place, so Emacs may load the resulting .elc from a build
         ;; directory (~/.emacs.d/straight/build/simplex-ts-mode/) that only
         ;; *symlinks* the .el source in -- the .elc itself is a real file
         ;; there, so `file-truename' on it does not lead anywhere useful.
         ;; Resolve to the sibling .el (which is what straight.el symlinks)
         ;; before following symlinks, so we land on the actual checkout
         ;; (e.g. ~/.emacs.d/straight/repos/simplex/emacs/simplex-ts-mode.el)
         ;; where `../tree-sitter-simplex/' is the real grammar directory.
         (el-file (if (string-suffix-p ".elc" this-file)
                      (concat (string-remove-suffix ".elc" this-file) ".el")
                    this-file))
         (el-file (if (file-exists-p el-file) el-file this-file))
         (this-dir (file-name-directory (file-truename el-file))))
    (expand-file-name "../tree-sitter-simplex/" this-dir))
  "Directory of the `tree-sitter-simplex' grammar checkout.

Used by `simplex-ts-mode-install-grammar' (to run \"make install-emacs\"
there) and by the stale-grammar check performed when `simplex-ts-mode' is
enabled.

Derived from the *true* (symlink-resolved) location of this mode's `.el'
source file, so it still finds the grammar when Emacs loads a compiled
`.elc' from a build directory that only symlinks the `.el', as package
managers like straight.el do."
  :type 'directory
  :group 'simplex)

(defcustom simplex-ts-mode-indent-offset 4
  "Preferred size of one Simplex indentation step, in spaces.

Simplex is indentation-significant and block bodies are verbatim, so
`simplex-ts-mode' never re-flows existing block text (see the indentation
section below).  This value is therefore only a hint for editor commands
that insert fresh indentation; it is not used to reformat existing lines."
  :type 'integer
  :safe #'integerp
  :group 'simplex)

;;; Faces --------------------------------------------------------------------

(defface simplex-ts-marker-face
  '((t :inherit font-lock-keyword-face))
  "Face for structural markers (headings, bullets, table markers)."
  :group 'simplex)

(defface simplex-ts-verbatim-face
  '((t :inherit font-lock-string-face))
  "Face for verbatim / special block bodies."
  :group 'simplex)

;;; Font-lock ----------------------------------------------------------------

(defvar simplex-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'simplex
   :feature 'property
   '((property_name) @font-lock-preprocessor-face)

   :language 'simplex
   :feature 'heading
   '((heading (heading_marker) @simplex-ts-marker-face)
     (heading title: (inline_text) @font-lock-function-name-face))

   :language 'simplex
   :feature 'marker
   '((arrow_marker) @simplex-ts-marker-face
     (item_marker) @simplex-ts-marker-face
     (table_marker) @simplex-ts-marker-face
     (table_rule_marker) @simplex-ts-marker-face
     (rule_marker) @simplex-ts-marker-face)

   :language 'simplex
   :feature 'definition
   '((define term: (term) @font-lock-variable-name-face)
     (remark term: (term) @font-lock-variable-name-face))

   :language 'simplex
   :feature 'verbatim
   '((verbatim_marker) @font-lock-keyword-face
     (verbatim body: (block) @simplex-ts-verbatim-face)
     (include_directive) @font-lock-preprocessor-face)

   :language 'simplex
   :feature 'command
   '((command name: (_) @font-lock-builtin-face)
     (command_args) @font-lock-variable-name-face
     (inline_args) @font-lock-variable-name-face)

   :language 'simplex
   :feature 'markup
   '((bold) @bold
     (italic) @italic
     (underline) @underline
     (smallcaps) @italic
     (math (math_content) @font-lock-constant-face)
     (inline_verb (verb_content) @font-lock-string-face))

   :language 'simplex
   :feature 'link
   '((link (link_content) @link)
     (reference (ref_label) @font-lock-constant-face)
     (footnote) @font-lock-doc-face)

   :language 'simplex
   :feature 'symbol
   '((symbol) @font-lock-constant-face
     (escape) @font-lock-escape-face)

   :language 'simplex
   :feature 'delimiter
   :override 'append
   '(["**" "*" "_" "//" "$"] @font-lock-delimiter-face
     ["\\@" "\\#" "\\!" "\\[" "\\{" "\\<" "\\(" "\\^" "\\^^"]
     @font-lock-bracket-face))
  "Tree-sitter font-lock settings for `simplex-ts-mode'.")

;;; Indentation --------------------------------------------------------------

;; Simplex is indentation-significant and the text inside a block is verbatim:
;; a `.#`/`.code` block may quote raw Simplex source with its own multi-level
;; indentation, a `:` description list continues at a deeper indent, and so on.
;; Re-flowing those lines corrupts the document.  Therefore indentation here is
;; deliberately conservative:
;;
;;   - Column-0 lines (markers, bare commands, `@`-properties) stay at column 0.
;;   - Every line that belongs to a block body keeps the author's own
;;     indentation untouched.
;;
;; The grammar exposes an entire indented block (including all of its interior
;; lines) as `block` nodes, so we cannot tell a block's first line from its
;; continuations structurally; the safe, format-preserving choice is to leave
;; all of them exactly as they are.

(defun simplex-ts-mode--keep-indent-anchor (_node _parent bol &rest _)
  "Anchor at the current line's existing indentation.
Returns the buffer position of the first non-whitespace character on the
line beginning at BOL (or BOL itself for a blank/whitespace-only line), so
that a zero offset preserves the line's current indentation."
  (save-excursion
    (goto-char bol)
    (skip-chars-forward " \t")
    (point)))

(defvar simplex-ts-mode--indent-rules
  `((simplex
     ;; Column-0 constructs anchor to the left margin.
     ((parent-is "document") column-0 0)
     ;; Block bodies are verbatim: never re-flow them, keep the author's
     ;; indentation exactly as written.
     ((node-is "block") simplex-ts-mode--keep-indent-anchor 0)
     ((parent-is "block") simplex-ts-mode--keep-indent-anchor 0)
     ;; Fallback: also preserve whatever indentation is already there.
     (catch-all simplex-ts-mode--keep-indent-anchor 0)))
  "Tree-sitter indentation rules for `simplex-ts-mode'.")

;;; Imenu / navigation -------------------------------------------------------

(defun simplex-ts-mode--imenu-heading-name (node)
  "Return an Imenu label for heading NODE."
  (let* ((marker (treesit-node-child-by-field-name node "marker"))
         (title  (treesit-node-child-by-field-name node "title"))
         (prefix (if marker (treesit-node-text marker t) ""))
         (text   (if title (string-trim (treesit-node-text title t)) "")))
    (string-trim (concat prefix " " text))))

(defun simplex-ts-mode--imenu-property-name (node)
  "Return an Imenu label for property NODE."
  (let ((name (treesit-node-child-by-field-name node "name")))
    (if name (treesit-node-text name t) (treesit-node-text node t))))

(defvar simplex-ts-mode--imenu-settings
  `(("Heading" "\\`heading\\'" nil simplex-ts-mode--imenu-heading-name)
    ("Property" "\\`property\\'" nil simplex-ts-mode--imenu-property-name))
  "Imenu configuration for `simplex-ts-mode'.")

;;; Outline ------------------------------------------------------------------

;; Simplex headings begin at column 0 with =, ==, ===, !! or !!!.
(defvar simplex-ts-mode--outline-regexp
  (rx bol (or "!!!" "!!" "===" "==" "=") (any " \t"))
  "Regexp matching Simplex heading lines for `outline-minor-mode'.")

(defun simplex-ts-mode--outline-level ()
  "Return the outline level of the heading on the current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at-p "!!!") 1)   ; part
     ((looking-at-p "!!")  2)   ; chapter
     ((looking-at-p "===") 5)   ; subsubsection
     ((looking-at-p "==")  4)   ; subsection
     ((looking-at-p "=")   3)   ; section
     (t 6))))

;;; Grammar installation / staleness check ------------------------------------

;; The grammar ships as source (tree-sitter-simplex/) and must be compiled to
;; a shared library that Emacs dlopen()s. Emacs never rebuilds or refreshes
;; that library on its own: if grammar.js or src/scanner.c change but the
;; installed .so is not rebuilt, `simplex-ts-mode' keeps running the old,
;; possibly buggy grammar with no indication anything is wrong (this has
;; previously manifested as multi-minute hangs / unbounded memory growth on
;; certain inputs, including something as simple as an empty buffer). These
;; helpers make it easy to keep the installed grammar in sync and to notice
;; when it has drifted.

(defun simplex-ts-mode--grammar-library-path ()
  "Return the path `simplex-ts-mode-install-grammar' installs to, or nil."
  (let ((name (cond ((eq system-type 'darwin) "libtree-sitter-simplex.dylib")
                     ((memq system-type '(windows-nt ms-dos)) "tree-sitter-simplex.dll")
                     (t "libtree-sitter-simplex.so"))))
    (expand-file-name name simplex-ts-mode-grammar-dir)))

;;;###autoload
(defun simplex-ts-mode-install-grammar (&optional callback)
  "Build the Simplex tree-sitter grammar and install it for Emacs.

Runs \"make install-emacs\" in `simplex-ts-mode-source-dir', which compiles
`tree-sitter-simplex/src/{parser,scanner}.c' and copies the resulting
shared library into `simplex-ts-mode-grammar-dir' (Emacs' default
`treesit' lookup path). Call this after pulling or editing the grammar
source so `simplex-ts-mode' picks up the change; the mode does not do
this automatically.

CALLBACK, if given, is called with no arguments once the build finishes
successfully."
  (interactive)
  (unless (file-directory-p simplex-ts-mode-source-dir)
    (user-error "Grammar source not found at %s (set `simplex-ts-mode-source-dir')"
                simplex-ts-mode-source-dir))
  (let ((buf (get-buffer-create "*simplex-ts-mode grammar build*"))
        (default-directory simplex-ts-mode-source-dir))
    (with-current-buffer buf
      (erase-buffer))
    (display-buffer buf)
    (make-process
     :name "simplex-ts-mode-install-grammar"
     :buffer buf
     :command (list "make" "install-emacs"
                     (concat "EMACS_TS_DIR=" simplex-ts-mode-grammar-dir))
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (if (zerop (process-exit-status proc))
             (progn
               (message "simplex-ts-mode: grammar installed to %s"
                        simplex-ts-mode-grammar-dir)
               (when callback (funcall callback)))
           (message "simplex-ts-mode: grammar build failed, see buffer %s"
                     (buffer-name buf))))))))

(defun simplex-ts-mode--grammar-stale-p ()
  "Return non-nil if the installed grammar looks older than its source.

Compares the modification time of the installed shared library (see
`simplex-ts-mode--grammar-library-path') against `grammar.js' and
`src/scanner.c' in `simplex-ts-mode-source-dir'. This is only a heuristic
(mtimes can be misleading, e.g. right after a fresh git clone) but catches
the common case of forgetting to reinstall after editing the grammar."
  (let* ((lib (simplex-ts-mode--grammar-library-path))
         (lib-time (and lib (file-exists-p lib)
                        (file-attribute-modification-time (file-attributes lib))))
         (src-files (seq-filter
                     #'file-exists-p
                     (list (expand-file-name "grammar.js" simplex-ts-mode-source-dir)
                           (expand-file-name "src/scanner.c" simplex-ts-mode-source-dir)))))
    (and lib-time
         src-files
         (seq-some (lambda (f)
                     (time-less-p lib-time (file-attribute-modification-time
                                             (file-attributes f))))
                   src-files))))

(defvar-local simplex-ts-mode--warned-stale nil
  "Non-nil once this buffer has warned about a stale grammar.")

(defun simplex-ts-mode--maybe-warn-stale-grammar ()
  "Warn once if the installed grammar predates its source checkout."
  (when (and (not simplex-ts-mode--warned-stale)
             (file-directory-p simplex-ts-mode-source-dir)
             (simplex-ts-mode--grammar-stale-p))
    (setq simplex-ts-mode--warned-stale t)
    (message (concat "simplex-ts-mode: the installed grammar looks older than "
                      "tree-sitter-simplex/{grammar.js,src/scanner.c}; run "
                      "`M-x simplex-ts-mode-install-grammar' to rebuild it."))))

;;; Paragraph formatting -------------------------------------------------------

;; `simplex-ts-mode-format-paragraph-or-region' reflows prose text to
;; `fill-column', the way `fill-paragraph'/`fill-region' do in `text-mode'.
;; This has to be more careful than plain filling, though: Simplex block
;; bodies are verbatim (see the "Indentation" section above) and reflowing
;; one -- a `.code' listing, a table row, a declaration's raw value -- would
;; corrupt the document exactly the way naive re-indentation used to. So
;; formatting here always asks the tree-sitter parse which construct point
;; (or each paragraph touched by the region) is in, and only reflows
;; constructs whose text is ordinary prose:
;;
;;   - plain paragraphs (`.' or bare indented text)
;;   - list items (`*'/`**'/`+'/`++'/`-'/`--')
;;   - description items (`:'/`::')
;;   - `->' advise items
;;   - `:='/`:-' define/remark text
;;
;; Verbatim bodies, property/include values, table rows, headings, and
;; command lines are left completely untouched.

(defconst simplex-ts-mode--fillable-node-types
  '("paragraph" "item_bullet" "item_number" "description_item"
    "describe_item" "advise_item" "define" "remark")
  "Tree-sitter node types whose text is ordinary prose, safe to reflow.")

(defconst simplex-ts-mode--protected-node-types
  '("verbatim" "property" "include" "table" "table_cell" "table_def"
    "table_caption" "table_rule" "command" "horizontal_rule")
  "Tree-sitter node types whose text must never be reflowed.

Bodies/values here are verbatim (code, raw declaration values, table
cells, ...); reflowing them would corrupt the document the same way naive
re-indentation used to (see the \"Indentation\" section above).")

(defconst simplex-ts-mode--unbreakable-node-types
  '("bold" "italic" "underline" "smallcaps" "math" "inline_verb"
    "link" "reference" "footnote" "symbol" "escape")
  "Tree-sitter node types whose text must never be split across a line break.

None of Simplex's inline markup spans (`**bold**', `*italic*', `$math$',
links, references, footnotes, symbols, ...) can themselves span multiple
lines -- their delimiters are matched with a `[^delim\\n]+' style regexp
in the grammar (see grammar.js). Inserting a line break inside one while
filling would either produce a parse error or, worse, silently
reinterpret the text as something else. See
`simplex-ts-mode--fill-nobreak-p', installed as a `fill-nobreak-predicate'
so `fill-region'/`fill-paragraph' never break inside these spans.")

(defun simplex-ts-mode--fill-nobreak-p ()
  "`fill-nobreak-predicate' member: forbid breaking inside inline markup.

Returns non-nil (meaning \"do not break the line here\") when point is
inside one of `simplex-ts-mode--unbreakable-node-types'."
  (let ((node (treesit-node-at (point))))
    (catch 'unbreakable
      (while node
        (when (member (treesit-node-type node)
                       simplex-ts-mode--unbreakable-node-types)
          (throw 'unbreakable t))
        (setq node (treesit-node-parent node)))
      nil)))

(defun simplex-ts-mode--inline-text-child (node)
  "Return NODE's own `inline_text' descendant on its first line, or nil.

Handles all the shapes the grammar uses:
  - a directly-named `inline:' field (`block_or_line' itself);
  - a `text:' field that IS the `inline_text' (`define', `remark');
  - a `text:' field that is a `block_or_line' one level down
    (`item_bullet', `item_number', `description_item', `describe_item',
    `advise_item') -- descend into its `inline:' field;
  - an anonymous `inline_text' child right under a bare `.'-marker
    `paragraph'."
  (or (treesit-node-child-by-field-name node "inline")
      (let ((text-field (treesit-node-child-by-field-name node "text")))
        (cond
         ((null text-field) nil)
         ((equal (treesit-node-type text-field) "inline_text") text-field)
         ((equal (treesit-node-type text-field) "block_or_line")
          (treesit-node-child-by-field-name text-field "inline"))))
      (let ((n (treesit-node-child node 0)))
        (catch 'found
          (while n
            (when (equal (treesit-node-type n) "inline_text")
              (throw 'found n))
            (setq n (treesit-node-next-sibling n)))
          nil))))

(defun simplex-ts-mode--reflowable-end (node)
  "Return the end of the part of fillable NODE that is safe to reflow.

Only NODE's first-line `inline_text' is ever parsed as inline markup
(bold/italic/links/.../see the \"Paragraph formatting\" section); any
trailing block is opaque, un-sub-parsed text that may itself contain
markup-*looking* characters (e.g. a stray `*') with no tree-sitter node
to protect via `simplex-ts-mode--fill-nobreak-p'. Reflowing such text can
insert a line break in the middle of what looks like -- but is not
represented as -- inline markup, silently changing the rendered document.
So filling never goes past the end of NODE's own `inline_text'; a
trailing block is left completely untouched, like a verbatim body."
  (let ((inline (simplex-ts-mode--inline-text-child node)))
    (if inline (treesit-node-end inline) (treesit-node-end node))))

(defun simplex-ts-mode--protected-at-p (pos)
  "Return non-nil if POS is inside a construct that must not be reflowed."
  (let ((node (treesit-node-at pos)))
    (catch 'protected
      (while node
        (when (member (treesit-node-type node)
                       simplex-ts-mode--protected-node-types)
          (throw 'protected t))
        (setq node (treesit-node-parent node)))
      nil)))

(defun simplex-ts-mode--fillable-node-at (pos)
  "Return the nearest fillable ancestor node at POS, or nil.

Nil is also returned when POS is inside a protected construct (see
`simplex-ts-mode--protected-node-types'), even if a fillable node also
encloses it -- protection always wins."
  (unless (simplex-ts-mode--protected-at-p pos)
    (let ((node (treesit-node-at pos)))
      (while (and node (not (member (treesit-node-type node)
                                     simplex-ts-mode--fillable-node-types)))
        (setq node (treesit-node-parent node)))
      node)))

(defun simplex-ts-mode--paragraph-marker-end (node)
  "Return the buffer position where fillable NODE's own prose text begins.

Prefers `simplex-ts-mode--inline-text-child' so the marker -- including
compound ones like a `define''s `:=  Term: ' -- is identified precisely
via the grammar instead of guessed at with a regexp. Falls back to a
regexp over the first line's leading marker for node types with no
`inline_text' at all (a bare, markerless `paragraph').

Either way, any leading spaces/tabs right at the found position are then
skipped too: `inline_text' can itself start with whitespace (e.g. right
after a bare `.' marker, where the grammar does not treat the following
spaces as `extras'), and callers want the position of the actual first
non-blank character, not wherever the node's span happens to begin."
  (save-excursion
    (let ((inline (simplex-ts-mode--inline-text-child node)))
      (if inline
          (goto-char (treesit-node-start inline))
        (goto-char (treesit-node-start node))
        (beginning-of-line)
        (skip-chars-forward " \t")
        (when (looking-at (rx (or "**" "*" "++" "+" "--" "-" "::" ":" "->" ".")
                               (* (any " \t"))))
          (goto-char (match-end 0)))))
    (skip-chars-forward " \t")
    (point)))

(defun simplex-ts-mode--paragraph-fill-prefix (node)
  "Compute the fill prefix for fillable NODE.

The prefix is as many spaces as `simplex-ts-mode--paragraph-marker-end'
covers on NODE's first line, so continuation lines align under the text
rather than under the marker, matching how the rest of the paragraph is
already indented in Simplex documents (block bodies are indented text;
see the \"Indentation\" section)."
  (save-excursion
    (let ((start (treesit-node-start node)))
      (goto-char start)
      (beginning-of-line)
      (make-string (- (simplex-ts-mode--paragraph-marker-end node) (point)) ?\s))))

;;;###autoload
(defun simplex-ts-mode-format-paragraph-or-region (beg end)
  "Reflow Simplex prose to `fill-column' in the region, or the paragraph at point.

With an active region (BEG..END), reflow every fillable paragraph, list
item, description item, or definition the region touches. Without a
region, reflow just the one at point. Verbatim bodies, table rows,
headings, property/include values, and command lines are left completely
untouched -- see the \"Paragraph formatting\" section above for why.

Interactively, BEG and END come from the region when a region is active;
otherwise both are `point'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (let* ((point-only (= beg end))
         (end-marker (copy-marker end))
         (pos beg)
         (filled 0)
         (looped nil))
    (while (or (< pos (marker-position end-marker))
               (and point-only (not looped)))
      (setq looped t)
      (let ((node (simplex-ts-mode--fillable-node-at pos)))
        (if (null node)
            ;; Not in a fillable construct (e.g. a verbatim body, a table
            ;; row, or plain whitespace between paragraphs): skip past this
            ;; line and keep scanning the rest of the region.
            (setq pos (save-excursion
                        (goto-char pos)
                        (min (marker-position end-marker)
                             (progn (forward-line 1) (point)))))
          (let* ((full-end (treesit-node-end node))
                 (reflow-end (simplex-ts-mode--reflowable-end node))
                 (marker-end (simplex-ts-mode--paragraph-marker-end node))
                 (fill-prefix (simplex-ts-mode--paragraph-fill-prefix node))
                 ;; Track FULL-END through the edit with a marker:
                 ;; `fill-region' may grow or shrink the text, and plain
                 ;; integer positions captured before the edit would
                 ;; otherwise point at the wrong place afterward.
                 (end-tracker (copy-marker full-end)))
            ;; Fill from right after the marker, not from the node's start:
            ;; the marker itself (and its exact trailing whitespace, e.g.
            ;; `.   ' vs. `.  ') is left completely untouched on the first
            ;; line, while continuation lines get FILL-PREFIX. Only fill up
            ;; to REFLOW-END (the node's own first-line `inline_text'), never
            ;; into a trailing block: see
            ;; `simplex-ts-mode--reflowable-end' for why a trailing block
            ;; must stay untouched even though the node it belongs to
            ;; counts as \"fillable\".
            ;;
            ;; Narrow to exactly [MARKER-END, REFLOW-END) first: `fill-region'
            ;; determines paragraph boundaries via `paragraph-start'/
            ;; `paragraph-separate' (here just "blank line", the `text-mode'
            ;; default), which do not know about Simplex's actual grammar
            ;; boundaries. Without narrowing, a fillable node that is not
            ;; followed by a blank line (e.g. a `:=' definition with no text
            ;; immediately followed by a column-0 command) lets `fill-region'
            ;; read past REFLOW-END and merge unrelated following lines into
            ;; the paragraph it fills -- corrupting the document.
            (save-restriction
              (when (< marker-end reflow-end)
                (narrow-to-region marker-end reflow-end)
                (fill-region (point-min) (point-max))))
            (setq filled (1+ filled))
            ;; Always resume strictly after this paragraph's (possibly new)
            ;; end. Using a marker (rather than re-querying the parse tree)
            ;; guarantees forward progress every iteration, no matter how
            ;; the parse tree looks post-edit -- re-querying previously
            ;; could return the *same* node end repeatedly and loop forever
            ;; on documents where a fillable node's bounds happened to stay
            ;; fixed across a no-op fill.
            (setq pos (1+ (marker-position end-tracker)))
            (set-marker end-tracker nil)))))
    (set-marker end-marker nil)
    (message "simplex-ts-mode: reformatted %d paragraph%s"
             filled (if (= filled 1) "" "s"))))

(defun simplex-ts-mode--fill-paragraph-function (&optional _justify)
  "`fill-paragraph-function' for `simplex-ts-mode'.

Delegates to `simplex-ts-mode-format-paragraph-or-region' for the
paragraph at point, so the ordinary `M-q' (`fill-paragraph') binding
respects Simplex's verbatim block bodies. Always returns non-nil so
`fill-paragraph' does not fall back to its default (non-Simplex-aware)
behavior."
  (simplex-ts-mode-format-paragraph-or-region (point) (point))
  t)

;;; Mode ---------------------------------------------------------------------

;;;###autoload
(define-derived-mode simplex-ts-mode text-mode "Simplex"
  "Major mode for editing Simplex (Simple LaTeX) documents.

Powered by the tree-sitter grammar in `tree-sitter-simplex/'.

\\{simplex-ts-mode-map}"
  :group 'simplex
  (unless (treesit-ready-p 'simplex)
    (error "Tree-sitter grammar for `simplex' is not available; \
see `treesit-install-language-grammar'"))

  (treesit-parser-create 'simplex)

  ;; Comments: Simplex has no inline comment syntax, but `%' at column 0 is a
  ;; convenient editor-side line comment and matches the `.%' verbatim block.
  (setq-local comment-start "% ")
  (setq-local comment-start-skip "%+[ \t]*")
  (setq-local comment-end "")

  ;; Indentation.
  (setq-local treesit-simple-indent-rules simplex-ts-mode--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings simplex-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((property heading command)
                (markup verbatim definition link)
                (marker symbol)
                (delimiter)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings simplex-ts-mode--imenu-settings)

  ;; Navigation: treat headings as defuns.
  (setq-local treesit-defun-type-regexp (rx (or "heading" "property")))
  (setq-local treesit-defun-name-function
              (lambda (node)
                (pcase (treesit-node-type node)
                  ("heading" (simplex-ts-mode--imenu-heading-name node))
                  ("property" (simplex-ts-mode--imenu-property-name node))
                  (_ nil))))

  ;; Outline.
  (setq-local outline-regexp simplex-ts-mode--outline-regexp)
  (setq-local outline-level #'simplex-ts-mode--outline-level)

  ;; Paragraph formatting: keep `M-q' (fill-paragraph) and `C-x f'-style
  ;; region filling Simplex-aware, i.e. never reflow verbatim block bodies
  ;; and never break a line inside an inline markup span.
  (setq-local fill-paragraph-function #'simplex-ts-mode--fill-paragraph-function)
  (setq-local fill-nobreak-predicate
              (cons #'simplex-ts-mode--fill-nobreak-p fill-nobreak-predicate))
  (keymap-set simplex-ts-mode-map "M-q" #'simplex-ts-mode-format-paragraph-or-region)

  (treesit-major-mode-setup)

  ;; Best-effort, non-blocking warning if the installed grammar predates the
  ;; checked-out source (see the "Grammar installation" section above).
  (run-with-idle-timer 0 nil #'simplex-ts-mode--maybe-warn-stale-grammar))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.simplex\\'" . simplex-ts-mode))

(provide 'simplex-ts-mode)

;;; simplex-ts-mode.el ends here
