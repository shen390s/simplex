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
;;
;; Installation:
;;
;;   1. Ensure the grammar is installed where Emacs can find it.  Either build
;;      and copy the shared object into `treesit-extra-load-path', or add an
;;      entry to `treesit-language-source-alist' and run
;;      `treesit-install-language-grammar':
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

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup simplex nil
  "Editing support for Simplex (Simple LaTeX) documents."
  :group 'text
  :prefix "simplex-")

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

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.simplex\\'" . simplex-ts-mode))

(provide 'simplex-ts-mode)

;;; simplex-ts-mode.el ends here
