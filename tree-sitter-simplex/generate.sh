#!/usr/bin/env bash
# Regenerate the tree-sitter grammar sources (parser.c, grammar.json,
# node-types.json and the tree_sitter/ headers) from grammar.js.
#
# These generated files are committed to the repository so that Emacs'
# `treesit-install-language-grammar' can compile the grammar from a clean
# clone without requiring the tree-sitter CLI.  Because they are committed,
# they MUST stay in sync with grammar.js -- this script (and the pre-commit
# hook that calls it) is what keeps them consistent.
#
# A pinned tree-sitter CLI is used for reproducible output:
#   * inside `nix develop' the tree-sitter on PATH is used;
#   * otherwise the flake's pinned nixpkgs tree-sitter is fetched via nix.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$here/.." && pwd)"

run_generate() {
  ( cd "$here" && tree-sitter generate )
}

if command -v tree-sitter >/dev/null 2>&1; then
  run_generate
elif command -v nix >/dev/null 2>&1; then
  # Use the flake's locked nixpkgs so the CLI version is reproducible.
  nix shell --inputs-from "$repo_root" nixpkgs#tree-sitter \
    --command bash -c "cd '$here' && tree-sitter generate"
else
  echo "generate.sh: need either 'tree-sitter' or 'nix' on PATH" >&2
  exit 1
fi
