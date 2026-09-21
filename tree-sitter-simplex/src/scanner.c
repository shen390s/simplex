/*
 * External scanner for tree-sitter-simplex.
 *
 * Simplex is indentation / blank-line delimited. The core grammar cannot see
 * indentation on its own, so this scanner supplies three tokens:
 *
 *   BLOCK_TEXT : one or more consecutive indented (or non-column-0) lines that
 *                make up the text belonging to a control command. Emitted when
 *                the current line is indented (starts with a space or tab) and
 *                is not empty.
 *   BLOCK_END  : a blank line (or end of file) that terminates a block. Also
 *                emitted as a standalone blank-line separator between blocks.
 *   NEWLINE    : the end of a control/command line (column 0) that carries a
 *                marker and inline arguments, before its indented block.
 *
 * The scanner keeps no persistent state beyond what the parse position gives
 * us, so serialize/deserialize are no-ops.
 */

#include "tree_sitter/parser.h"
#include <wctype.h>

enum TokenType {
  BLOCK_TEXT,
  BLOCK_END,
  NEWLINE,
  ERROR_SENTINEL,
};

void *tree_sitter_simplex_external_scanner_create(void) { return NULL; }
void tree_sitter_simplex_external_scanner_destroy(void *p) { (void)p; }

unsigned tree_sitter_simplex_external_scanner_serialize(void *p, char *buffer) {
  (void)p; (void)buffer;
  return 0;
}

void tree_sitter_simplex_external_scanner_deserialize(void *p, const char *b, unsigned n) {
  (void)p; (void)b; (void)n;
}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

/* Consume the rest of the current line (up to and including the newline). */
static void consume_line(TSLexer *lexer) {
  while (lexer->lookahead != 0 && lexer->lookahead != '\n') {
    advance(lexer);
  }
  if (lexer->lookahead == '\n') {
    advance(lexer);
  }
}

bool tree_sitter_simplex_external_scanner_scan(void *payload, TSLexer *lexer,
                                               const bool *valid_symbols) {
  (void)payload;

  if (valid_symbols[ERROR_SENTINEL]) {
    /* In error recovery let the internal lexer take over. */
    return false;
  }

  /* NEWLINE: end the current control/command line. Also fire at end-of-input
   * so a final control/command line with no trailing newline still closes.
   * Without the EOF case the parser can never reduce such a line and spins,
   * which manifests as a hang / unbounded memory growth. */
  if (valid_symbols[NEWLINE] &&
      (lexer->lookahead == '\n' || lexer->lookahead == 0)) {
    if (lexer->lookahead == '\n') {
      advance(lexer);
    }
    lexer->result_symbol = NEWLINE;
    return true;
  }

  /* Block handling only makes sense at the very start of a line (column 0).
   * We inspect the current line exactly once and decide between a blank line
   * (BLOCK_END) and an indented text block (BLOCK_TEXT).  Both branches read
   * the leading whitespace with `advance` and rely on `mark_end` to control
   * what is actually consumed, so neither destructively discards input the
   * other needs -- the earlier version used `skip` in the BLOCK_END probe,
   * which ate the indentation and stopped BLOCK_TEXT from ever matching. */
  bool at_line_start = lexer->get_column(lexer) == 0;

  if (at_line_start && (valid_symbols[BLOCK_END] || valid_symbols[BLOCK_TEXT])) {
    /* Nothing is consumed yet; the token, if any, starts here. */
    lexer->mark_end(lexer);

    /* Measure leading whitespace without discarding it. */
    bool indented = (lexer->lookahead == ' ' || lexer->lookahead == '\t');
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
           lexer->lookahead == '\r') {
      advance(lexer);
    }

    /* Blank line (only whitespace then newline/EOF): a BLOCK_END separator. */
    if (lexer->lookahead == '\n') {
      if (valid_symbols[BLOCK_END]) {
        advance(lexer);            /* consume the newline */
        lexer->mark_end(lexer);    /* the blank line is consumed */
        lexer->result_symbol = BLOCK_END;
        return true;
      }
      /* BLOCK_END not wanted here; fall through without consuming. */
    } else if (indented && valid_symbols[BLOCK_TEXT]) {
      /* Indented, non-blank: gather this and any following indented, non-blank
       * lines into a single BLOCK_TEXT token. */
      bool consumed_any = false;
      for (;;) {
        /* At this point we are positioned just past the indentation of a
         * non-blank line (lookahead is the first content character). */
        consume_line(lexer);       /* consume through the trailing newline */
        consumed_any = true;
        lexer->mark_end(lexer);

        /* Peek at the next line: continue only if it is indented and not
         * blank.  Use advance (mark_end already fixed the token boundary) so
         * that trailing indentation/blank lines are left for BLOCK_END. */
        if (!(lexer->lookahead == ' ' || lexer->lookahead == '\t')) {
          break;                   /* column-0 line or EOF: block ends here */
        }
        while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
          advance(lexer);
        }
        if (lexer->lookahead == '\n' || lexer->lookahead == 0) {
          break;                   /* blank line: block ends, leave it */
        }
      }
      if (consumed_any) {
        lexer->result_symbol = BLOCK_TEXT;
        return true;
      }
    }
    /* Otherwise: a column-0, non-blank line (a control marker or command).
     * Consume nothing here and let the internal lexer handle it. */
  }

  return false;
}
