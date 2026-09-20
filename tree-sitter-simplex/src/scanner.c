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
static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

/* Consume the rest of the current line (up to and including the newline). */
static void consume_line(TSLexer *lexer) {
  while (lexer->lookahead != 0 && lexer->lookahead != '\n') {
    advance(lexer);
  }
  if (lexer->lookahead == '\n') {
    advance(lexer);
  }
}

/* Is the current line (from the current column onward) blank? */
static int line_is_blank(TSLexer *lexer) {
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
         lexer->lookahead == '\r') {
    advance(lexer);
  }
  return lexer->lookahead == '\n' || lexer->lookahead == 0;
}

bool tree_sitter_simplex_external_scanner_scan(void *payload, TSLexer *lexer,
                                               const bool *valid_symbols) {
  (void)payload;

  if (valid_symbols[ERROR_SENTINEL]) {
    /* In error recovery let the internal lexer take over. */
    return false;
  }

  /* NEWLINE: end the current control/command line. */
  if (valid_symbols[NEWLINE] && lexer->lookahead == '\n') {
    advance(lexer);
    lexer->result_symbol = NEWLINE;
    return true;
  }

  /* At the very start of a line (column 0). */
  bool at_line_start = lexer->get_column(lexer) == 0;

  /* BLOCK_END: a blank line, or EOF, terminates a block or separates blocks. */
  if (valid_symbols[BLOCK_END]) {
    if (lexer->lookahead == 0) {
      lexer->result_symbol = BLOCK_END;
      return true;
    }
    if (at_line_start) {
      lexer->mark_end(lexer);
      /* Skip whitespace to see whether this line is blank. */
      while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
             lexer->lookahead == '\r') {
        skip(lexer);
      }
      if (lexer->lookahead == '\n') {
        skip(lexer);
        lexer->result_symbol = BLOCK_END;
        lexer->mark_end(lexer);
        return true;
      }
    }
  }

  /* BLOCK_TEXT: indented, non-blank lines gathered into a single block. */
  if (valid_symbols[BLOCK_TEXT] && at_line_start &&
      (lexer->lookahead == ' ' || lexer->lookahead == '\t')) {
    bool consumed_any = false;
    for (;;) {
      /* Peek: is this line indented and non-blank? */
      if (!(lexer->lookahead == ' ' || lexer->lookahead == '\t')) {
        break;
      }
      /* Look ahead past indentation to check for blank line. */
      while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        advance(lexer);
      }
      if (lexer->lookahead == '\n' || lexer->lookahead == 0) {
        /* Blank line ends the block; do not consume it here. */
        break;
      }
      consume_line(lexer);
      consumed_any = true;
      lexer->mark_end(lexer);
      if (lexer->get_column(lexer) != 0) {
        break;
      }
    }
    if (consumed_any) {
      lexer->result_symbol = BLOCK_TEXT;
      return true;
    }
  }

  return false;
}
