#ifndef TOKEN_H
#define TOKEN_H

// This header file defines the tags used for tokens (i.e., terminal
// symbols in the grammar.)

enum TokenKind {
  TOK_IDENTIFIER,
  TOK_INTEGER_LITERAL,
  TOK_PLUS,
  TOK_MINUS,
  TOK_TIMES,
  TOK_DIVIDE,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_SEMICOLON,
  TOK_VAR,
  TOK_EQUAL,
  TOK_OR,
  TOK_AND,
  TOK_LESSER,
  TOK_LESSER_EQUAL,
  TOK_GREATER,
  TOK_GREATER_EQUAL,
  TOK_EQUAL_EQUAL,
  TOK_NOT_EQUAL
};

#endif // TOKEN_H
