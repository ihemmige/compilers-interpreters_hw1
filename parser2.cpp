#include <cassert>
#include <map>
#include <string>
#include <memory>
#include "token.h"
#include "ast.h"
#include "exceptions.h"
#include "parser2.h"

#include <iostream>

////////////////////////////////////////////////////////////////////////
// Parser2 implementation
// This version of the parser builds an AST directly,
// rather than first building a parse tree.
////////////////////////////////////////////////////////////////////////

// This is the grammar (Unit is the start symbol):
//
// Unit -> Stmt
// Unit -> Stmt Unit
// Stmt -> A ;
// E -> T E'
// E' -> + T E'
// E' -> - T E'
// E' -> epsilon
// T -> F T'
// T' -> * F T'
// T' -> / F T'
// T' -> epsilon
// F -> number
// F -> ident
// F -> ( A )
// Stmt → var ident ;
// A    → ident = A
// A    → L
// L    → R || R
// L    → R && R
// L    → R
// R    → E < E
// R    → E <= E
// R    → E > E
// R    → E >= E
// R    → E == E
// R    → E != E
// R    → E

Parser2::Parser2(Lexer *lexer_to_adopt)
  : m_lexer(lexer_to_adopt)
  , m_next(nullptr) {
}

Parser2::~Parser2() {
  delete m_lexer;
}

Node *Parser2::parse() {
  return parse_Unit();
}
Node *Parser2::parse_Unit() {
  // note that this function produces a "flattened" representation
  // of the unit

  std::unique_ptr<Node> unit(new Node(AST_UNIT));
  for (;;) {
    unit->append_kid(parse_Stmt());
    if (m_lexer->peek() == nullptr)
      break;
  }
  return unit.release();
}

Node *Parser2::parse_Stmt() {
  // Stmt -> ^ var ident ;
  // Stmt -> ^ A ;

  std::unique_ptr<Node> s(new Node(AST_STATEMENT));

  Node *next = m_lexer->peek();
  Node *next_next = m_lexer->peek(2);
  if (!next || !next_next) {
    SyntaxError::raise(m_lexer->get_current_loc(), "Unexpected end of input looking for statement");
  }
  if (next->get_tag() == TOK_VAR) { // var ident
    Node* var(expect(TOK_VAR)); // consume var
    var->set_tag(AST_VARDEF);
    var->set_str(""); // for AST visualization
    Node* ident(expect(TOK_IDENTIFIER));
    ident->set_tag(AST_VARREF);
    // s -> var -> ident
    var->append_kid(ident);
    s->append_kid(var);
  } else { // A
    s->append_kid(parse_A());
  }
  
  expect_and_discard(TOK_SEMICOLON);
  return s.release();
}

Node *Parser2::parse_A() {
  // A    → ^ ident = A
  // A    → ^ L

  Node *next = m_lexer->peek();
  Node *next_next = m_lexer->peek(2);

  if (!next || !next_next) {
    SyntaxError::raise(m_lexer->get_current_loc(), "Unexpected end of input looking for statement");
  }

  // ident = A
  if (next->get_tag() == TOK_IDENTIFIER && next_next->get_tag() == TOK_EQUAL) {
    Node* ident(expect(TOK_IDENTIFIER)); // consume identifier
    ident->set_tag(AST_VARREF);
    
    Node* equal(expect(TOK_EQUAL)); // consume =
    equal->set_tag(AST_EQUAL);

    // ident <- equal -> A
    equal->append_kid(ident);
    equal->append_kid(parse_A());
    equal->set_str(""); // for AST visualization
    return equal;
  }
  return parse_L(); // L
}

Node *Parser2::parse_L() {
  // L    → ^ R || R
  // L    → ^ R && R
  // L    → ^ R

  std::unique_ptr<Node> left(parse_R()); // left side of logical operator

  Node *next = m_lexer->peek();
  // if the next token is || or &&
  if (next) {
    if (next->get_tag() == TOK_OR || next->get_tag() == TOK_AND) {
      std::unique_ptr<Node> oper(expect(static_cast<enum TokenKind>(next->get_tag()))); // consume || or &&
      std::unique_ptr<Node> right(parse_R()); // parse right side of logical operator
      
      // left <- oper -> right
      oper->append_kid(left.release());
      oper->append_kid(right.release());
      oper->set_str(""); // for AST visualization
      oper->set_tag(next->get_tag() == TOK_OR ? AST_OR : AST_AND);
      return oper.release();
    }
  }
  return left.release();
}

Node *Parser2::parse_R() {
  // R    → E < E
  // R    → E <= E
  // R    → E > E
  // R    → E >= E
  // R    → E == E
  // R    → E != E
  // R    → E

  std::unique_ptr<Node> left(parse_E()); // left side of relational operator
  Node *next = m_lexer->peek();
  if (!next) {
    return left.release();
  }
  // if the next token is relational operator
  int next_tag = next->get_tag();
  if (next_tag == TOK_LESSER || next_tag == TOK_LESSER_EQUAL || 
      next_tag == TOK_GREATER || next_tag == TOK_GREATER_EQUAL ||
      next_tag == TOK_EQUAL_EQUAL || next_tag == TOK_NOT_EQUAL) {
        std::unique_ptr<Node> oper(expect(static_cast<enum TokenKind>(next_tag))); // consume the relational operator
        // left <- oper -> parse_E()
        oper->append_kid(left.release());
        oper->append_kid(parse_E());
        oper->set_str("");

        // based on the relational operator, set the corresponding AST tag
        switch (next_tag) {
          case TOK_LESSER: 
            oper->set_tag(AST_LESSER); break;
          case TOK_LESSER_EQUAL:
            oper->set_tag(AST_LESSER_EQUAL); break;
          case TOK_GREATER:
            oper->set_tag(AST_GREATER); break;
          case TOK_GREATER_EQUAL:
            oper->set_tag(AST_GREATER_EQUAL); break;
          case TOK_EQUAL_EQUAL:
            oper->set_tag(AST_EQUAL_EQUAL); break;
          case TOK_NOT_EQUAL:
            oper->set_tag(AST_NOT_EQUAL); break;
          default: error_at_current_loc("Unrecognized relational operator");
        } 
        return oper.release();
  }
  return left.release();
}

Node *Parser2::parse_E() {
  // E -> ^ T E'

  // Get the AST corresponding to the term (T)
  Node *ast = parse_T();

  // Recursively continue the additive expression
  return parse_EPrime(ast);
}

// This function is passed the "current" portion of the AST
// that has been built so far for the additive expression.
Node *Parser2::parse_EPrime(Node *ast_) {
  // E' -> ^ + T E'
  // E' -> ^ - T E'
  // E' -> ^ epsilon

  std::unique_ptr<Node> ast(ast_);

  // peek at next token
  Node *next_tok = m_lexer->peek();
  if (next_tok != nullptr) {
    int next_tok_tag = next_tok->get_tag();
    if (next_tok_tag == TOK_PLUS || next_tok_tag == TOK_MINUS)  {
      // E' -> ^ + T E'
      // E' -> ^ - T E'
      std::unique_ptr<Node> op(expect(static_cast<enum TokenKind>(next_tok_tag)));

      // build AST for next term, incorporate into current AST
      Node *term_ast = parse_T();
      ast.reset(new Node(next_tok_tag == TOK_PLUS ? AST_ADD : AST_SUB, {ast.release(), term_ast}));

      // copy source information from operator node
      ast->set_loc(op->get_loc());

      // continue recursively
      return parse_EPrime(ast.release());
    }
  }

  // E' -> ^ epsilon
  // No more additive operators, so just return the completed AST
  return ast.release();
}

Node *Parser2::parse_T() {
  // T -> F T'

  // Parse primary expression
  Node *ast = parse_F();

  // Recursively continue the multiplicative expression
  return parse_TPrime(ast);
}

Node *Parser2::parse_TPrime(Node *ast_) {
  // T' -> ^ * F T'
  // T' -> ^ / F T'
  // T' -> ^ epsilon

  std::unique_ptr<Node> ast(ast_);

  // peek at next token
  Node *next_tok = m_lexer->peek();
  if (next_tok != nullptr) {
    int next_tok_tag = next_tok->get_tag();
    if (next_tok_tag == TOK_TIMES || next_tok_tag == TOK_DIVIDE)  {
      // T' -> ^ * F T'
      // T' -> ^ / F T'
      std::unique_ptr<Node> op(expect(static_cast<enum TokenKind>(next_tok_tag)));

      // build AST for next primary expression, incorporate into current AST
      Node *primary_ast = parse_F();
      ast.reset(new Node(next_tok_tag == TOK_TIMES ? AST_MULTIPLY : AST_DIVIDE, {ast.release(), primary_ast}));

      // copy source information from operator node
      ast->set_loc(op->get_loc());

      // continue recursively
      return parse_TPrime(ast.release());
    }
  }

  // T' -> ^ epsilon
  // No more multiplicative operators, so just return the completed AST
  return ast.release();
}

Node *Parser2::parse_F() {
  // F -> ^ number
  // F -> ^ ident
  // F -> ^ ( E )

  Node *next_tok = m_lexer->peek();
  if (next_tok == nullptr) {
    error_at_current_loc("Unexpected end of input looking for primary expression");
  }

  int tag = next_tok->get_tag();
  if (tag == TOK_INTEGER_LITERAL || tag == TOK_IDENTIFIER) {
    // F -> ^ number
    // F -> ^ ident
    std::unique_ptr<Node> tok(expect(static_cast<enum TokenKind>(tag)));
    int ast_tag = tag == TOK_INTEGER_LITERAL ? AST_INT_LITERAL : AST_VARREF;
    std::unique_ptr<Node> ast(new Node(ast_tag));
    ast->set_str(tok->get_str());
    ast->set_loc(tok->get_loc());
    return ast.release();
  } else if (tag == TOK_LPAREN) {
    // F -> ^ ( A )
    expect_and_discard(TOK_LPAREN);
    std::unique_ptr<Node> ast(parse_A());
    expect_and_discard(TOK_RPAREN);
    return ast.release();
  } else {
    SyntaxError::raise(next_tok->get_loc(), "Invalid primary expression");
  }
}

Node *Parser2::expect(enum TokenKind tok_kind) {
  std::unique_ptr<Node> next_terminal(m_lexer->next());
  if (next_terminal->get_tag() != tok_kind) {
    SyntaxError::raise(next_terminal->get_loc(), "Unexpected token '%s'", next_terminal->get_str().c_str());
  }
  return next_terminal.release();
}

void Parser2::expect_and_discard(enum TokenKind tok_kind) {
  Node *tok = expect(tok_kind);
  delete tok;
}

void Parser2::error_at_current_loc(const std::string &msg) {
  SyntaxError::raise(m_lexer->get_current_loc(), "%s", msg.c_str());
}
