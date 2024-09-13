#ifndef AST_H
#define AST_H

#include "treeprint.h"

// AST node tags
enum ASTKind {
  AST_ADD = 2000,
  AST_SUB,
  AST_MULTIPLY,
  AST_DIVIDE,
  AST_VARREF,
  AST_INT_LITERAL,
  AST_UNIT,
  AST_STATEMENT,
  AST_VARDEF,
  AST_EQUAL, 
  AST_OR,
  AST_AND,
  AST_LESSER,
  AST_LESSER_EQUAL,
  AST_GREATER,
  AST_GREATER_EQUAL,
  AST_EQUAL_EQUAL,
  AST_NOT_EQUAL
};

class ASTTreePrint : public TreePrint {
public:
  ASTTreePrint();
  virtual ~ASTTreePrint();

  virtual std::string node_tag_to_string(int tag) const;
};

#endif // AST_H
