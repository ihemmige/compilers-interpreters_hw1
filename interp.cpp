#include <cassert>
#include <algorithm>
#include <memory>
#include "ast.h"
#include "node.h"
#include "exceptions.h"
#include "function.h"
#include "interp.h"
#include <unordered_set>
#include <iostream>
using namespace std;

Interpreter::Interpreter(Node *ast_to_adopt)
  : m_ast(ast_to_adopt) {
}

Interpreter::~Interpreter() {
  delete m_ast;
}

void check_vars(std::unordered_set<std::string>& var_set, Node* parent) {
  for (auto it = parent->cbegin(); it != parent->cend(); ++it) {
    Node* child = *it;
    if (child->get_tag() == AST_VARREF) {
      if (var_set.find(child->get_str()) == var_set.end()) {
        SemanticError::raise(child->get_loc(), "Undefined variable %s", child->get_str().c_str());
      }
    }
    check_vars(var_set, child);
  } 
}

void Interpreter::analyze() {
  std::unordered_set<std::string> var_set;
  for (auto it = m_ast->cbegin(); it != m_ast->cend(); ++it) {
      Node* child_node = *it;
      if (child_node->get_kid(0)->get_tag() == AST_VARDEF) {
        var_set.insert(child_node->get_kid(0)->get_kid(0)->get_str());
      } else { // check for any varrefs 
        check_vars(var_set, child_node);
      }
  }
}


Value Interpreter::execute() {
  // TODO: implement
  Value result;
  return result;
}

