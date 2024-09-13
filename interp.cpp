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

void Interpreter::check_vars(std::unordered_set<std::string>& var_set, Node* parent) {
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
  Value result;
  Environment* env = new Environment();
  for (auto it = m_ast->cbegin(); it != m_ast->cend(); ++it) {
    result = execute_node(*env, *it);
  }
  delete env;
  return result;
}

Value Interpreter::execute_node(Environment& env, Node* node) {
  int node_tag = node->get_tag();
  switch (node_tag) {
    case AST_ADD:
      return Value(execute_node(env, node->get_kid(0)).get_ival() + execute_node(env, node->get_kid(1)).get_ival());
    case AST_SUB:
      return Value(execute_node(env, node->get_kid(0)).get_ival() - execute_node(env, node->get_kid(1)).get_ival());
    case AST_MULTIPLY:
      return Value(execute_node(env, node->get_kid(0)).get_ival() * execute_node(env, node->get_kid(1)).get_ival());
    case AST_DIVIDE: {
      int numerator = execute_node(env, node->get_kid(0)).get_ival();
      int denominator = execute_node(env, node->get_kid(1)).get_ival();
      if (denominator == 0) {
        EvaluationError::raise(node->get_loc(),"Division by zero");
      }
      return Value(numerator/denominator);
    }
    case AST_VARREF:
      return env.get_var(node->get_str());
    case AST_INT_LITERAL:
      return Value(std::stoi(node->get_str()));
    case AST_UNIT: {
      Value res;
      for (auto it = node->cbegin(); it != node->cend(); ++it) {
        res = execute_node(env, *it);
      }
      return res;
    }
    case AST_STATEMENT:
      return execute_node(env, node->get_kid(0));
    case AST_VARDEF:
      return env.create_var(node->get_kid(0)->get_str());
    case AST_EQUAL:
      return env.set_var(node->get_kid(0)->get_str(), execute_node(env, node->get_kid(1)).get_ival());
    case AST_OR:
      return Value(execute_node(env, node->get_kid(0)).get_ival() || execute_node(env, node->get_kid(1)).get_ival());
    case AST_AND:
      return Value(execute_node(env, node->get_kid(0)).get_ival() && execute_node(env, node->get_kid(1)).get_ival());
    case AST_LESSER:
      return Value(execute_node(env, node->get_kid(0)).get_ival() < execute_node(env, node->get_kid(1)).get_ival());
    case AST_LESSER_EQUAL:
      return Value(execute_node(env, node->get_kid(0)).get_ival() <= execute_node(env, node->get_kid(1)).get_ival());
    case AST_GREATER:
      return Value(execute_node(env, node->get_kid(0)).get_ival() > execute_node(env, node->get_kid(1)).get_ival());
    case AST_GREATER_EQUAL:
      return Value(execute_node(env, node->get_kid(0)).get_ival() >= execute_node(env, node->get_kid(1)).get_ival());
    case AST_EQUAL_EQUAL:
      return Value(execute_node(env, node->get_kid(0)).get_ival() == execute_node(env, node->get_kid(1)).get_ival());
    case AST_NOT_EQUAL:
      return Value(execute_node(env, node->get_kid(0)).get_ival() != execute_node(env, node->get_kid(1)).get_ival());
    default:
      EvaluationError::raise(node->get_loc(),"Unrecognized node type");
  }
}