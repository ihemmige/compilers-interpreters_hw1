#include "environment.h"

Environment::Environment(Environment *parent)
  : m_parent(parent) {
  assert(m_parent != this);
}

Environment::~Environment() {
}

Value Environment::get_var(std::string var) {
  return Value(m_lookup_table[var]);
}

Value Environment::set_var(std::string var, int value) {
  m_lookup_table[var] = value;
  return Value(value);
}

Value Environment::create_var(std::string var) {
  m_lookup_table[var] = 0;
  return Value(0);
}
