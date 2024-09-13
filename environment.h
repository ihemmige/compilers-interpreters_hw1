#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include <cassert>
#include <map>
#include <string>
#include "value.h"
#include <unordered_map>

class Environment {
private:
  Environment *m_parent;
  // TODO: representation of environment (map of names to values)
  std::unordered_map<std::string, int> m_lookup_table;

  // copy constructor and assignment operator prohibited
  Environment(const Environment &);
  Environment &operator=(const Environment &);

public:
  Environment(Environment *parent = nullptr);
  ~Environment();

  Value get_var(std::string var);
  Value set_var(std::string var, int value);
  Value create_var(std::string var);
};

#endif // ENVIRONMENT_H
