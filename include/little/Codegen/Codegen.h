#ifndef LITTLE_CODEGEN_H
#define LITTLE_CODEGEN_H

#include "../Parser/ParserUtils.h"

#include <map>
#include <vector>
#include <cassert>

namespace little {

enum class Type {t_int, t_bool, t_void, t_array, t_fun};

struct IdInfo {
  Type type;
};

struct SymbolTable {
  void newScope() {
    table.push_back({});
  }
  void popScope() {
    table.pop_back();
  }
  bool lookup(std::string name, IdInfo& info) {
    for (auto&& m : table) {
      auto iter = m.find(name); 
      if (iter != m.end()) {
        info = iter->second;
      }
    }
    return false;
  }
  std::vector<std::map<std::string, IdInfo>> table;
  std::map<std::string, std::vector<IdInfo>> functions;
  // Functions don't need a stack because they are all in global scope
};
  
class Codegen {
public:
  Codegen(mm::SyntaxTree st_): st(st_) {
    assert(st.Node == "program");
    for (auto&& child : st.Children) {
      assert(child.Node == "function");
    }
  }
  bool operator()(std::string filename) { // other llvm options?
    return true;
  }
private:
  mm::SyntaxTree st;
  SymbolTable syms;
};
}
#endif
