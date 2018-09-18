#ifndef LITTLE_CODEGEN_H
#define LITTLE_CODEGEN_H

#include "../Parser/ParserUtils.h"

#include <map>
#include <vector>
#include <cassert>

namespace little {
using mm::SyntaxTree;

enum class Type {t_int, t_bool, t_void, t_array, t_fun};
Type TypeFromString(std::string in);
std::string StringFromType(Type t);

struct SymbolTable {
  void newScope() {
    table.push_back({});
  }
  void popScope() {
    table.pop_back();
  }
  void insert(std::string name, Type t) {
    Type foo;
    if (lookup(name, foo)) {
      assert(false && "shadowing"); // TODO: better error message
    } else {
      table.back()[name] = t;
    }
  }
  bool lookup(std::string name, Type& type);
  std::vector<std::map<std::string, Type>> table;
  std::map<std::string, std::vector<std::pair<std::string, Type>>> functions;
  // Functions don't need a stack because they are all in global scope

  Type getFunctionReturnType(std::string fun) {
    return functions[fun][0].second;
  }

  void dump(std::ostream& out);

};

class Codegen {
public:
  Codegen(mm::SyntaxTree st_);

  bool operator()(std::string filename) { // other llvm options?
    // Pass 2 : Codegen
    for (auto&& function : st.Children) {
      processFunction(function);

    }

    return true;
  }
  void dumpSyms(std::ostream& out) {
    syms.dump(out);
  }
private:
  std::string FunctionBeingProcessed;

  void processFunction(SyntaxTree& function);
  void processStmtBlock(SyntaxTree& stb);
  void processStmt(SyntaxTree& stmt);
  Type processExpr(SyntaxTree& expr);
  Type processCall(SyntaxTree& st);
  Type checkVar(SyntaxTree& st);

  mm::SyntaxTree st;
  SymbolTable syms;
};
}
#endif
