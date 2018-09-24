#ifndef LITTLE_CODEGEN_H
#define LITTLE_CODEGEN_H

#include "../Parser/ParserUtils.h"

#include <map>
#include <vector>
#include <cassert>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

namespace little {
using mm::SyntaxTree;

enum class Type {t_int, t_bool, t_void, t_array, t_fun, t_str};
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

using namespace llvm;

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

  LLVMContext TheContext;
  IRBuilder<> Builder;
  std::unique_ptr<Module> TheModule;
  std::map<std::string, Value *> NamedValues;

};
}
#endif
