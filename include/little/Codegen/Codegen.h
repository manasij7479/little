#ifndef LITTLE_CODEGEN_H
#define LITTLE_CODEGEN_H

#include "../Parser/ParserUtils.h"

#include <map>
#include <vector>
#include <cassert>

namespace little {

enum class Type {t_int, t_bool, t_void, t_array, t_fun};
Type TypeFromString(std::string in) {
  static std::map<std::string, Type> map = {
    {"int", Type::t_int},
    {"bool", Type::t_bool},
    {"void", Type::t_void},
    {"array", Type::t_array}
  };
  return map[in];
}
std::string StringFromType(Type t) {
  switch (t) {
    case Type::t_int : return "int";
    case Type::t_bool : return "bool";
    case Type::t_void : return "void";
    case Type::t_array : return "array";
    case Type::t_fun : return "function";
  }
}

struct SymbolTable {
  void newScope() {
    table.push_back({});
  }
  void popScope() {
    table.pop_back();
  }
  bool lookup(std::string name, Type& type) {
    for (auto&& m : table) {
      auto iter = m.find(name);
      if (iter != m.end()) {
        type = iter->second;
      }
    }
    return false;
  }
  std::vector<std::map<std::string, Type>> table;
  std::map<std::string, std::vector<std::pair<std::string, Type>>> functions;
  // Functions don't need a stack because they are all in global scope


  void dump(std::ostream& out) {
    for (auto f : functions) {
      out << "function " << f.first << "\n";
      for (auto t : f.second) {
        out << "(" << t.first << ", " << StringFromType(t.second) << ") ";
      }
      out << "\n";
    }
    for (auto t : table) {
      for (auto p : t) {
        out << "var " << "(" << p.first << ", " << StringFromType(p.second) << ") \n";
      }
    }
  }

};

class Codegen {
public:
  Codegen(mm::SyntaxTree st_): st(st_) {
    assert(st.Node == "program" && "Syntax Tree Root missing");

    // Pass 1 : Store name and type of functions
    for (auto&& child : st.Children) {
      assert(child.Node == "function" && "Not a function");
      assert(child.Children.size() == 4 && "Malformed function");
      auto rettype = child.Children[0];
      assert(rettype.Node == "type");
      auto name = child.Children[1];
      assert(name.Node == "id");
      auto args = child.Children[2];
      assert(args.Node == "args");
      // no need to process body here

      std::vector<std::pair<std::string, Type>> argtypes;
      std::pair<std::string, Type> p = {std::string("#ret"), TypeFromString(rettype.Children[0].Node)};
      argtypes.push_back(p);

      assert(argtypes[0].second != Type::t_array && "Function return type can not be array");

      for (auto&& arg : args.Children) {
        assert(arg.Node == "decl");
        assert(arg.Children.size() == 2);
        std::pair<std::string, Type> p = {arg.Children[1].Attributes["val"],
          TypeFromString(arg.Children[0].Children[0].Node)};
        assert(p.second != Type::t_void && "Function argument can not have void type");
        argtypes.push_back(p);
      }

      assert(argtypes.size() > 0 && "Function must have a return type"); // must have return type
      syms.functions[name.Attributes["val"]] = argtypes;
    }

  }
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

  void processFunction(SyntaxTree& function) {
          // Codegen for a specific function
    FunctionBeingProcessed = function.Children[1].Attributes["val"];

    auto info = syms.functions[FunctionBeingProcessed];

    auto body = function.Children[3];
    assert(body.Node == "stmtblock");

    processStmtBlock(body);

    // llvm module getOrInsertFunction TODO?
  }
  void processStmtBlock(SyntaxTree& stb) {
    for (auto&& stmt : stb.Children) {
        assert(stmt.Node == "stmt");
        processStmt(stmt);
    }
  }
  void processStmt(SyntaxTree& stmt) {
    auto st = stmt.Node;
    if (st == "stmt") {
      processStmt(stmt.Children[0]);
      return;
    }
    if (st == "stmtblock") {
      processStmtBlock(stmt);
    } else if (st == "if") {
      assert(stmt.Children.size() == 3);
      auto t = processExpr(stmt.Children[0]);
      assert(t == Type::t_bool && "If condition must be boolean");

      processStmt(stmt.Children[1]); // if body
      if (stmt.Children[1].Children.size() == 1) {
        processStmt(stmt.Children[1].Children[0]); // optional else body
      }
    } else if (st == "while") {
      assert(stmt.Children.size() == 2);
      auto t = processExpr(stmt.Children[0]);
      assert(t == Type::t_bool && "While condition must be boolean");

      processStmt(stmt.Children[1]); // while body
    } else if (st == "for") {
      assert(stmt.Children.size() == 2);

      assert(false && "unimplemented");

      processStmt(stmt.Children[1]); // for body
    } else if (st == "print") {
      // go through args anc check if they are strings or ints
    } else if (st == "scall") {
      // type checking for call expressions
    } else if (st == "assign") {
      // check if type matches
    } else if (st == "arraydecls") {
      // update symbol table
    } else if (st == "decls") {
      // update symbol table
    } else if (st == "store") {
      // check if array exists and rhs is integer
    } else if (st == "return") {
      // check if return type matches with function
    }
    assert(false && "unimplemented");
  }

  Type processExpr(SyntaxTree& expr) { // Might return a llvm::Value* ?
    assert(false && "unimplemented");
    return Type::t_void;
  }
  mm::SyntaxTree st;
  SymbolTable syms;
};
}
#endif
