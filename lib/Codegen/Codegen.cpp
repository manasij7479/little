#include "little/Codegen/Codegen.h"
#include <iostream>

namespace little {

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

bool SymbolTable::lookup(std::string name, Type& type) {
  for (auto&& m : table) {
    auto iter = m.find(name);
    if (iter != m.end()) {
      type = iter->second;
      return true;
    }
  }
  return false;
}

void SymbolTable::dump(std::ostream& out) {
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

Codegen::Codegen(mm::SyntaxTree st_): st(st_) {
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

void Codegen::processFunction(SyntaxTree& function) {
        // Codegen for a specific function
  FunctionBeingProcessed = function.Children[1].Attributes["val"];

  auto info = syms.functions[FunctionBeingProcessed];

  auto body = function.Children[3];
  assert(body.Node == "stmtblock");

  syms.newScope();

  for (int i = 1; i < info.size(); ++i) {
    syms.insert(info[i].first, info[i].second);
  }

  processStmtBlock(body);

  syms.popScope();

  // llvm module getOrInsertFunction TODO?
}
void Codegen::processStmtBlock(SyntaxTree& stb) {
  syms.newScope();
  for (auto&& stmt : stb.Children) {
      assert(stmt.Node == "stmt");
      processStmt(stmt);
  }
  syms.popScope();
}
void Codegen::processStmt(SyntaxTree& stmt) {
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

    if (t != Type::t_bool) {
      std::cerr << "Bad while cond " << StringFromType(t) << "\n";
    }
    assert(t == Type::t_bool && "While condition must be boolean");

    processStmt(stmt.Children[1]); // while body
  } else if (st == "for") {
    assert(stmt.Children.size() == 2);

    assert(false && "unimplemented");
    // check for cond, rhs must be array, lhs must be integer

    processStmt(stmt.Children[1]); // for body
  } else if (st == "print") {
    // go through args and check if they are strings or ints
    assert(false && "unimplemented");

  } else if (st == "scall") {
    // type checking for call expressions
    assert(stmt.Children.size() == 1);
    processCall(stmt.Children[0]); // no need to bother about return type

  } else if (st == "assign") {
    // check if type matches
    assert(stmt.Children.size() == 2);
    assert(checkVar(stmt.Children[0]) == processExpr(stmt.Children[1]));

  } else if (st == "arraydecls") {
    // update symbol table
    assert(false && "unimplemented");
  } else if (st == "decls") {
    assert(stmt.Children.size() == 2);
    auto type = TypeFromString(stmt.Children[0].Children[0].Node);

    for (auto id : stmt.Children[1].Children) {
      auto name = id.Attributes["val"];
      syms.insert(name, type);
    }
    // update symbol table
  } else if (st == "store") {
    // check if array exists and index, rhs is integer
    assert(stmt.Children.size() == 2);
    assert(checkVar(stmt.Children[0]) == Type::t_array);
    assert(processExpr(stmt.Children[1]) == Type::t_int);
    assert(processExpr(stmt.Children[2]) == Type::t_int);

  } else if (st == "return") {
    // check if return type matches with function
    auto rett = syms.getFunctionReturnType(FunctionBeingProcessed);
    switch (stmt.Children.size()) {
      case 0 : assert(rett == Type::t_void); break;
      case 1 : assert(rett == processExpr(stmt.Children[0].Children[0])); break;
      default : assert(false && "Functions can return at most one value");
    }
  }
}

Type Codegen::processCall(SyntaxTree& st) {
  assert(st.Children.size() == 2);
  auto name = st.Children[0].Attributes["val"];
  auto args = st.Children[1];
  auto params = syms.functions["name"];
  assert(args.Children.size() == params.size() - 1);
  for (int i = 0; i < args.Children.size(); ++i) {
    auto t = processExpr(args.Children[i]);
    assert(t == params[i + 1].second);
  }
  return syms.getFunctionReturnType(name);
}

Type Codegen::checkVar(SyntaxTree& st) {
  Type t;
  auto b = syms.lookup(st.Attributes["val"], t);
  if (!b) {
    std::cerr << "Lookup : " <<  st.Attributes["val"] << '\t' << b << "\n";
    syms.dump(std::cerr);
  }
  assert(b && "Use of undeclared variable");
  return t;
}

Type Codegen::processExpr(SyntaxTree& expr) { // Might return a llvm::Value* ?
  auto e = expr.Node;
  if (e == "expr") {
    return processExpr(expr.Children[0]);
  }
  if (e == "num") {
    return Type::t_int;
  } if (e == "id") {
    return checkVar(expr);
  } else if (e == "bool") {
    return Type::t_bool;
  } else if (e == "cond") {
    assert(false && "PARSER ISSUE"); // FIXME
  } else if (e == "sizeof") {
    assert(expr.Children.size() == 1);
    assert(processExpr(expr.Children[0]) == Type::t_array);
    return Type::t_int;
  } else if (e == "load") {
    assert(expr.Children.size() == 2);
    assert(checkVar(expr.Children[0]) == Type::t_array);
    assert(checkVar(expr.Children[1]) == Type::t_int);
    return Type::t_int;
  } else if (e == "call") {
    return processCall(expr);
  } else if (e == "binexpr") {
    assert(expr.Children.size() == 3);
    auto op = expr.Children[1].Children[0].Node;
    auto lhstype = processExpr(expr.Children[0]);
    auto rhstype = processExpr(expr.Children[2]);
    if (op == "==" || op == "!=") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_int || lhstype == Type::t_bool);
      return lhstype;
    }
    if (op == "+" || op == "-" || op == "*"
     || op == "^" || op == "/" || op == "%") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_int);
      return Type::t_int;
    }
    if (op == ">=" || op == ">" || op == "<=" || op == "<") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_int);
      return Type::t_bool;
    }
    if (op == "&" || op == "|") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_bool);
      return Type::t_bool;
    } else {
      std::cerr << "Bad Binary operator : " << op << "\n";
      assert(false && "Unexpected binary operator");
    }
  } else if (e == "unaryexpr") {
    assert(expr.Children.size() == 2);
    auto op = expr.Children[0].Node;
    auto opndtype = processExpr(expr.Children[1]);
    if (op == "!") {
      assert(opndtype == Type::t_bool);
      return Type::t_bool;
    } else if (op == "-") {
      assert(opndtype == Type::t_int);
      return Type::t_int;
    } else {
      assert(false && "Unexpected unary operator");
    }
  }

  expr.dump(std::cerr);
  assert(false && "unreachable");
}

}
