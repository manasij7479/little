#include "little/Codegen/Codegen.h"
#include <llvm/IR/Instructions.h>
#include <llvm/Support/TargetSelect.h>
#include <iostream>

namespace little {

uint Width = 64;

Type TypeFromString(std::string in) {
  static std::map<std::string, Type> map = {
    {"int", Type::t_int},
    {"bool", Type::t_bool},
    {"void", Type::t_void},
    {"array", Type::t_array},
    {"str", Type::t_str}
  };
  return map[in];
}

std::string unescape(const std::string& s)
{
  std::string res;
  std::string::const_iterator it = s.begin();
  while (it != s.end())
  {
    char c = *it++;
    if (c == '\\' && it != s.end())
    {
      switch (*it++) {
      case '\\': c = '\\'; break;
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
      // all other escapes
      default:
        // invalid escape sequence - skip it. alternatively you can copy it as is, throw an exception...
        continue;
      }
    }
    res += c;
  }

  return res;
}

llvm::Type* LLVMTypeFromString(std::string in, LLVMContext &C) {
  static std::map<std::string, llvm::Type*> map = {
    {"int", llvm::Type::getIntNTy(C, Width)},
    {"bool", llvm::Type::getInt1Ty(C)},
    {"void", llvm::Type::getVoidTy(C)},
    {"array", getArrayType(C)},
    //{"str", Type::t_str}
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
    case Type::t_str : return "str";
  }
  return "bad_type";
}

llvm::Value *RTToInternal(llvm::Value *V, llvm::IRBuilder<> *B, LLVMContext &C) {
  if (!V->getType()->isIntegerTy(64)) {
    return V;
  }
  if (Width == 64) {
    return V;
  } else if (Width < 64) {
    return B->CreateTrunc(V, llvm::Type::getIntNTy(C, Width));
  } else {
    return B->CreateSExt(V, llvm::Type::getIntNTy(C, Width));
  }
}

llvm::Value *InternalToRT(llvm::Value *V, llvm::IRBuilder<> *B, LLVMContext &C) {
  if (!V->getType()->isIntegerTy(Width)) {
    return V;
  }
  if (Width == 64) {
    return V;
  } else if (Width > 64) {
    return B->CreateTrunc(V, llvm::Type::getIntNTy(C, 64));
  } else {
    return B->CreateSExt(V, llvm::Type::getIntNTy(C, 64));
  }
}

Type GetLittleType (Value *V) {
  if (V->getType()->isIntegerTy(Width)) {
    return Type::t_int;
  } else if (V->getType()->isIntegerTy(1)) {
    return Type::t_bool;
  } else {
    return Type::t_array;
  }
}

Value* SymbolTable::lookup(std::string name) {
  for (auto m_it = table.rbegin(); m_it != table.rend(); ++m_it) {
    auto iter = m_it->find(name);
    if (iter != m_it->end()) {
      return iter->second;
    }
  }
  return nullptr;
}

void SymbolTable::dump(std::ostream& out) {
//   for (auto f : functions) {
//     out << "function " << f.first << "\n";
//     for (auto t : f.second) {
//       out << "(" << t.first << ", " << StringFromType(t.second) << ") ";
//     }
//     out << "\n";
//   }
  for (auto t : table) {
    for (auto p : t) {
      out << "var " << "(" << p.first << ", "
          << StringFromType( GetLittleType (p.second)) << ") \n";
    }
  }
}

std::string MangleName(std::string name) {
  if (name == "main") {
    return "l_main";
  } else if (name == "abort") {
    return "l_abort";
  } else if (name == "exp") {
    return "l_exp";
  } else {
    return name;
  }
}

Codegen::Codegen(mm::SyntaxTree st_, uint IntWidth_): st(st_),
    Builder(TheContext), syms(TheContext, Builder), IntWidth(IntWidth_) {

  Width = IntWidth_; //FIXME: Get rid of global :(

  TheModule = llvm::make_unique<Module>("main", TheContext);

  DeclareRuntimeFunctions();

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

    std::vector<llvm::Type*> argtypes;
    std::vector<std::string> Args;
    auto llvmrettype = LLVMTypeFromString(rettype.Children[0].Node, TheContext);

    assert(llvmrettype != llvm::Type::getIntNPtrTy(TheContext, Width) && "Function return type can not be array");

    for (auto&& arg : args.Children) {
      assert(arg.Node == "decl");
      assert(arg.Children.size() == 2);
      std::pair<std::string, std::string> p = {arg.Children[1].Attributes["val"],
        arg.Children[0].Children[0].Node};
      assert(p.second != "void" && "Function argument can not have void type");
      argtypes.push_back(LLVMTypeFromString(p.second, TheContext));
      Args.push_back(p.first);
    }

    auto FT = FunctionType::get(llvmrettype, argtypes, false);

    auto linkage = Function::InternalLinkage;
    if (name.Attributes["val"] == "main")
      linkage = Function::ExternalLinkage;

    Function *F =
      Function::Create(FT, linkage, MangleName(name.Attributes["val"]), TheModule.get());

  // Set names for all arguments.
    unsigned Idx = 0;
    for (auto &Arg : F->args())
      Arg.setName(Args[Idx++]);

  }

}

void Codegen::processFunction(SyntaxTree& function) {
  // Codegen for a specific function

  auto name = MangleName(function.Children[1].Attributes["val"]);
  FunctionBeingProcessed = TheModule->getFunction(name);

  Function* F = FunctionBeingProcessed;

  auto body = function.Children[3];
  assert(body.Node == "stmtblock");

  syms.newScope();
  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", FunctionBeingProcessed);
  Builder.SetInsertPoint(BB);

  for (Argument& arg : F->args()) {
    syms.insert(arg.getName(), GetLittleType(&arg));
    Builder.CreateStore(&arg, syms.lookup(arg.getName()));
  }
  auto newBB = processStmtBlock(body, "body");
  Builder.SetInsertPoint(BB);
  Builder.CreateBr(newBB.first);
  if (!newBB.second->getTerminator()) {
    if (F->getReturnType() == llvm::Type::getVoidTy(TheContext)) {
      Builder.SetInsertPoint(newBB.second);
      Builder.CreateRetVoid();
    }
    else if (auto I = llvm::dyn_cast<CallInst>(&newBB.second->back())) {
      if (I->getCalledFunction()->getName() == "l_abort") {
        Builder.SetInsertPoint(newBB.second);
        Builder.CreateUnreachable();
      }
    }
  }
  syms.popScope();
}
std::pair<BasicBlock*, BasicBlock*>
  Codegen::processStmtBlock(SyntaxTree& stb, std::string name) {
  assert(stb.Node == "stmtblock");
  syms.newScope();
  BasicBlock *BB = BasicBlock::Create(TheContext, name, FunctionBeingProcessed);
  Builder.SetInsertPoint(BB);
  auto FirstBB = BB;
  for (auto&& stmt : stb.Children) {
    assert(stmt.Node == "stmt");
    BB = processStmt(stmt, BB).second;
    Builder.SetInsertPoint(BB);
  }
  syms.popScope();
  return {FirstBB, BB};
}
std::pair<BasicBlock*, BasicBlock*>
  Codegen::processStmt(SyntaxTree& stmt, BasicBlock* BB) {
  auto st = stmt.Node;
  if (st == "stmt") {
    return processStmt(stmt.Children[0], BB);
  }
  if (st == "else") {
    if (stmt.Children.size() == 1) {
      return processStmt(stmt.Children[0], BB);
    } else {
      return {BB, BB}; // missing else, no op
    }
  }
  if (st == "stmtblock") {
    return processStmtBlock(stmt);
  } else if (st == "if") {
    assert(stmt.Children.size() == 3);
    auto t = processExpr(stmt.Children[0]);
    assert(t->getType()->isIntegerTy(1) && "If condition must be boolean");

    auto ifbb = processStmt(stmt.Children[1], BB); // if body
    ifbb.first->setName("if.true");
    Builder.SetInsertPoint(BB);
    std::pair<BasicBlock *, BasicBlock *> elsebb = {nullptr, nullptr};
    if (stmt.Children[2].Children.size() == 1) {
      elsebb = processStmt(stmt.Children[2].Children[0], BB); // optional else body
      elsebb.first->setName("if.false");
    }
    if (!elsebb.first) {
      elsebb.first = BasicBlock::Create(TheContext, "if.false.empty", FunctionBeingProcessed);
      elsebb.second = elsebb.first;
    }
    Builder.SetInsertPoint(BB);
    Builder.CreateCondBr(t, ifbb.first, elsebb.first);
    BasicBlock* endif = BasicBlock::Create(TheContext, "if.end", FunctionBeingProcessed);

    if (!ifbb.second->getTerminator()) {
      Builder.SetInsertPoint(ifbb.second);
      Builder.CreateBr(endif);
    }
    if (!elsebb.second->getTerminator()) {
      Builder.SetInsertPoint(elsebb.second);
      Builder.CreateBr(endif);
    }
    return {ifbb.first, endif};

  } else if (st == "while") {
    assert(stmt.Children.size() == 2);
    BasicBlock* header = BasicBlock::Create(TheContext, "while.header", FunctionBeingProcessed);
    Builder.SetInsertPoint(header);
    auto t = processExpr(stmt.Children[0]);

    if (!t->getType()->isIntegerTy(1)) {
      std::cerr << "Bad while cond " << "\n";
    }
    assert(t->getType()->isIntegerTy(1) && "While condition must be boolean");

    auto loopbb = processStmt(stmt.Children[1], BB); // while body
    loopbb.first->setName("while.body");

    BasicBlock* endwhile = BasicBlock::Create(TheContext, "while.end", FunctionBeingProcessed);

    Builder.SetInsertPoint(BB);
    Builder.CreateBr(header);
    Builder.SetInsertPoint(header);
    Builder.CreateCondBr(t, loopbb.first, endwhile);
    Builder.SetInsertPoint(loopbb.second);
    Builder.CreateBr(header);
    return {header, endwhile};

  } else if (st == "for") {
    assert(stmt.Children.size() == 2);

    assert(stmt.Children[0].Children.size() == 2);
//     assert(checkVar(stmt.Children[0].Children[0]) == Type::t_int);
//     assert(processExpr(stmt.Children[0].Children[1]) == Type::t_array);
    BasicBlock* preheader = BasicBlock::Create(TheContext, "for.preheader", FunctionBeingProcessed);
    Builder.SetInsertPoint(preheader);

    assert(stmt.Children[0].Children[1].Children.size() == 1);
    auto array = checkVarNoDeref(stmt.Children[0].Children[1].Children[0]);

    if(array->getType()->getPointerElementType() != getArrayType(TheContext)) {
      array->print(llvm::errs());
      llvm::errs() << "\n";
      array->getType()->print(llvm::errs());
      llvm::errs() << "\n";
    }
    assert(array->getType()->getPointerElementType() == getArrayType(TheContext));


    auto i = Builder.CreateAlloca(llvm::Type::getIntNTy(TheContext, Width),
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/Width, 0, /*bool*/false)));

    Builder.CreateStore(llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/Width, 0, /*bool*/false)), i);
    // i = 0

    std::vector<Value*> ArraySizeIdx =
    {
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 1, /*bool*/false))
    };
    auto sizeptr = Builder.CreateGEP(array, ArraySizeIdx);

    BasicBlock* header = BasicBlock::Create(TheContext, "for.header", FunctionBeingProcessed);
    Builder.SetInsertPoint(header);


    auto cond = Builder.CreateICmpSLT(Builder.CreateLoad(i), Builder.CreateLoad(sizeptr));

    syms.newScope();
    syms.insert(stmt.Children[0].Children[0].Attributes["val"], Type::t_int);
    auto v = checkVarNoDeref(stmt.Children[0].Children[0]);
    std::vector<Value*> ArrayMemIdx =
    {
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false))
    };
    auto addr = Builder.CreateGEP(array, ArrayMemIdx);
    auto memptr = Builder.CreateLoad(addr);
    auto targetptr = Builder.CreateGEP(memptr, Builder.CreateLoad(i));

    auto value = Builder.CreateLoad(targetptr);
    // array[i]

    Builder.CreateStore(value, v);
    // v = array[i]


    auto  loopbb = processStmt(stmt.Children[1], header); // for body
    loopbb.first->setName("for.body");


    BasicBlock* inc = BasicBlock::Create(TheContext, "for.inc", FunctionBeingProcessed);
    Builder.SetInsertPoint(inc);

    auto newi = Builder.CreateAdd
      (Builder.CreateLoad(i), llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/Width, 1, /*bool*/false)));

    Builder.CreateStore(newi, i);

    BasicBlock* endfor = BasicBlock::Create(TheContext, "for.end", FunctionBeingProcessed);

    Builder.SetInsertPoint(loopbb.second);
    if (!loopbb.second->getTerminator()) {
      Builder.CreateBr(inc);
    }

    Builder.SetInsertPoint(inc);
    Builder.CreateBr(header);

    Builder.SetInsertPoint(preheader);
    Builder.CreateBr(header);

    Builder.SetInsertPoint(header);
    Builder.CreateCondBr(cond, loopbb.first, endfor);

    Builder.SetInsertPoint(BB);
    Builder.CreateBr(preheader);

    syms.popScope();

    return {preheader, endfor};
  } else if (st == "print") {
    // go through args and check if they are strings or ints
    for (auto arg : stmt.Children[0].Children) {
      auto t = InternalToRT(processExpr(arg), &Builder, TheContext);
      std::vector<Value*> args = {t};
      // t->print(llvm::errs()); llvm::errs() << "\n";
      assert(t->getType()->isIntegerTy(64) || t->getType()->isPointerTy());
      if (t->getType()->isIntegerTy(64)) {
        Builder.CreateCall(TheModule->getFunction("printint"), args);
      } else {
        Builder.CreateCall(TheModule->getFunction("printstring"), args);
      }
    }

    return {BB, BB};

  } else if (st == "scall") {
    // type checking for call expressions
    processCall(stmt); // no need to bother about return type
    return {BB, BB};

  } else if (st == "assign") {
    // check if type matches
    assert(stmt.Children.size() == 2);
    auto v = checkVarNoDeref(stmt.Children[0]);
    auto e = processExpr(stmt.Children[1]);

    assert(v->getType()->getPointerElementType() == e->getType());

    Builder.CreateStore(e, v);
    return {BB, BB};

  } else if (st == "arraydecls") {
    for (auto arraydecl : stmt.Children) {
      assert(arraydecl.Children.size() == 2);
      auto name = arraydecl.Children[0].Attributes["val"];
      auto size = processExpr(arraydecl.Children[1]);
      assert(size->getType()->isIntegerTy(Width));
      syms.insertArray(name, size);
    }
    return {BB, BB};

  } else if (st == "decls") {
    assert(stmt.Children.size() == 2);
    auto type = TypeFromString(stmt.Children[0].Children[0].Node);

    for (auto id : stmt.Children[1].Children) {
      auto name = id.Attributes["val"];
      syms.insert(name, type);
    }
    return {BB, BB};
    // update symbol table
  } else if (st == "store") {
    // check if array exists and index, rhs is integer
    assert(stmt.Children.size() == 3);

    auto v = checkVarNoDeref(stmt.Children[0]);
    auto i = processExpr(stmt.Children[1]);
    auto e = processExpr(stmt.Children[2]);

    assert(i->getType() == llvm::Type::getIntNTy(TheContext, Width));
    assert(e->getType() == llvm::Type::getIntNTy(TheContext, Width));

    std::vector<Value*> ArrayMemIdx =
    {
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false))
    };
    auto addr = Builder.CreateGEP(v, ArrayMemIdx);
    auto memptr = Builder.CreateLoad(addr);
    auto targetptr = Builder.CreateGEP(memptr, i);
    Builder.CreateStore(e, targetptr);

    return {BB, BB};

  } else if (st == "return") {
    // check if return type matches with function
    auto rett = FunctionBeingProcessed->getReturnType();
    switch (stmt.Children[0].Children.size()) {
      case 0 : {
        assert(rett == llvm::Type::getVoidTy(TheContext)); Builder.CreateRetVoid();
        return {BB, BB};
      }
      case 1 : {
        auto t = processExpr(stmt.Children[0].Children[0]);
        assert(rett == t->getType());
        Builder.CreateRet(t);
        return {BB, BB};
      }
      default : assert(false && "Functions can return at most one value");
    }
  }
  std::cerr << "This ->" << st <<"\n";
  assert(false && "Unexpected stmt type");
}

Value* Codegen::processCall(SyntaxTree& st) {
  assert(st.Children.size() == 2);
  auto name = MangleName(st.Children[0].Attributes["val"]);
  auto args = st.Children[1];
  Function *F = TheModule->getFunction(name);
  assert (F && "Use of Undefined function");
  assert(args.Children.size() == F->arg_size());
  int i = 0;
  std::vector<Value*> list;

  bool mustConvert = ! (F->hasInternalLinkage());

  for (Argument& param : F->args()) {
    auto t = processExpr(args.Children[i++]);

    if (mustConvert) {
       t = InternalToRT(t, &Builder, TheContext);
    }

    if (t->getType() != param.getType()) {
      t->getType()->print(llvm::errs());
      param.getType()->print(llvm::errs());
    }
    // llvm::errs() << "HERE " << mustConvert << " " << F->getName() <<"\n";
    // t->getType()->print(llvm::errs());
    // llvm::errs() << "\n";
    // param.getType()->print(llvm::errs());
    // llvm::errs() << "\n";

    assert(t->getType() == param.getType());


    list.push_back(t);
  }
  return Builder.CreateCall(F, list);
}

Value* Codegen::checkVar(SyntaxTree& st) {
  assert(st.Node == "id");
  auto v = syms.lookup(st.Attributes["val"]);
  if (!v) {
    std::cerr << "Lookup : " <<  st.Attributes["val"] << "\n";
    syms.dump(std::cerr);
    st.dump(std::cerr);
  }
  assert(v && "Use of undeclared variable");
  if (llvm::isa<Argument>(v)) {
    return v;
  } else {
    return Builder.CreateLoad(v);
  }
}

Value* Codegen::checkVarNoDeref(SyntaxTree& st) {
  assert(st.Node == "id");
  auto v = syms.lookup(st.Attributes["val"]);
  if (!v) {
    std::cerr << "Lookup : " <<  st.Attributes["val"] << "\n";
    syms.dump(std::cerr);
    st.dump(std::cerr);
  }
  assert(v && "Use of undeclared variable");
  return v;
}

Value* Codegen::processExpr(SyntaxTree& expr) { // Might return a llvm::Value* ?
  auto e = expr.Node;
  if (e == "expr") {
    return processExpr(expr.Children[0]);
  }
  if (e == "num") {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(Width, std::stoi(expr.Attributes["val"]), false));
  } if (e == "id") {
    return checkVar(expr);
  } else if (e == "bool") {
    auto bv = expr.getFirstChild().Node;
    assert (bv == "true" || bv == "false");
    if (bv == "true") {
      return llvm::ConstantInt::get(TheContext, llvm::APInt(1, 1, false));
    }
    if (bv == "false") {
      return llvm::ConstantInt::get(TheContext, llvm::APInt(1, 0, false));
    }
  } else if (e == "cond") {
    auto c = processExpr(expr.Children[0]);
    assert(c->getType()->isIntegerTy(1));
    auto t1 = processExpr(expr.Children[2]);
    auto t2 = processExpr(expr.Children[4]);
    assert(t1->getType() == t2->getType());
    return Builder.CreateSelect(c, t1, t2);
  } else if (e == "sizeof") {
    assert(expr.Children.size() == 1);

    auto v = checkVarNoDeref(expr.Children[0]);

    std::vector<Value*> ArraySizeIdx =
    {
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 1, /*bool*/false))
    };
    auto addr = Builder.CreateGEP(v, ArraySizeIdx);;
    return Builder.CreateLoad(addr);

  } else if (e == "load") {
    assert(expr.Children.size() == 2);

    auto v = checkVarNoDeref(expr.Children[0]);
    auto i = processExpr(expr.Children[1]);

    assert(i->getType() == llvm::Type::getIntNTy(TheContext, Width));

    std::vector<Value*> ArrayMemIdx =
    {
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false))
    };
    auto addr = Builder.CreateGEP(v, ArrayMemIdx);
    auto memptr = Builder.CreateLoad(addr);
    auto targetptr = Builder.CreateGEP(memptr, i);
    return Builder.CreateLoad(targetptr);
  } else if (e == "call") {
    return processCall(expr);
  }  else if (e == "input()") {
    return RTToInternal(Builder.CreateCall(TheModule->getFunction("input")),
      &Builder, TheContext);
  } else if (e == "str") {
    auto str = unescape(expr.Attributes["val"]);
    str.pop_back();
    str.erase(str.begin());
    return Builder.CreateGlobalStringPtr(str);
  } else if (e == "binexpr") {
    assert(expr.Children.size() == 3);
    auto op = expr.Children[1].Children[0].Node;
    auto lhs = processExpr(expr.Children[0]);
    auto rhs = processExpr(expr.Children[2]);
    auto lhstype = GetLittleType(lhs);
    auto rhstype = GetLittleType(rhs);
    if (op == "==" || op == "!=") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_int || lhstype == Type::t_bool);
      if (op == "==") {
        return Builder.CreateICmpEQ(lhs, rhs);
      }
      if (op == "!=") {
        return Builder.CreateICmpNE(lhs, rhs);
      }
    }
    if (op == "+" || op == "-" || op == "*"
     || op == "^" || op == "/" || op == "%") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_int);
      if (op == "+") {
        return Builder.CreateAdd(lhs, rhs);
      }
      if (op == "-") {
        return Builder.CreateSub(lhs, rhs);
      }
      if (op == "*") {
        return Builder.CreateMul(lhs, rhs);
      }
      if (op == "/") {
        return Builder.CreateSDiv(lhs, rhs);
      }
      if (op == "^") {
        std::vector<Value *> args = {lhs, rhs};
        return Builder.CreateCall(TheModule->getFunction("l_exp"), args);
      }
      if (op == "%") {
        return Builder.CreateSRem(lhs, rhs);
      }
    }
    if (op == ">=" || op == ">" || op == "<=" || op == "<") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_int);
      if (op == ">=") {
        return Builder.CreateICmpSGE(lhs, rhs);
      }
      if (op == ">") {
        return Builder.CreateICmpSGT(lhs, rhs);
      }
      if (op == "<=") {
        return Builder.CreateICmpSLE(lhs, rhs);
      }
      if (op == "<") {
        return Builder.CreateICmpSLT(lhs, rhs);
      }
    }
    if (op == "&" || op == "|") {
      assert(lhstype == rhstype);
      assert(lhstype == Type::t_bool);
      if (op == "&") {
        return Builder.CreateAnd(lhs, rhs);
      }
      if (op == "|") {
        return Builder.CreateOr(lhs, rhs);
      }
    } else {
      std::cerr << "Bad Binary operator : " << op << "\n";
      assert(false && "Unexpected binary operator");
    }
  } else if (e == "unaryexpr") {
    assert(expr.Children.size() == 2);
    auto op = expr.Children[0].Children[0].Node;
    auto opnd = processExpr(expr.Children[1]);
    auto opndtype = GetLittleType(opnd);
    if (op == "!") {
      assert(opndtype == Type::t_bool);
      return Builder.CreateNot(opnd);
    } else if (op == "-") {
      assert(opndtype == Type::t_int);
      return Builder.CreateSub(
        llvm::ConstantInt::get(TheContext, llvm::APInt(Width, 0, false)), opnd);
    } else {
      assert(false && "Unexpected unary operator");
    }
  }

  expr.dump(std::cerr);
  assert(false && "unreachable");
  return nullptr;
}

}
