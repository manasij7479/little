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
using namespace llvm;

enum class Type {t_int, t_bool, t_void, t_array, t_fun, t_str};
Type TypeFromString(std::string in);
std::string StringFromType(Type t);

llvm::Type *getArrayType(LLVMContext &Ctx) {
  static std::vector<llvm::Type*> types =
  {llvm::Type::getInt64PtrTy(Ctx), llvm::Type::getInt64Ty(Ctx)};
  return llvm::StructType::create(Ctx, types);
}


struct SymbolTable {
  SymbolTable(LLVMContext &TC, IRBuilder<> &B) : TheContext(TC), Builder(B) {};
  void newScope() {
    table.push_back({});
  }
  void popScope() {
    table.pop_back();
  }
  void insert(std::string name, Type t) {
    if (lookup(name)) {
      assert(false && "shadowing"); // TODO: better error message
    } else {
      Value* V = nullptr;
      if (t == Type::t_int) {
       V = Builder.CreateAlloca(
         llvm::Type::getInt64Ty(TheContext),
         llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 1, /*bool*/false)),
         name);
      } else if (t == Type::t_bool) {
        V = Builder.CreateAlloca(
         llvm::Type::getInt1Ty(TheContext),
         llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 1, /*bool*/false)),
         name);
      }
      assert (V && "unimplemented type");
      table.back()[name] = V;
    }
  }
  void insertArray(std::string name, Value *Size) {
    if (lookup(name)) {
      assert(false && "shadowing"); // TODO: better error message
    } else {
      Value *V = Builder.CreateAlloca(
         getArrayType(TheContext),
         llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 1, /*bool*/false)),
         name);

      Value* Mem = Builder.CreateAlloca(llvm::Type::getInt64Ty(TheContext), Size);

      std::vector<Value*> ArrayMemIdx =
      {
        llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
        llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false))
      };
      std::vector<Value*> ArraySizeIdx =
      {llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 0, /*bool*/false)),
        llvm::ConstantInt::get(TheContext, llvm::APInt(/*nbits*/32, 1, /*bool*/false))
      };

      Value *MemAddr = Builder.CreateGEP(V, ArrayMemIdx);
      Value *SizeAddr = Builder.CreateGEP(V, ArraySizeIdx);

      Builder.CreateStore(Mem, MemAddr);
      Builder.CreateStore(Size, SizeAddr);

      table.back()[name] = V;
    }
  }
  void insert(std::string name, llvm::Value *V) {
    table.back()[name] = V;
  }
  void overwrite(std::string name, llvm::Value *V) {
    for (auto m_it = table.rbegin(); m_it != table.rend(); ++m_it) {
      auto iter = m_it->find(name);
      if (iter != m_it->end()) {
        iter->second = V;
      }
    }
  }
  Value* lookup(std::string name);
  std::vector<std::map<std::string, Value*>> table;
  std::map<std::string, Function*> functions;
  // Functions don't need a stack because they are all in global scope

  llvm::Type *getFunctionReturnType(std::string fun) {
    return functions[fun]->getReturnType();
  }

  void dump(std::ostream& out);

  LLVMContext &TheContext;
  IRBuilder<> &Builder;
};

class Codegen {
public:
  Codegen(mm::SyntaxTree st_);

  bool operator()(std::string filename) { // other llvm options?
    // Pass 2 : Actual Codegen

    for (auto&& function : st.Children) {
      processFunction(function);
      assert(!verifyFunction(*FunctionBeingProcessed, &llvm::errs()));
    }
    assert(!verifyModule(*TheModule, &llvm::errs()));
    TheModule->print(errs(), nullptr);
    return true;
  }
  void dumpSyms(std::ostream& out) {
    syms.dump(out);
  }
private:
  Function *FunctionBeingProcessed;

  void processFunction(SyntaxTree& function);
  BasicBlock* processStmtBlock(SyntaxTree& stb, std::string name = "");
  BasicBlock* processStmt(SyntaxTree& stmt, BasicBlock* BB);
  Value* processExpr(SyntaxTree& expr);
  Value* processCall(SyntaxTree& st);
  Value* checkVar(SyntaxTree& st);
  Value* checkVarNoDeref(SyntaxTree& st);

  void DeclareRuntimeFunctions() {
    std::vector<llvm::Type*> args = {};
    auto FT = FunctionType::get(llvm::Type::getInt64Ty(TheContext), args, false);
    Function::Create(FT, Function::ExternalLinkage, "input", TheModule.get());

    args = {llvm::Type::getInt64Ty(TheContext)};
    FT = FunctionType::get(llvm::Type::getVoidTy(TheContext), args, false);
    Function::Create(FT, Function::ExternalLinkage, "printint", TheModule.get());

    args = {llvm::Type::getInt8PtrTy(TheContext)};
    FT = FunctionType::get(llvm::Type::getVoidTy(TheContext), args, false);
    Function::Create(FT, Function::ExternalLinkage, "printstring", TheModule.get());

    args = {llvm::Type::getInt64Ty(TheContext)};
    FT = FunctionType::get(llvm::Type::getInt64PtrTy(TheContext), args, false);
    Function::Create(FT, Function::ExternalLinkage, "heapalloc", TheModule.get());

  }

  mm::SyntaxTree st;
  SymbolTable syms;

  LLVMContext TheContext;
  IRBuilder<> Builder;
  std::unique_ptr<Module> TheModule;
  std::map<std::string, Value *> NamedValues;

};
}
#endif
