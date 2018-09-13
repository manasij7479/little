#ifndef LITTLE_PARSER_H
#define LITTLE_PARSER_H
#include "ParserUtils.h"
#include "ParseTokens.h"

namespace little {

auto Id = ParseIdentifier;
auto Num = ParseNumber;
auto Str = ParseStringLiteral;
auto Type = SS("type", {"bool", "int", "array", "void"});
auto Binop = SS("binop", {"+", "-", "*", "^", "/", "%", "&", "|", "==", "!=", ">=", ">", "<=", "<"});
auto Unaryop = SS("unaryop", {"!", "-"});
auto AssignOp = S(":=");
auto Decl = Seq("decl", {Type, Id});

SyntaxTree Expr(Stream& in) {
  return
  Choice("expr", {
    SS("bool", {"true", "false"}), Num,
    P(Seq("cond", {Expr, S("?"), Expr, S(":"), Expr})),
    Seq("sizeof", {S("sizeof"), P(Id)}),
    // Seq("input", {S("input()")}),
    Seq("load", {Id, T(Expr)}),
    Seq("call", {Id, PCSLE("args", Expr)}),
    P(Seq("binexpr", {Expr, Binop, Expr})),
    P(Seq("unaryexpr", {Unaryop, Expr})), Id, Str
  }) (in);
}

SyntaxTree Stmt(Stream& in);
SyntaxTree StmtBlock(Stream& in) {
  // {\n <stmt \n>* }\n
  return B(R2(Seq(".", {S("\n"),
    Star("stmtblock", R1(Seq(".", {Stmt, S("\n")})))
  })))(in);
};

SyntaxTree Stmt(Stream& in) {
  return
  Choice("stmt", {
    StmtBlock,
    PSeq("if", {P(Expr), StmtBlock, Opt("else", PFX("else", StmtBlock))}),
    PSeq("while", {P(Expr), StmtBlock}),
    PSeq("for", {P(RM2(Seq("forcond", {Id, S(":") , Expr}))), StmtBlock}),
    PFX("print", PCSL("args", Expr)),
    Seq("scall", {Id, PCSLE("args", Expr)}),
    B(Star("stmts", Stmt)),
    RM2(Seq("assign", {Id, AssignOp, Expr})),
    R2(Seq(".", {S("array"), CSL("arraydecls", Seq("arraydecl", {Id, T(Expr)}))})),
    Seq("decls", {Type, CSL("ids", Id)}),
    PFX("print", PCSL("args", Expr)),
    RM2(Seq("store", {Id, T(Expr), AssignOp, Expr})),

    // TODO comments, maybe handle as a preprocessing step?
    PFX("return", Opt("returnexpr", Expr)), Expr
  }) (in);
}

Action ParseLittleProgram() {
  auto Function =
    Seq("function", { Type, Id, PCSLE("args", Decl), StmtBlock});
  auto Program = Plus("program", N(Function));
  return Program;
}

}
#endif
