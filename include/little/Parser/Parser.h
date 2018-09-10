#ifndef LITTLE_PARSER_H
#define LITTLE_PARSER_H
#include "ParserUtils.h"
#include "ParseTokens.h"

namespace little {

auto Id = ParseIdentifier;
auto Num = ParseNumber;
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
    Seq("input", {S("input()")}),
    Seq("load", {Id, T(Expr)}),
    Seq("call", {Id, PCSL("args", Expr)}),
    P(Seq("binexpr", {Expr, Binop, Expr})),
    P(Seq("unaryexpr", {Unaryop, Expr})), Id
  }) (in);
}

SyntaxTree Stmt(Stream& in) {
  return
  Choice("stmt", {
    PSeq("if", {P(Expr), Stmt, Opt("else", PFX("else", Stmt))}),
    PSeq("while", {P(Expr), Stmt}),
    PSeq("for", {P(RM2(Seq("forcond", {Id, S(":") , Expr}))), Stmt}),
    Seq("scall", {Id, PCSL("args", Expr)}),
    B(Star("stmts", Stmt)),
    R2(Seq(".", {S("array"), CSL("arraydecls", Seq("arraydecl", {Id, T(Expr)}))})),
    Seq("decls", {Type, CSL("ids", Id)}),
    PFX("print", PCSL("args", Expr)),
    RM2(Seq("store", {Id, T(Expr), AssignOp, Expr})),
    RM2(Seq("assign", {Id, AssignOp, Expr})),
    // TODO comments, maybe handle as a preprocessing step?
    PFX("return", Opt("returnexpr", Expr)),
  }) (in);
}

Action ParseLittleProgram() {
  auto Function = Seq("function", { Type, Id,
    PCSLE("args", Decl), B(Star("body", Stmt))});
  auto Program = Plus("functions", Function);
  return Program; 
}

}
#endif
