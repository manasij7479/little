#ifndef LITTLE_PARSER_H
#define LITTLE_PARSER_H
#include "ParserUtils.h"

namespace little {
namespace parser {
using mm::SyntaxTree;
using mm::Stream;
using mm::Action;

SyntaxTree Expr(Stream& in);

SyntaxTree Stmt(Stream& in);
SyntaxTree StmtBlock(Stream& in);

SyntaxTree Stmt(Stream& in);

Action ParseLittleProgram();
}

}
#endif
