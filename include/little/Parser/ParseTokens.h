#ifndef LITTLE_PARSE_TOKENS_H
#define LITTLE_PARSE_TOKENS_H

#include "ParserUtils.h"
using namespace mm;
namespace little {
  SyntaxTree ParseIdentifier(Stream& in) {
    bool firstChar = true;
    std::string tok = in.loop([&firstChar](char in) {
      if (firstChar) {
        firstChar = false;
        return (in >= 'a' && in <= 'z') ||
               (in >= 'A' && in <= 'Z') ||
               (in == '_');
      } else {
        return (in >= 'a' && in <= 'z') ||
               (in >= 'A' && in <= 'Z') ||
               (in >= '0' && in <= '9') ||
               (in == '_');
      }
    });
    if (tok != "") {
      return SyntaxTree{"id", {{"val", tok}}, {}};
    } else {
      return Error("Expected Identifier", in.getIndex());
    }
  }

  SyntaxTree ParseNumber(Stream& in) {
    bool firstChar = true;
    StreamRAII s(in);
    std::string tok = in.loop([&firstChar](char in) {
      if (firstChar) {
        firstChar = false;
        return (in >= '0' && in <= '9') ||
               (in == '-');
      } else {
        return (in >= '0' && in <= '9');
      }
    });
    if (tok == "-") {
      return Error("- is not a number", in.getIndex());
    }
    else if (tok != "") {
      s.invalidate();
      return SyntaxTree{"num", {{"val", tok}}, {}};
    } else {
      return Error("Expected Number", in.getIndex());
    }
  }

  SyntaxTree ParseStringLiteral(Stream& in) {
    throw(std::runtime_error("Unimplemented"));
  }
  
}
#endif