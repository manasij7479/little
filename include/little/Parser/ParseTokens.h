#ifndef LITTLE_PARSE_TOKENS_H
#define LITTLE_PARSE_TOKENS_H

#include "ParserUtils.h"
using namespace mm;
namespace little {
  SyntaxTree ParseIdentifier(Stream& in) {
    bool firstChar = true;
    StreamRAII s(in);
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
      s.invalidate();
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
    int state = 0;
    // 0 = on start,
    // 1 = on opening "
    // 2 = on closing "
    StreamRAII s(in);
    std::string result = in.loop([&state](char in) {
      if (state == 0) {
        state = 1;
        return in == '\"';
      } else if (state == 1) {
        if (in == '\"') {
          state = 2;
        }
        return true;
      } else {
        return false;
      }
    });

    if (result != "" && result[0] == '\"' && result[result.length() - 1] == '\"') {
      s.invalidate();
      return SyntaxTree{"str", {{"val", result}}, {}};
    } else {
      return Error("Expected String Literal", in.getIndex());
    }
  }
}
#endif
