#ifndef MM_PARSER_UTILS
#define MM_PARSER_UTILS
#include <vector>
#include <string>
#include <map>
#include <functional>
#include <iostream>
#include <ostream>
#include <cassert>

namespace mm {

// Missing Features
// * Sensible error messages for Choice
// * Cache results when backtracking
// * Match Regex
// * index <-> (line, column) mapping
// * Policy based whitespace skipping

struct Stream {
  Stream(const char* p, int i, int max) : ptr(p), index(i), bounds(max) {}
  const char *ptr;
  int index;
  int bounds;

  char get(bool ignoreWhiteSpace = true) {
    if (index >= bounds)
      return '\0';
    if (std::isspace(ptr[index]) && ignoreWhiteSpace) {
      index++;
      return get(ignoreWhiteSpace);
    }
    return ptr[index++];
  }

  std::string loop(std::function<bool(char)> Pred) {
    skipWhiteSpace();
    std::string Result;
    while (Pred(ptr[index])) {
      Result += ptr[index];
      index++;
    }
    return Result;
  }

  bool fixed(std::string Str) {
    skipWhiteSpace();
    bool failed = false;
    for (unsigned int i = 0; i < Str.length(); ++i) {
      if (Str[i] != ptr[index+i])
        failed = true;
    }
    if (!failed) {
      index += Str.length();
      return true;
    }
    else return false;
  }

  void skipWhiteSpace() {
    while (index < bounds && std::isspace(ptr[index]))
      index++;
  }

  int getIndex() const {
    return index;
  }

};

class StreamRAII {
public:
  StreamRAII(Stream& in) : in_(in) {
    SavedIndex = in.index;
  }
  ~StreamRAII(){
    if (ShouldRevert) {
      in_.index = SavedIndex;
    }
  }
  void invalidate() {
    ShouldRevert = false;
  }
private:
  bool ShouldRevert = true;
  int SavedIndex;
  Stream& in_;
};

struct SyntaxTree {
  std::string Node;
  std::map<std::string, std::string> Attributes;
  std::vector<SyntaxTree> Children;

  explicit operator bool() const {
    return Node != "";
  }

  std::string error() {
    auto e = Attributes.find("error");
    if (e != Attributes.end()) {
      return e->second;
    } else {
      return "Syntax Error :(";
    }
  }

  void promoteSingleChild() {
    assert(Children.size() == 1);

    auto Child = Children[0];

    for (auto x : Child.Attributes) {
      Attributes.insert(x);
    }

    std::swap(Children, Child.Children);
  }

  void promoteSecondChild() {
    assert(Children.size() == 2);

    auto Child = Children[1];

    for (auto x : Child.Attributes) {
      Attributes.insert(x);
    }
    Children.pop_back();
    for (auto C : Child.Children) {
      Children.push_back(C);
    }
  }

  SyntaxTree getSecondChild() {
    assert(Children.size() >= 2);
    return Children[1];
  }

  SyntaxTree getFirstChild() {
    assert(Children.size() >= 1);
    return Children[0];
  }

  void removeFirstChild() {
    assert(Children.size() >= 1);
    for (int i = 1; i < Children.size(); ++i) {
      Children[i - 1] = Children[i];
    } 
    Children.pop_back();
  }

  void removeSecondChild() {
    assert(Children.size() >= 2);
    for (int i = 2; i < Children.size(); ++i) {
      Children[i - 1] = Children[i];
    } 
    Children.pop_back();
  }

  std::string indent(int indent, std::vector<int> mark) {
    std::string result = "";
    while (indent--) result += "  ";
    if (result.length() >= 2) {
      result[result.length() - 1] = '-';
      result[result.length() - 2] = '\\';
    }
    for (auto m : mark) {
      result[2*m] = '|';
    }
    return result;
  }

  void dump(std::ostream& out, int indent_ = 0, std::vector<int> mark = {}) {
    auto s = indent(indent_, mark);
    out << s << Node << "\n";
    for (auto x : Attributes) {
      out << s <<  "(" << x.first << " : " << x.second << ")\n";
    }
    for (int i = 0; i < Children.size(); ++i) {
      auto C = Children[i];
      if (Children.size() >= 2 && i != Children.size() - 1) mark.push_back(indent_);
      C.dump(out, indent_ + 1, mark);
      if (Children.size() >= 2 && i != Children.size() - 1) mark.pop_back();
    }
  }
};

typedef std::function<SyntaxTree(Stream&)> Action;

SyntaxTree Error(std::string msg, int index = -1) {
  SyntaxTree t = {};
  t.Attributes["error"] = msg;
  if (index != -1) {
    t.Attributes["loc"] = std::to_string(index);
  }
  return t;
}

Action Choice(std::string name, std::vector<Action> Actions) {
  return [name, Actions](Stream& in) {
    for (auto a : Actions) {
      StreamRAII s(in);
      auto t = a(in);
      if (t) {
        s.invalidate();
        return SyntaxTree{name, {}, {t}};
      }
    }
    return Error(name, in.index);
    // TODO: Better error message
  };
}

Action Seq(std::string name, std::vector<Action> Actions) {
  return [name, Actions](Stream& in) {
    StreamRAII s(in);
    SyntaxTree result{name, {}, {}};
    
    for (auto a : Actions) {
      auto t = a(in);
      if (!t) {
        return Error("! " + t.error() + " in " + name, in.index);
      }
      result.Children.push_back(t);
    }
    s.invalidate();
    return result;
  };
};

Action Star(std::string name, Action A) {
  return [name, A](Stream& in) { 
    SyntaxTree result{name, {}, {}};
    while (true) {
      StreamRAII s(in);
      auto t = A(in);
      if (!t) {
        break;
      } else {
        s.invalidate();
        result.Children.push_back(t);
      }
    }
    return result;
  };
}

Action Pr1(Action A) {
  return [A] (Stream& in) {
    auto t = A(in);
    if (t) t.promoteSingleChild();
    return t;
  };
}

Action Pr2(Action A) {
  return [A] (Stream& in) {
    auto t = A(in);
    if (t) t.promoteSecondChild();
    return t;
  };
}

Action R1(Action A) {
  return [A] (Stream& in) {
    auto t = A(in);
    if (t) t = t.getFirstChild(); 
    return t;
  };
}

Action R2(Action A) {
  return [A] (Stream& in) {
    auto t = A(in);
    if (t) t = t.getSecondChild(); 
    return t;
  };
}

Action RM1(Action A) {
 return [A] (Stream& in) {
    auto t = A(in);
    if (t) t.removeFirstChild(); 
    return t;
  }; 
}

Action RM2(Action A) {
 return [A] (Stream& in) {
    auto t = A(in);
    if (t) t.removeSecondChild(); 
    return t;
  }; 
}

Action Plus(std::string name, Action A) {
  auto TreeResult = Seq(name, {A, Star("nest" , A)});
  return Pr2(TreeResult);
}

Action Opt(std::string name, Action A) {
  return [name, A] (Stream& in) {
    SyntaxTree result {name, {}, {}};
    StreamRAII s(in);
    auto t = A(in);
    if (t) {
      s.invalidate();
      result.Children.push_back(t);
    }
    return result;
  };
}

Action Empty(std::string name) {
  return [name] (Stream& in) {
    SyntaxTree t{name, {}, {}};
    return t;
  };
}

// Parse Exact String
Action S(std::string name) {
  return [name] (Stream& in) {
    StreamRAII s(in);
    auto str = in.fixed(name);
    if (str) {
      s.invalidate();
      return SyntaxTree{name, {}, {}};
    } else {
      return Error("Expected " + name, in.getIndex());
    }
  };
}

Action SS(std::string name, std::vector<std::string> choices) {
  std::vector<Action> actions;
  for (auto x : choices) {
    actions.push_back(S(x));
  }
  return Choice(name, actions);
}

// Comma Separated Non-empty List
Action CSL(std::string name, Action A) {
  return Pr2(Seq(name, {A,
            Star("nest1",
            R2(Seq("nest2", {S(","), A})) )}));
}

// Parenthesize (A)
Action P(Action A) {
  return R2(Seq("nest1", {S("("), A , S(")")}));
}

// Parenthesized Comma Separated Non-Empty List
Action PCSL(std::string name, Action A) {
  return P(CSL(name, A));
}

Action PCSLE(std::string name, Action A) {
  return R1(Choice(".", {PCSL(name, A), P(Empty(name))}));
}

// Brace for impact {A}
Action B(Action A) {
  return R2(Seq("nest1", {S("{"), A , S("}")}));
}

// [A]
Action T(Action A) {
  return R2(Seq("nest1", {S("["), A , S("]")}));
}

// <A>
Action A(Action A) {
  return R2(Seq("nest1", {S("<"), A , S(">")}));
}

}

#endif