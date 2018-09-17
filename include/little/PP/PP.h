#ifndef LITTLE_PP_H
#define LITTLE_PP_H

#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <iostream>

namespace little {
// removes comments and [\\, \n wp*] pairs
class Preprocessor {
public:
  Preprocessor(std::string filename);
  const char* getPtr() {return data.c_str();};
  int getLength() {return data.length();}
  void printPreprocessed(std::ostream& out) {
    out << data;
  }
private:
  std::string data;

  // TODO : Store line-column
  bool is_empty(std::string str) {
    bool flag = true;
    for (auto c : str) {
      if (!isspace(c)) {
        return false;
      }
    }
    return flag;
  }

};
}
#endif
