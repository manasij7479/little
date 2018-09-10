#ifndef LITTLE_PP_H
#define LITTLE_PP_H

#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <iostream>

namespace little {
// removes comments and [\\, \n] pairs
class Preprocessor {
public:
  Preprocessor(std::string filename) {
    std::ifstream in(filename);
    if (!in) throw std::runtime_error("File " + filename + " not found.");

    std::string line;
    bool comment = false;
    while (std::getline(in, line)) {
      if (comment) {
        line[0] = ';';
        comment = false;
      }
      trim(line);
      if (line[line.length() - 1] == '\\') {
        line[line.length() - 1] = ' ';
        trim(line);
        if (line[0] != ';') {
          data += line;
        } else {
          comment = true;
        }
      }
      else if (line[0] != ';') {
        data += line + "\n";
      }
    }

  }
  const char* getPtr() {return data.c_str();};
  int getLength() {return data.length();}
private:
  std::string data;

  // TODO : Store line-column

  std::string& ltrim(std::string& str, const std::string& chars = "\t\n\v\f\r ") {
      str.erase(0, str.find_first_not_of(chars));
      return str;
  }

  std::string& rtrim(std::string& str, const std::string& chars = "\t\n\v\f\r") {
      str.erase(str.find_last_not_of(chars) + 1);
      return str;
  }

  std::string& trim(std::string& str, const std::string& chars = "\t\n\v\f\r ") {
      return ltrim(rtrim(str, chars), chars);
  }

};
}
#endif
