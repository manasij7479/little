#include "little/PP/PP.h"

namespace little {

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

Preprocessor::Preprocessor(std::string filename) {
  std::ifstream in(filename);
  if (!in) throw std::runtime_error("File " + filename + " not found.");

  std::vector<std::string> buf;
  bool continued = false;
  for (std::string line; std::getline(in, line);) {
    trim(line);
    auto c_old = continued;
    continued = false;
    if (line.length() > 0 && line.back() == '\\') {
      continued = true;
      line.back() = ' ';
      rtrim(line);
    }
    if (c_old) {
      buf.back() += line;
    } else {
      buf.push_back(line);
    }
  }
  for (auto line : buf) {
    auto comment_loc = line.find_first_of(";");
    if (comment_loc != std::string::npos) {
      line = line.substr(0, comment_loc);
    }
    if (is_empty(line)) {
      continue;
    }
    data += (line + "\n");
  }
}
}
