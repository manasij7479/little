#include <iostream>
#include "little/Parser/Parser.h"
#include <fstream>
#include <sstream>

using namespace little;

int main(int argc, char** argv) {
  assert(argc > 1); // TODO: switch to argparse
  std::string filename = argv[1];

  std::ifstream ifs(filename);

  if (!ifs) {
    std::cerr << "File not found : " << argv[1] << std::endl;
    return 1;
  }

  std::string input;
  input.assign( (std::istreambuf_iterator<char>(ifs) ),
              (std::istreambuf_iterator<char>()));

  Stream in(input.c_str(), 0, input.length() - 1);

  auto Parser = ParseLittleProgram();

  auto st = Parser(in);

  st.dump(std::cout);

}