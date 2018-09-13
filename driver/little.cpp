#include <iostream>
#include "little/Parser/Parser.h"
#include "little/PP/PP.h"
#include <fstream>
#include <sstream>

using namespace little;

int main(int argc, char** argv) {
  assert(argc > 1); // TODO: switch to argparse
  std::string filename = argv[1];

  Preprocessor file(filename);

  Stream in(file.getPtr(), 0, file.getLength() - 1);

  auto st = ParseLittleProgram()(in);

  if (st && in.eof()) {
    if (argc > 2 && std::string(argv[2]) == "--print-ast") {
      st.dump(std::cout);
    }
    return 0;
  } else if (in.eof()) {
    std::cout << in.index << '\t' << file.getLength() - 1 << std::endl;
    std::cerr << "Failed to parse all input.\n";
  } else {
    st.dump(std::cerr);
  }
  return 1;
}
