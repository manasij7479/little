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

  ParseLittleProgram()(in).dump(std::cout);

}
