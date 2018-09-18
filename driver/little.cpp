#include <iostream>
#include "little/Parser/Parser.h"
#include "little/PP/PP.h"
#include "little/Codegen/Codegen.h"
#include <fstream>
#include <sstream>
#include <set>

using namespace little;


int main(int argc, char** argv) {
  assert(argc > 1); // TODO: switch to argparse
  std::string filename = argv[1];

  Preprocessor file(filename);

  std::set<std::string> args;
  for (int i = 2; i < argc; ++ i) {
    args.insert(argv[i]);
  }
  if (args.find("--debug-pp") != args.end()) {
    file.printPreprocessed(std::cout);
  }

  mm::Stream in(file.getPtr(), 0, file.getLength() - 1);

  auto st = parser::ParseLittleProgram()(in);

  if (st && in.eof()) {
    if (args.find("--print-ast") != args.end()) {
      st.dump(std::cout);
    }
    Codegen gen(st);
    gen.dumpSyms(std::cerr);
    gen("dummy");
    
    return 0;
  } else {
    st.dump(std::cerr);
    if (!in.eof()) {
      std::cerr << "Failed to parse all input (" << in.index
              << "\\"<< file.getLength() - 1 <<").\n";
    }
  }
  return 1;
}
