cmake_minimum_required(VERSION 2.8.8)

add_library(Codegen SHARED Codegen.cpp)
target_link_libraries(Codegen ${llvm_libs})
