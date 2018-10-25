cmake_minimum_required(VERSION 2.8.8)
add_executable(little-frontend little.cpp)
target_link_libraries(little-frontend
  PP Codegen Parser ${llvm_libs})
