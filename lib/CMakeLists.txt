cmake_minimum_required(VERSION 2.8.8)
add_subdirectory(AST)

add_library(Little SHARED empty.cpp)

target_link_libraries(Little
  AST
)