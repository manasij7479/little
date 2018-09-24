cmake_minimum_required(VERSION 3.6)
project(little)

ADD_DEFINITIONS(
    -std=c++1z # Or -std=c++0x
    -O2
)

find_package(LLVM REQUIRED CONFIG)

message(STATUS “Found LLVM ${LLVM_PACKAGE_VERSION}“)
message(STATUS “Using LLVMConfig.cmake in: ${LLVM_DIR}“)

add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs support core irreader)


# OSX
if(APPLE)
    set(CMAKE_MACOSX_RPATH ON)
    set(CMAKE_SKIP_BUILD_RPATH FALSE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
    list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
    if("${isSystemDir}" STREQUAL "-1")
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    endif()
endif()

project(little)

include_directories(include ${LLVM_INCLUDE_DIRS})
link_directories(lib)

add_subdirectory(lib)
add_subdirectory(driver)
