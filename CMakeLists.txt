cmake_minimum_required(VERSION 3.6)
ADD_DEFINITIONS(
    -std=c++1z # Or -std=c++0x
    -O2
)
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

include_directories(include)
link_directories(lib)

add_subdirectory(lib)
add_subdirectory(driver)
