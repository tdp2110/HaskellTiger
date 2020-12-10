cmake_minimum_required(VERSION 3.14)

project(tiger_rt)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include(CheckIPOSupported)
check_ipo_supported(RESULT LTO_SUPPORTED)

if (LTO_SUPPORTED)
  set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

set(SOURCES "src/tiger-rt.cpp")

add_library(tiger_rt ${SOURCES})

target_include_directories(tiger_rt PUBLIC include)
