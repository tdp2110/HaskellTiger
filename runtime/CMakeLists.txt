cmake_minimum_required(VERSION 3.2.1)

project(tiger_rt)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(SOURCES "src/tiger-rt.cpp")

add_library(tiger_rt ${SOURCES})

target_include_directories(tiger_rt PUBLIC include)
