cmake_minimum_required(VERSION 3.10)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
project(mindustry.compiler C)

add_compile_options(-Werror -std=c11)

file(GLOB SRC *.c)
add_executable(main ${SRC})
