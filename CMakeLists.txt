cmake_minimum_required(VERSION 3.10)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
project(mindustry.compiler C)

add_compile_options(-Werror -std=c11)

add_executable(main main.c lex.c parse.c)
