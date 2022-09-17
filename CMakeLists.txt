cmake_minimum_required(VERSION 3.12)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly" AND NOT CMAKE_TOOLCHAIN_FILE)
    set(CMAKE_TOOLCHAIN_FILE "${CMAKE_CURRENT_LIST_DIR}/cmake/wasm.cmake" CACHE FILEPATH "")
endif()

project(mindustry.compiler C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
add_compile_options(-Werror -std=c11 -Wall -Wno-parentheses -Wno-missing-braces -Wno-comment)

if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-Wno-format-truncation)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_options(-fsanitize=undefined,address)
    add_link_options(-fsanitize=undefined,address)

    if(LINUX)
        link_libraries(-lasan -lubsan)
    endif()
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY web)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY web)

    # Use custom CRT
    file(GLOB CRT_SRC wasm/ffi/* wasm/crt/*)
    add_library(mindcc-crt OBJECT ${CRT_SRC})
    target_include_directories(mindcc-crt PUBLIC wasm/crt wasm/ffi)
    link_libraries(mindcc-crt)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    add_compile_options(-D_CRT_SECURE_NO_WARNINGS)

    # Extend uCRT with additional posix functions
    file(GLOB CRT_SRC windows/*)
    add_library(mindcc-crt OBJECT ${CRT_SRC})
    target_include_directories(mindcc-crt PUBLIC windows)
    link_libraries(mindcc-crt)
endif()

file(GLOB LIB lib/*.c lib/*.h)
add_library(mindcc-lib OBJECT ${LIB})
target_include_directories(mindcc-lib PUBLIC lib)

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly")
    file(GLOB SRC wasm/*)
    add_library(mindcc-wasmlib SHARED ${SRC})
    target_link_libraries(mindcc-wasmlib PRIVATE mindcc-lib)
    set_target_properties(mindcc-wasmlib PROPERTIES
        LINK_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/wasm/ffi/symbols.txt"
    )
    target_link_options(mindcc-wasmlib PRIVATE
        "LINKER:--allow-undefined-file=${CMAKE_CURRENT_SOURCE_DIR}/wasm/ffi/symbols.txt"
        "LINKER:--no-entry"
        "LINKER:--export=wasmmain"
        "LINKER:--export=call"
        "LINKER:--export=callback"
        "LINKER:--export=malloc"
        "LINKER:--export=free"
        "LINKER:--export=memcpy"
        "LINKER:--export=memset"
        "LINKER:--export=get_callback_buffer"
    )

    file(GLOB web_files "targets/web/*" "examples/*")
    option(GENERATE_DOCS "" ON)

    if(GENERATE_DOCS)
        list(APPEND web_files "docs/extensions.md")
    endif()

    add_wasmjs_target(mindcc-asmjs mindcc-wasmlib "web/mindcc-wasm.js")
    add_website_target(website DIR web SOURCES ${web_files})
    add_dependencies(website mindcc-asmjs mindcc-wasmlib)

    install(TARGETS mindcc-wasmlib mindcc-asmjs website DESTINATION "var/run")
else()
    add_executable(main cli/main.c)
    target_link_libraries(main PRIVATE mindcc-lib)

    file(GLOB TEST_SOURCES tests/*)
    add_executable(runall-test ${TEST_SOURCES})
    target_link_libraries(runall-test PRIVATE mindcc-lib)

    add_custom_target(test COMMAND runall-test "${PROJECT_SOURCE_DIR}" VERBATIM)
endif()
