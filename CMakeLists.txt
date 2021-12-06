cmake_minimum_required(VERSION 3.12)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly" AND NOT CMAKE_TOOLCHAIN_FILE)
    set(CMAKE_TOOLCHAIN_FILE "${CMAKE_CURRENT_LIST_DIR}/cmake/wasm.cmake" CACHE FILEPATH "")
endif()

project(mindustry.compiler C)

add_compile_options(-Werror -Wno-parentheses -std=c11)

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY web)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY web)

    # Use custom CRT
    file(GLOB CRT_SRC wasm/ffi/*.c wasm/crt/*.c)
    add_library(mindcc-crt OBJECT ${CRT_SRC})
    target_include_directories(mindcc-crt PUBLIC wasm/crt wasm/ffi)
    link_libraries(mindcc-crt)
endif()

file(GLOB LIB lib/*.c)
add_library(mindcc-lib OBJECT ${LIB})
target_include_directories(mindcc-lib PUBLIC lib)

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly")
    file(GLOB SRC wasm/*.c)
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
    file(GLOB SRC cli/*.c)
    add_executable(main ${SRC})
    target_link_libraries(main PRIVATE mindcc-lib)

    file(GLOB_RECURSE TEST_SOURCES tests/*.c)
    add_executable(runall-test ${TEST_SOURCES})
    target_link_libraries(runall-test PRIVATE mindcc-lib)
    add_test(AllTests runall-test)
endif()
