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

    set_target_properties(mindcc-wasmlib PROPERTIES LINK_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wasm/ffi/symbols.txt)
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
else()
    file(GLOB SRC cli/*.c)
    add_executable(main ${SRC})
    target_link_libraries(main PRIVATE mindcc-lib)

    file(GLOB TEST_SRC tests/*.c)
    add_executable(minc-tests ${TEST_SRC})
    target_link_libraries(minc-tests PRIVATE mindcc-lib)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Webassembly")
    file(GLOB_RECURSE WEB_FILES RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/targets/web" targets/web/*)
    list(FILTER WEB_FILES EXCLUDE REGEX "^cache\\.manifest\\.in$")
    set(OUT_WEB_FILES)
    foreach(WEB_FILE IN LISTS WEB_FILES)
        list(APPEND OUT_WEB_FILES "${CMAKE_CURRENT_BINARY_DIR}/web/${WEB_FILE}")
        add_custom_command(
            OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/web/${WEB_FILE}"
            COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_SOURCE_DIR}/targets/web/${WEB_FILE}" "${CMAKE_CURRENT_BINARY_DIR}/web/"
            DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/targets/web/${WEB_FILE}"
        )
        install(FILES "targets/web/${WEB_FILE}" DESTINATION "var/run")
    endforeach()
    file(GLOB_RECURSE examples RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/examples" "examples/*")
    foreach(example IN LISTS examples)
        list(APPEND OUT_WEB_FILES "${CMAKE_CURRENT_BINARY_DIR}/web/${example}")
        add_custom_command(
            OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/web/${example}"
            COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_SOURCE_DIR}/examples/${example}" "${CMAKE_CURRENT_BINARY_DIR}/web/"
            DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/examples/${example}"
        )
        install(FILES "examples/${example}" DESTINATION "var/run")
    endforeach()
    add_custom_target(copy-web-assets ALL DEPENDS ${OUT_WEB_FILES})
    string(TIMESTAMP VERSION UTC)
    configure_file(targets/web/cache.manifest.in cache.manifest.inactive @ONLY)

    find_program(WASMOPT wasm-opt PATHS ${CMAKE_CURRENT_SOURCE_DIR}/binaryen)
    find_program(WASM2JS wasm2js PATHS ${CMAKE_CURRENT_SOURCE_DIR}/binaryen)

    if(NOT WASM2JS OR NOT WASMOPT)
        message(FATAL_ERROR "Needs wasm2js and wasm-opt from Binaryen to compile for asm.js. Please download https://github.com/WebAssembly/binaryen/releases/download/1.38.32/binaryen-1.38.32-x86_64-windows.tar.gz and unpack to either `${CMAKE_CURRENT_SOURCE_DIR}/binaryen` or add it to your path.")
    endif()

    add_custom_command(
        OUTPUT web/mindcc-wasm.js
        COMMAND "${WASM2JS}" "$<TARGET_FILE:mindcc-wasmlib>" --emscripten ">" "${CMAKE_CURRENT_BINARY_DIR}/web/mindcc-wasm.js"
        DEPENDS mindcc-wasmlib
        VERBATIM
    )
    add_custom_target(
        mindcc_asmjs ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/web/mindcc-wasm.js
    )

    add_custom_target(dev-server
        COMMAND docker run -it --rm
            --mount type=bind,src=${CMAKE_CURRENT_BINARY_DIR},dst=/usr/share/nginx/html,readonly
            -p 8080:80
            nginx:latest
            nginx -c /usr/share/nginx/html/nginx.conf
        DEPENDS mindcc-wasmlib copy-web-assets mindcc_asmjs
        USES_TERMINAL
    )
    add_custom_target(dev-server-in-docker
        COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/targets/web/run-nginx.sh" "${CMAKE_CURRENT_BINARY_DIR}/web"
        DEPENDS mindcc-wasmlib copy-web-assets mindcc_asmjs
        USES_TERMINAL
    )

    install(
        FILES
            ${CMAKE_CURRENT_BINARY_DIR}/web/mindcc-wasm.js
            $<TARGET_FILE:mindcc-wasmlib>
        DESTINATION "var/run"
    )
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cache.manifest.inactive DESTINATION "var/run" RENAME "cache.manifest")
elseif(0)
    enable_testing()
    find_package(GTest REQUIRED)
    file(GLOB_RECURSE TEST_SOURCES test/*.cpp)
    add_executable(runall-test ${TEST_SOURCES})
    target_link_libraries(runall-test PRIVATE native GTest::GTest GTest::Main)
    add_test(AllTests runall-test)
endif()
