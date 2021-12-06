set(CMAKE_CROSSCOMPILING ON)

set(CMAKE_SYSTEM_NAME Webassembly)
set(CMAKE_SYSTEM_VERSION 1)
set(CMAKE_SYSTEM_PROCESSOR x86)

if(CMAKE_HOST_WIN32)
    if(DEFINED ENV{WASM_TOOLCHAIN})
        file(TO_CMAKE_PATH "$ENV{WASM_TOOLCHAIN}" CLANG_ROOT)
    elseif(EXISTS ${CMAKE_CURRENT_LIST_DIR}/../clang/bin/clang.exe)
        set(CLANG_ROOT "${CMAKE_CURRENT_LIST_DIR}/../clang")
    elseif(EXISTS "$ENV{VCINSTALLDIR}Tools/Llvm/bin/clang++.exe")
        file(TO_CMAKE_PATH "$ENV{VCINSTALLDIR}Tools/Llvm" CLANG_ROOT)
    else()
        message(FATAL_ERROR "Can't locate clang compiler.

    Download LLVM and unzip with 7zip, then set %WASM_TOOLCHAIN%")
    endif()

    set(CMAKE_CXX_COMPILER "${CLANG_ROOT}/bin/clang++.exe" CACHE FILEPATH "" FORCE)
    set(CMAKE_C_COMPILER "${CLANG_ROOT}/bin/clang.exe" CACHE FILEPATH "" FORCE)
    set(CMAKE_AR "${CLANG_ROOT}/bin/llvm-ar.exe" CACHE FILEPATH "")
else()
    set(CMAKE_CXX_COMPILER clang++ CACHE STRING "" FORCE)
    set(CMAKE_C_COMPILER clang CACHE STRING "" FORCE)
    set(CMAKE_AR llvm-ar CACHE STRING "" FORCE)
endif()

set(CMAKE_CXX_FLAGS "--target=wasm32 -nostdlib -m32 -fdiagnostics-absolute-paths -D__wasm -fno-exceptions -fno-threadsafe-statics -fno-rtti" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g -flto=thin -gsplit-dwarf" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-Os -g -DNDEBUG -flto=thin" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_RELEASE "-Os -DNDEBUG -flto=thin" CACHE STRING "" FORCE)
set(CMAKE_C_FLAGS "${CMAKE_CXX_FLAGS}" CACHE STRING "" FORCE)
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}" CACHE STRING "" FORCE)
set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO}" CACHE STRING "" FORCE)
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE}" CACHE STRING "" FORCE)
set(CMAKE_SHARED_LINKER_FLAGS "-Xlinker --import-memory -Xlinker --merge-data-segments -Xlinker --gc-sections" CACHE STRING "" FORCE)
set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "-Xlinker --strip-all" CACHE STRING "" FORCE)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")

if(CMAKE_HOST_WIN32)
    configure_file("${CMAKE_CURRENT_LIST_DIR}/nuget/packages.proj" "${CMAKE_BINARY_DIR}/nuget/packages.proj" COPYONLY)
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/nuget/obj/packages.proj.nuget.g.props")
        execute_process(COMMAND
            "${CMAKE_CURRENT_LIST_DIR}/nuget/nuget.exe" "restore" "${CMAKE_BINARY_DIR}/nuget/packages.proj"
            "-source" "https://sgreenlay.pkgs.visualstudio.com/ToDo/_packaging/ToDo/nuget/v3/index.json"
        )
    endif()
    if(EXISTS "${CMAKE_BINARY_DIR}/nuget/obj/packages.proj.nuget.g.props")
        file(READ "${CMAKE_BINARY_DIR}/nuget/obj/packages.proj.nuget.g.props" _contents)
        string(REGEX MATCH "<PkgBinaryen-x64-windows Condition=\" '\\$\\(PkgBinaryen-x64-windows\\)' == '' \">(.*)</PkgBinaryen-x64-windows>" PKG "${_contents}")
        list(APPEND CMAKE_PROGRAM_PATH "${CMAKE_MATCH_1}")
    endif()
endif()

find_program(WASMOPT wasm-opt)
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(CMAKE_CXX_CREATE_SHARED_LIBRARY
        "<CMAKE_CXX_COMPILER> <CMAKE_SHARED_LIBRARY_CXX_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET_BASE>.unopt.wasm <OBJECTS> <LINK_LIBRARIES>" "\"${WASMOPT}\" -o <TARGET> -Oz <TARGET_BASE>.unopt.wasm")
endif()

function(add_website_target arg_target)
    cmake_parse_arguments(PARSE_ARGV 1 arg "" "DIR" "SOURCES")
    if(arg_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR "Unexpected arguments in add_website_target(): ${arg_UNPARSED_ARGUMENTS}")
    endif()
    if(NOT arg_DIR)
        message(FATAL_ERROR "Expected 'DIR' in add_website_target()")
    endif()
    get_filename_component(builddir "${arg_DIR}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}")

    set(build_files)
    set(install_files)
    foreach(source IN LISTS arg_SOURCES)
        get_filename_component(fn "${source}" NAME)
        get_filename_component(ext "${source}" LAST_EXT)
        get_filename_component(afn "${source}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
        if(fn STREQUAL "cache.manifest.in")
            string(TIMESTAMP VERSION UTC)
            configure_file("${afn}" "CMakeFiles/${arg_target}.dir/cache.manifest" @ONLY)
            list(APPEND install_files "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${arg_target}.dir/cache.manifest")
        else()
            if(ext STREQUAL ".md")
                # Transform markdown with pandoc
                find_program(PANDOC pandoc REQUIRED)
                get_filename_component(base "${source}" NAME_WLE)
                set(build_file "${builddir}/${base}.html")
                add_custom_command(
                    OUTPUT "${build_file}"
                    COMMAND "${PANDOC}" "${afn}" -o "${build_file}"
                    DEPENDS "${afn}"
                )
                list(APPEND install_files "${afn}")
            else()
                set(build_file "${builddir}/${fn}")
                add_custom_command(
                    OUTPUT "${build_file}"
                    COMMAND "${CMAKE_COMMAND}" -E copy "${afn}" "${build_file}"
                    DEPENDS "${afn}"
                    VERBATIM
                )
                list(APPEND install_files "${build_file}")
            endif()
            list(APPEND build_files "${build_file}")
        endif()
    endforeach()

    add_custom_target(_${arg_target}_files ALL DEPENDS ${build_files})
    add_library(${arg_target} INTERFACE)
    add_dependencies(${arg_target} _${arg_target}_files)
    set_target_properties(${arg_target} PROPERTIES "RESOURCE" "${install_files}")

    find_program(servethis NAME servethis)
    if(servethis)
        add_custom_target(serve-${arg_target}
            COMMAND ${servethis}
            WORKING_DIRECTORY "${builddir}"
            DEPENDS "${arg_target}"
            VERBATIM
            USES_TERMINAL
        )
    endif()

    add_custom_target(dev-server
        COMMAND docker run -it --rm
            --mount type=bind,src=${builddir},dst=/usr/share/nginx/html,readonly
            -p 8080:80
            nginx:latest
            nginx -c /usr/share/nginx/html/nginx.conf
        DEPENDS "${arg_target}"
        VERBATIM
        USES_TERMINAL
    )

    add_custom_target(dev-server-in-docker
        COMMAND "${CMAKE_CURRENT_LIST_DIR}/run-nginx.sh" "${builddir}"
        DEPENDS "${arg_target}"
        VERBATIM
        USES_TERMINAL
    )
endfunction()

function(add_wasmjs_target arg_target wasm_target output_file)
    find_program(WASM2JS wasm2js PATHS ${CMAKE_CURRENT_SOURCE_DIR}/binaryen)

    if(NOT WASM2JS)
        message(FATAL_ERROR "Needs wasm2js from Binaryen to compile for asm.js. Please download https://github.com/WebAssembly/binaryen/releases/download/1.38.32/binaryen-1.38.32-x86_64-windows.tar.gz and unpack to either `${CMAKE_CURRENT_SOURCE_DIR}/binaryen` or add it to your path.")
    endif()

    if(TARGET "${wasm_target}")
        set(input_file "$<TARGET_FILE:${wasm_target}>")
    else()
        message(FATAL_ERROR "No target \"${wasm_target}\"")
    endif()

    # get_filename_component(output_file "${output_file}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}")

    add_custom_command(
        OUTPUT "${output_file}"
        COMMAND "${WASM2JS}" "${input_file}" --emscripten ">" "${output_file}"
        DEPENDS "${wasm_target}"
        VERBATIM
    )
    add_library("${arg_target}" INTERFACE "${output_file}")
    set_target_properties("${arg_target}" PROPERTIES "RESOURCE" "${output_file}")
endfunction()
