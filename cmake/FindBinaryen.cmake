if(CMAKE_HOST_WIN32)
    configure_file("${CMAKE_CURRENT_LIST_DIR}/nuget/packages.proj" "${CMAKE_BINARY_DIR}/CMakeFiles/FindBinaryen/packages.proj" COPYONLY)
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/CMakeFiles/FindBinaryen/obj/packages.proj.nuget.g.props")
        execute_process(COMMAND
            "${CMAKE_CURRENT_LIST_DIR}/nuget/nuget.exe" "restore" "${CMAKE_BINARY_DIR}/CMakeFiles/FindBinaryen/packages.proj"
            "-source" "https://sgreenlay.pkgs.visualstudio.com/ToDo/_packaging/ToDo/nuget/v3/index.json"
        )
    endif()
    if(EXISTS "${CMAKE_BINARY_DIR}/CMakeFiles/FindBinaryen/obj/packages.proj.nuget.g.props")
        file(READ "${CMAKE_BINARY_DIR}/CMakeFiles/FindBinaryen/obj/packages.proj.nuget.g.props" _contents)
        string(REGEX MATCH "<PkgBinaryen-x64-windows Condition=\" '\\$\\(PkgBinaryen-x64-windows\\)' == '' \">(.*)</PkgBinaryen-x64-windows>" PKG "${_contents}")
        list(APPEND CMAKE_PROGRAM_PATH "${CMAKE_MATCH_1}")
    endif()
endif()

# Can be from https://github.com/WebAssembly/binaryen/releases/download/1.38.32/binaryen-1.38.32-x86_64-windows.tar.gz

find_program(Wasm2JS_EXECUTABLE wasm2js PATHS ${CMAKE_CURRENT_SOURCE_DIR}/binaryen)
find_program(WasmOpt_EXECUTABLE wasm-opt PATHS ${CMAKE_CURRENT_SOURCE_DIR}/binaryen)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
    Binaryen
    FOUND_VAR Binaryen_FOUND
    REQUIRED_VARS Wasm2JS_EXECUTABLE WasmOpt_EXECUTABLE
)
