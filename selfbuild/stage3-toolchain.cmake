set(CMAKE_SYSTEM_PROCESSOR x86_64)
set(CMAKE_SYSTEM_NAME Darwin)
set(CMAKE_SYSTEM_VERSION 21.3.0)
set(CMAKE_C_COMPILER ${CMAKE_CURRENT_LIST_DIR}/stage3/main)
include_directories(
    /Library/Developer/CommandLineTools/SDKs/MacOSX11.3.sdk/usr/include
    /Library/Developer/CommandLineTools/usr/lib/clang/13.0.0/include
)
set(CMAKE_C_FLAGS_INIT)
set(CMAKE_LINKER clang)
add_link_options(-target x86_64-apple-darwin20.3.0 -mcpu=x86-64)

# set(CMAKE_C_LINK_EXECUTABLE "<CMAKE_LINKER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
