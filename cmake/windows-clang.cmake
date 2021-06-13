set(CMAKE_CXX_COMPILER clang++ CACHE STRING "" FORCE)
set(CMAKE_C_COMPILER clang CACHE STRING "" FORCE)

if(DEFINED VCPKG_ROOT)
    set(VCPKG_TARGET_TRIPLET x64-windows)
    include(${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake)
endif()
