set(CMAKE_CXX_COMPILER clang++ CACHE STRING "" FORCE)
set(CMAKE_C_COMPILER clang CACHE STRING "" FORCE)

if(DEFINED VCPKG_ROOT)
    set(VCPKG_TARGET_TRIPLET x64-windows-static-md)
    set(VCPKG_HOST_TRIPLET x64-windows-static-md)
    include(${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake)
endif()
