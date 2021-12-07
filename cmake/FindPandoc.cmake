find_program(Pandoc_EXECUTABLE NAMES pandoc)

if(NOT Pandoc_EXECUTABLE)
    include(FetchContent)

    FetchContent_Declare(
        pandoc
        URL "https://github.com/jgm/pandoc/releases/download/2.16.2/pandoc-2.16.2-windows-x86_64.zip"
        URL_HASH SHA256=a01bfd0fb702c4fc3de2e829fac2c6964afdb2bc285f57dce4ec00945069eff7
    )

    FetchContent_MakeAvailable(pandoc)

    find_program(Pandoc_EXECUTABLE NAMES pandoc PATHS "${pandoc_SOURCE_DIR}")
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
    Pandoc
    FOUND_VAR Pandoc_FOUND
    REQUIRED_VARS
        Pandoc_EXECUTABLE
)
