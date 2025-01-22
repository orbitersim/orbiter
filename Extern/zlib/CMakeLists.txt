project(zlib)

if (MSVC)
  Include(FetchContent)

  set(SKIP_INSTALL_ALL ON)

  FetchContent_Declare(
    zlib
    GIT_REPOSITORY https://github.com/madler/zlib.git
    GIT_TAG v1.2.11
  )
  FetchContent_MakeAvailable(zlib)
  target_include_directories(zlib INTERFACE ${zlib_SOURCE_DIR})
  target_include_directories(zlib INTERFACE ${zlib_BINARY_DIR})

  set_target_properties(zlib PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
      LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
      RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
  )
  install(TARGETS zlib RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
else()
  find_library(zlib 
    REQUIRED
    NAMES libz.so zlib.so
    HINTS $ENV{ZLIB_DIR}
    PATH_SUFFIXES lib64 lib x86_64-linux-gnu
    PATHS
      /usr/local
      /usr/lib
      /usr
      /sw
      /opt/local
      /opt
  )
  message(STATUS "Using zlib: ${zlib}")
endif()
