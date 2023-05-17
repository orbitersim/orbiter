project(zlib)

Include(FetchContent)

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
install(FILES $<TARGET_FILE:zlib> DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
