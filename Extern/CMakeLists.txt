Include(FetchContent)

if(NOT DEFINED ORBITER_BINARY_ROOT_DIR)
    message(FATAL_ERROR "ORBITER_BINARY_ROOT_DIR is not set")
endif(NOT DEFINED ORBITER_BINARY_ROOT_DIR)

add_subdirectory(lua)
add_subdirectory(zlib)

## LFS
add_library(lfs SHARED luafilesystem/src/lfs.c)
#install(TARGETS lfs	DESTINATION ${ORBITER_BINARY_ROOT_DIR})
#install(TARGETS lfs DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils/)
target_include_directories(lfs PUBLIC LUAJIT_DIR)
target_link_libraries(lfs libluajit)
SET_TARGET_PROPERTIES(lfs PROPERTIES LINK_FLAGS "/DEF:\"${CMAKE_CURRENT_SOURCE_DIR}/luafilesystem/src/lfs.def\"")

set_target_properties(lfs
	PROPERTIES
	FOLDER ${ORBITER_BINARY_ROOT_DIR}/Extern/Lua/
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/Extern/Lua/
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/Extern/Lua/
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/Extern/Lua/
)
install(TARGETS lfs	DESTINATION ${ORBITER_BINARY_ROOT_DIR}/Extern/Lua/)

add_custom_target(CopyLDoc ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ldoc/ ${ORBITER_BINARY_ROOT_DIR}/packages/ldoc
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Penlight/lua/ ${ORBITER_BINARY_ROOT_DIR}/Extern/Lua/lua
)
