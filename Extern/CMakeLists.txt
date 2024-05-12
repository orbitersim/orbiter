project(Extern)

##########################################
## Lua

file(GLOB LUA_FILES "Lua/*.c" "Lua/*.h")
list(REMOVE_ITEM LUA_FILES ${CMAKE_CURRENT_SOURCE_DIR}/Lua/lua.c)
list(REMOVE_ITEM LUA_FILES ${CMAKE_CURRENT_SOURCE_DIR}/Lua/luac.c)
list(REMOVE_ITEM LUA_FILES ${CMAKE_CURRENT_SOURCE_DIR}/Lua/print.c)
add_library(lua SHARED ${LUA_FILES})
target_compile_definitions(lua PUBLIC LUA_BUILD_AS_DLL)

# for tests and running Orbiter from inside the build dir
set_target_properties(lua PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

install(TARGETS lua RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
install(TARGETS lua DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib/Lua)
install(TARGETS lua DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils/)


add_executable(lua_bin
	${LUA_FILES} "Lua/lua.c"
)
set_target_properties(lua_bin
	PROPERTIES
	OUTPUT_NAME lua
)
set_target_properties(lua_bin PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

install(TARGETS lua_bin
	DESTINATION ${ORBITER_BINARY_ROOT_DIR}
)
install(TARGETS lua_bin DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils/)

## LFS
add_library(lfs SHARED luafilesystem/src/lfs.c)
	install(TARGETS lfs	DESTINATION ${ORBITER_BINARY_ROOT_DIR})
	install(TARGETS lfs DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils/)
	target_include_directories(lfs PUBLIC Lua)
	target_link_libraries(lfs lua)
	SET_TARGET_PROPERTIES(lfs PROPERTIES LINK_FLAGS "/DEF:\"${CMAKE_CURRENT_SOURCE_DIR}/luafilesystem/src/lfs.def\""
)
install(TARGETS lfs
	DESTINATION ${ORBITER_BINARY_ROOT_DIR}
)
set_target_properties(lfs PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

add_custom_target(CopyLDoc ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ldoc/ ${ORBITER_BINARY_ROOT_DIR}/packages/ldoc
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Penlight/lua/ ${ORBITER_BINARY_ROOT_DIR}/
)

##########################################
## zlib

set(SKIP_INSTALL_ALL ON)
add_subdirectory(zlib)

target_include_directories(zlib INTERFACE ${zlib_SOURCE_DIR})
target_include_directories(zlib INTERFACE ${zlib_BINARY_DIR}) # for zconf.h

# for tests and running Orbiter from inside the build dir
set_target_properties(zlib PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

install(TARGETS zlib RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

##########################################
## Catch2

if(ORBITER_MAKE_TESTS)
    add_subdirectory(Catch2)
endif()
