project(lua)

Include(FetchContent)

FetchContent_Declare(
  lua
  GIT_REPOSITORY https://github.com/lua/lua.git
  GIT_TAG v5.1
)
FetchContent_MakeAvailable(lua)

file(GLOB LUA_SRC_FILES "${lua_SOURCE_DIR}/*.c")
file(GLOB LUA_HDR_FILES "${lua_SOURCE_DIR}/*.h")

# TODO: add executable
add_executable(lua-exe ${LUA_SRC_FILES})
add_executable(lua::exe ALIAS lua-exe)
set_target_properties(lua-exe PROPERTIES OUTPUT_NAME lua)

add_compile_definitions(LUA_BUILD_AS_DLL)

add_library(lua SHARED ${LUA_SRC_FILES})
add_library(lua::lib ALIAS lua)
set(LUA_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
file(COPY ${LUA_HDR_FILES} DESTINATION ${LUA_INCLUDE_DIR})
target_include_directories(lua INTERFACE ${LUA_INCLUDE_DIR})

set_target_properties(lua PROPERTIES 
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)
install(FILES $<TARGET_FILE:lua::lib> DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
