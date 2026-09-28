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

list(REMOVE_ITEM LUA_SRC_FILES "${lua_SOURCE_DIR}/lua.c")
list(REMOVE_ITEM LUA_SRC_FILES "${lua_SOURCE_DIR}/luac.c")
list(REMOVE_ITEM LUA_SRC_FILES "${lua_SOURCE_DIR}/print.c")

if(WIN32) # Windows: upstream
# Executable - lua.exe
else() # Linux port
# Executable - lua
endif() # WIN32
add_executable(lua-exe ${LUA_SRC_FILES} "${lua_SOURCE_DIR}/lua.c")
set_target_properties(lua-exe PROPERTIES OUTPUT_NAME lua)

set_target_properties(lua-exe PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

install(TARGETS lua-exe RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
install(TARGETS lua-exe DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib/Lua)
install(TARGETS lua-exe DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils/)

if(WIN32) # Windows: upstream
# Library - lua.dll
else() # Linux port
# Library - liblua.so
endif() # WIN32
add_library(lua SHARED ${LUA_SRC_FILES})

if(WIN32) # Windows: upstream
if(MSVC)
  target_compile_definitions(lua PUBLIC LUA_BUILD_AS_DLL)
else()
  target_link_libraries(lua-exe PUBLIC m)
  target_link_libraries(lua PUBLIC m)
endif()
else() # Linux port
# LUA_BUILD_AS_DLL is the MSVC side; POSIX + dlopen give require() the C-module loader it gets from LoadLibrary
target_compile_definitions(lua-exe PRIVATE LUA_USE_POSIX LUA_USE_DLOPEN)
target_compile_definitions(lua PRIVATE LUA_USE_POSIX LUA_USE_DLOPEN)
target_link_libraries(lua-exe PUBLIC m ${CMAKE_DL_LIBS})
target_link_libraries(lua PUBLIC m ${CMAKE_DL_LIBS})
endif() # WIN32

set_target_properties(lua PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)
install(TARGETS lua DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

set(LUA_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
file(COPY ${LUA_HDR_FILES} DESTINATION ${LUA_INCLUDE_DIR})
target_include_directories(lua INTERFACE ${LUA_INCLUDE_DIR})

# Aliases for easier reference
add_executable(lua::exe ALIAS lua-exe)
add_library(lua::lib ALIAS lua)
