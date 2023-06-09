##########################################
project(lua)

file(GLOB LUA_FILES "Lua/*.c" "Lua/*.h")

add_library(lua SHARED ${LUA_FILES})
add_library(lua::lib ALIAS lua)
target_compile_definitions(lua PUBLIC LUA_BUILD_AS_DLL)

# for tests and running Orbiter from inside the build dir
set_target_properties(lua PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

install(TARGETS lua RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
install(TARGETS lua DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib/Lua)

##########################################
## zlib
add_subdirectory(zlib)
