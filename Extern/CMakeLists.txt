##########################################
project(lua)

file(GLOB LUA_FILES "Lua/*.c" "Lua/*.h")

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

##########################################
project(zlib)

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
project(Catch2)

if(ORBITER_MAKE_TESTS)
    add_subdirectory(Catch2)
endif()