project(lua)

if(MSVC)
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

  # Executable - lua.exe
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

  # Library - lua.dll
  add_library(lua SHARED ${LUA_SRC_FILES})

  if(MSVC)
    target_compile_definitions(lua PUBLIC LUA_BUILD_AS_DLL)
  else()
    target_link_libraries(lua-exe PUBLIC m)
    target_link_libraries(lua PUBLIC m)
  endif()

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
else()
  # Define Lua executable
  find_program(LUA_EXECUTABLE REQUIRED NAMES lua)
  add_executable(lua-exe IMPORTED GLOBAL)
  set_target_properties(lua-exe PROPERTIES IMPORTED_LOCATION ${LUA_EXECUTABLE})
  add_executable(lua::exe ALIAS lua-exe)

  # Define Lua library
  find_library(
    LUA_LIBRARY 
    REQUIRED
    NAMES lua5.1
    HINTS $ENV{LUA_DIR}
    PATH_SUFFIXES lib64 lib x86_64-linux-gnu
    PATHS
      /usr/local
      /usr/lib
      /usr
      /sw
      /opt/local
      /opt
  )
  # find_package(Lua51 REQUIRED)
  message(STATUS "Using lua: ${LUA_LIBRARY}")
  add_library(lua IMPORTED SHARED GLOBAL)
  set_target_properties(lua PROPERTIES IMPORTED_LOCATION ${LUA_LIBRARY})
  target_include_directories(lua INTERFACE ${LUA_INCLUDE_DIR})
  add_library(lua::lib ALIAS lua)
endif()
