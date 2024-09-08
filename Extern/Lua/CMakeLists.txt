cmake_minimum_required(VERSION 3.0)

include(FetchContent)

FetchContent_Declare(
  luajitsrc
  GIT_REPOSITORY https://github.com/LuaJIT/LuaJIT.git
  GIT_TAG v2.1
)

FetchContent_MakeAvailable(luajitsrc)

set(LUAJIT_DIR ${luajitsrc_SOURCE_DIR})

if(LUAJIT_DIR)
  project(luajit)
  include(LuaJIT.cmake)
  if (PARENT_DIRECTORY)
    set(LUA_TARGET luajit PARENT_SCOPE)
    set(LUA_LIBRARIES libluajit PARENT_SCOPE)
  endif()
else()
  message(FATAL_ERROR "LUAJIT_DIR not set")
endif()
