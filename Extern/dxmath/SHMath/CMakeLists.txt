# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

cmake_minimum_required (VERSION 3.20)

if(DEFINED DIRECTXMATH_VERSION)
   set(SHMATH_VERSION ${DIRECTXMATH_VERSION})
else()
   set(SHMATH_VERSION 1.0.6)
endif()

project(DirectXSH
  VERSION ${SHMATH_VERSION}
  DESCRIPTION "C++ Spherical Harmonics Math Library"
  HOMEPAGE_URL "https://go.microsoft.com/fwlink/?LinkID=615560"
  LANGUAGES CXX)

if(CMAKE_VERSION VERSION_LESS 3.21)
  get_property(not_top DIRECTORY PROPERTY PARENT_DIRECTORY)
  if(not_top)
    set(PROJECT_IS_TOP_LEVEL false)
  else()
    set(PROJECT_IS_TOP_LEVEL true)
  endif()
endif()

if(PROJECT_IS_TOP_LEVEL)
  message(FATAL_ERROR "SHMath should be built by the main CMakeLists using BUILD_SHMATH")
endif()

option(BUILD_DX11 "Build with DirectX11 support" OFF)

option(BUILD_DX12 "Build with DirectX12 support" OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(DEFINED VCPKG_TARGET_ARCHITECTURE)
    set(DXMATH_ARCHITECTURE ${VCPKG_TARGET_ARCHITECTURE})
elseif(CMAKE_GENERATOR_PLATFORM MATCHES "^[Ww][Ii][Nn]32$")
    set(DXMATH_ARCHITECTURE x86)
elseif(CMAKE_GENERATOR_PLATFORM MATCHES "^[Xx]64$")
    set(DXMATH_ARCHITECTURE x64)
elseif(CMAKE_GENERATOR_PLATFORM MATCHES "^[Aa][Rr][Mm]$")
    set(DXMATH_ARCHITECTURE arm)
elseif(CMAKE_GENERATOR_PLATFORM MATCHES "^[Aa][Rr][Mm]64$")
    set(DXMATH_ARCHITECTURE arm64)
elseif(CMAKE_GENERATOR_PLATFORM MATCHES "^[Aa][Rr][Mm]64EC$")
    set(DXMATH_ARCHITECTURE arm64ec)
elseif(CMAKE_VS_PLATFORM_NAME_DEFAULT MATCHES "^[Ww][Ii][Nn]32$")
    set(DXMATH_ARCHITECTURE x86)
elseif(CMAKE_VS_PLATFORM_NAME_DEFAULT MATCHES "^[Xx]64$")
    set(DXMATH_ARCHITECTURE x64)
elseif(CMAKE_VS_PLATFORM_NAME_DEFAULT MATCHES "^[Aa][Rr][Mm]$")
    set(DXMATH_ARCHITECTURE arm)
elseif(CMAKE_VS_PLATFORM_NAME_DEFAULT MATCHES "^[Aa][Rr][Mm]64$")
    set(DXMATH_ARCHITECTURE arm64)
elseif(CMAKE_VS_PLATFORM_NAME_DEFAULT MATCHES "^[Aa][Rr][Mm]64EC$")
    set(DXMATH_ARCHITECTURE arm64ec)
elseif(NOT (DEFINED DXMATH_ARCHITECTURE))
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "[Aa][Rr][Mm]64|aarch64|arm64")
        set(DXMATH_ARCHITECTURE arm64)
    else()
        set(DXMATH_ARCHITECTURE x64)
    endif()
endif()

#--- Library
set(LIBRARY_HEADERS DirectXSH.h)
set(LIBRARY_SOURCES DirectXSH.cpp)

if(WIN32 AND BUILD_DX11)
  set(LIBRARY_SOURCES ${LIBRARY_SOURCES} DirectXSHD3D11.cpp)
endif()

if(WIN32 AND BUILD_DX12)
  set(LIBRARY_SOURCES ${LIBRARY_SOURCES} DirectXSHD3D12.cpp)
endif()

add_library(${PROJECT_NAME} STATIC ${LIBRARY_SOURCES} ${LIBRARY_HEADERS})

target_include_directories(${PROJECT_NAME} PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_compile_features(${PROJECT_NAME} INTERFACE cxx_std_11)

target_link_libraries(${PROJECT_NAME} PRIVATE DirectXMath)

if(MINGW AND BUILD_DX12)
    find_package(directx-headers CONFIG REQUIRED)
    target_link_libraries(${PROJECT_NAME} PUBLIC Microsoft::DirectX-Headers)
    target_compile_definitions(${PROJECT_NAME} PUBLIC USING_DIRECTX_HEADERS)
else()
    find_package(directx-headers CONFIG QUIET)
endif()

if(directx-headers_FOUND)
    message(STATUS "Using DirectX-Headers package")
    target_link_libraries(${PROJECT_NAME} PRIVATE Microsoft::DirectX-Headers)
    target_compile_definitions(${PROJECT_NAME} PRIVATE USING_DIRECTX_HEADERS)
endif()

#--- Package
include(CMakePackageConfigHelpers)

string(TOLOWER ${PROJECT_NAME} PACKAGE_NAME)
cmake_path(GET CMAKE_CURRENT_LIST_DIR PARENT_PATH DIRECTXMATH_PATH)

write_basic_package_version_file(
  ${PACKAGE_NAME}-config-version.cmake
  VERSION ${SHMATH_VERSION}
  COMPATIBILITY AnyNewerVersion
  ARCH_INDEPENDENT)

install(TARGETS ${PROJECT_NAME}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

configure_package_config_file(${DIRECTXMATH_PATH}/build/DirectXMath-config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}-config.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PACKAGE_NAME})

install(EXPORT ${PROJECT_NAME}-targets
  FILE ${PROJECT_NAME}-targets.cmake
  NAMESPACE Microsoft::
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PACKAGE_NAME})

install(FILES ${LIBRARY_HEADERS}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}-config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PACKAGE_NAME})

#--- Compiler switches
if(MSVC)
    target_compile_options(${PROJECT_NAME} PRIVATE /Wall /EHsc /GR "$<$<NOT:$<CONFIG:DEBUG>>:/guard:cf>")

    if((MSVC_VERSION GREATER_EQUAL 1928)
       AND (CMAKE_SIZEOF_VOID_P EQUAL 8)
       AND ((NOT (CMAKE_CXX_COMPILER_ID MATCHES "Clang|IntelLLVM")) OR (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 13.0)))
        target_compile_options(${PROJECT_NAME} PRIVATE "$<$<NOT:$<CONFIG:DEBUG>>:/guard:ehcont>")
    endif()
else()
    target_compile_definitions(${PROJECT_NAME} PRIVATE $<IF:$<CONFIG:DEBUG>,_DEBUG,NDEBUG>)
endif()

if(NOT (${DXMATH_ARCHITECTURE} MATCHES "^arm"))
    if(CMAKE_SIZEOF_VOID_P EQUAL 4)
        set(ARCH_SSE2 $<$<CXX_COMPILER_ID:MSVC,Intel>:/arch:SSE2> $<$<NOT:$<CXX_COMPILER_ID:MSVC,Intel>>:-msse2>)
    else()
        set(ARCH_SSE2 $<$<NOT:$<CXX_COMPILER_ID:MSVC,Intel>>:-msse2>)
    endif()

    if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
        list(APPEND ARCH_SSE2 -mfpmath=sse)
    endif()

    target_compile_options(${PROJECT_NAME} PRIVATE ${ARCH_SSE2})
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|IntelLLVM")
    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 16.0)
        target_compile_options(${PROJECT_NAME} PRIVATE /ZH:SHA_256 "-Wno-unsafe-buffer-usage")
    endif()
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    target_compile_options(${PROJECT_NAME} PRIVATE -Wno-ignored-attributes)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    target_compile_options(${PROJECT_NAME} PRIVATE /Zc:__cplusplus /Zc:inline /fp:fast /Qdiag-disable:161)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    target_compile_options(${PROJECT_NAME} PRIVATE
         /sdl /Zc:inline /fp:fast
         "/wd4061" "/wd4365" "/wd4514" "/wd4571" "/wd4668" "/wd4710" "/wd4820" "/wd5039" "/wd5045")

    if(CMAKE_INTERPROCEDURAL_OPTIMIZATION)
        target_compile_options(${PROJECT_NAME} PRIVATE $<$<NOT:$<CONFIG:DEBUG>>:/Gy /Gw>)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.10)
        target_compile_options(${PROJECT_NAME} PRIVATE /permissive-)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.14)
        target_compile_options(${PROJECT_NAME} PRIVATE /Zc:__cplusplus)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.24)
        target_compile_options(${PROJECT_NAME} PRIVATE /ZH:SHA_256)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.26)
        target_compile_options(${PROJECT_NAME} PRIVATE /Zc:preprocessor /wd5105)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.28)
        target_compile_options(${PROJECT_NAME} PRIVATE /Zc:lambda)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.29)
        target_compile_options(${PROJECT_NAME} PRIVATE /external:W4)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.34)
        target_compile_options(${PROJECT_NAME} PRIVATE /wd5262 /wd5264)
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 19.35)
        if(CMAKE_INTERPROCEDURAL_OPTIMIZATION)
          target_compile_options(${PROJECT_NAME} PRIVATE $<$<NOT:$<CONFIG:DEBUG>>:/Zc:checkGwOdr>)
        endif()

        target_compile_options(${PROJECT_NAME} PRIVATE $<$<VERSION_GREATER_EQUAL:${CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION},10.0.22000>:/Zc:templateScope>)
    endif()
endif()

if(NOT WIN32)
    file(DOWNLOAD
        https://raw.githubusercontent.com/dotnet/runtime/v9.0.2/src/coreclr/pal/inc/rt/sal.h
        "${CMAKE_CURRENT_BINARY_DIR}/sal/sal.h"
        EXPECTED_HASH SHA512=8085f67bfa4ce01ae89461cadf72454a9552fde3f08b2dcc3de36b9830e29ce7a6192800f8a5cb2a66af9637be0017e85719826a4cfdade508ae97f319e0ee8e
    )

    target_include_directories(${PROJECT_NAME} PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/sal")
endif()
