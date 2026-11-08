# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.24)
set(CMAKE_POLICY_VERSION_MINIMUM 3.5)

set(CMAKE_OSX_ARCHITECTURES arm64 CACHE STRING "Target architecture")
set(CMAKE_OSX_DEPLOYMENT_TARGET 15.0 CACHE STRING "Minimum macOS version")

project (Orbiter VERSION 21.7.24 LANGUAGES C CXX OBJC OBJCXX)

if(NOT APPLE)
	message(FATAL_ERROR "This fork builds Orbiter for macOS only")
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_OBJCXX_STANDARD 17)

include(CMakePrintHelpers)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_MODULE_PATH
	${CMAKE_SOURCE_DIR}/cmake
	${CMAKE_MODULE_PATH}
)

set(BUILD64 1)
set(ARCH "arm64")

set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")

set(ORBITER_SOURCE_ROOT_DIR ${CMAKE_SOURCE_DIR})
set(ORBITER_SOURCE_DIR ${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter)
set(ORBITER_SOURCE_MODULE_DIR ${ORBITER_SOURCE_ROOT_DIR}/Src/Plugin)
set(ORBITER_SOURCE_SDK_DIR ${ORBITER_SOURCE_ROOT_DIR}/Orbitersdk)
set(ORBITER_SOURCE_SDK_INCLUDE_DIR ${ORBITER_SOURCE_SDK_DIR}/include)
set(ORBITER_SOURCE_HTML_DIR ${ORBITER_SOURCE_ROOT_DIR}/Html)
set(ORBITER_COMPAT_INCLUDE_DIR ${ORBITER_SOURCE_SDK_INCLUDE_DIR}/compat)

set(ORBITER_BINARY_ROOT_DIR ${CMAKE_BINARY_DIR})
set(ORBITER_BINARY_CONFIG_DIR ${ORBITER_BINARY_ROOT_DIR}/Config)
set(ORBITER_BINARY_MESH_DIR ${ORBITER_BINARY_ROOT_DIR}/Meshes)
set(ORBITER_BINARY_MODULE_DIR ${ORBITER_BINARY_ROOT_DIR}/Modules)
set(ORBITER_BINARY_STARTUP_DIR ${ORBITER_BINARY_MODULE_DIR}/Startup)
set(ORBITER_BINARY_CELBODY_DIR ${ORBITER_BINARY_MODULE_DIR}/Celbody)
set(ORBITER_BINARY_PLUGIN_DIR ${ORBITER_BINARY_MODULE_DIR}/Plugin)
set(ORBITER_BINARY_VESSEL_DIR ${ORBITER_BINARY_MODULE_DIR})
set(ORBITER_BINARY_TEXTURES_DIR ${ORBITER_BINARY_ROOT_DIR}/Textures)
set(ORBITER_BINARY_TEXTURES2_DIR ${ORBITER_BINARY_ROOT_DIR}/Textures2)
set(ORBITER_BINARY_HTML_DIR ${ORBITER_BINARY_ROOT_DIR}/Html)
set(ORBITER_BINARY_DOC_DIR ${ORBITER_BINARY_ROOT_DIR}/Doc)
set(ORBITER_BINARY_SDK_DIR ${ORBITER_BINARY_ROOT_DIR}/Orbitersdk)

set(ORBITER_INSTALL_ROOT_DIR "Orbiter")
set(ORBITER_INSTALL_MESH_DIR ${ORBITER_INSTALL_ROOT_DIR}/Meshes)
set(ORBITER_INSTALL_MODULE_DIR ${ORBITER_INSTALL_ROOT_DIR}/Modules)
set(ORBITER_INSTALL_CELBODY_DIR ${ORBITER_INSTALL_MODULE_DIR}/Celbody)
set(ORBITER_INSTALL_PLUGIN_DIR ${ORBITER_INSTALL_MODULE_DIR}/Plugin)
set(ORBITER_INSTALL_TEXTURES_DIR ${ORBITER_INSTALL_ROOT_DIR}/Textures)
set(ORBITER_INSTALL_TEXTURES2_DIR ${ORBITER_INSTALL_ROOT_DIR}/Textures2)
set(ORBITER_INSTALL_HTML_DIR  ${ORBITER_INSTALL_ROOT_DIR}/Html)
set(ORBITER_INSTALL_DOC_DIR ${ORBITER_INSTALL_ROOT_DIR}/Doc)
set(ORBITER_INSTALL_UTILS_DIR ${ORBITER_INSTALL_ROOT_DIR}/Utils)
set(ORBITER_INSTALL_SDK_DIR ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk)

set(EXTERN_DIR "${CMAKE_SOURCE_DIR}/Extern")

set(ORBITER_TOOL_MESHC "$<TARGET_FILE:meshc>")
file(GENERATE OUTPUT ${ORBITER_BINARY_ROOT_DIR}/tools/ldoc.sh CONTENT "#!/bin/sh
export LUA_PATH='${ORBITER_BINARY_ROOT_DIR}/?.lua;${ORBITER_BINARY_ROOT_DIR}/?/init.lua;;'
export LUA_CPATH='${ORBITER_BINARY_ROOT_DIR}/?.so;;'
exec '$<TARGET_FILE:lua::exe>' '${ORBITER_BINARY_ROOT_DIR}/packages/LDoc/ldoc.lua' -v --multimodule \"$@\"
" FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
set(ldoc ${ORBITER_BINARY_ROOT_DIR}/tools/ldoc.sh)

find_package(LATEX)

# Toolchain settings shared by the core and every module. The Windows sources
# compile against the Win32 subset in Orbitersdk/include/compat, which is backed
# by the OrbiterPlatform library.
include_directories(BEFORE ${ORBITER_COMPAT_INCLUDE_DIR})
add_compile_definitions(NOMINMAX _USE_MATH_DEFINES)
add_compile_options(
	-fdeclspec
	-Wno-ignored-attributes
	-Wno-unknown-attributes
	-Wno-unknown-pragmas
	-Wno-writable-strings
	-Wno-deprecated-declarations
	-Wno-invalid-source-encoding
	"$<$<COMPILE_LANGUAGE:CXX,OBJCXX>:-Wno-c++11-narrowing>"
	"$<$<COMPILE_LANGUAGE:CXX,OBJCXX>:-Wno-register>"
	"$<$<COMPILE_LANGUAGE:CXX,OBJCXX>:-Wno-reorder-ctor>"
	"$<$<COMPILE_LANGUAGE:C,OBJC>:-Wno-implicit-function-declaration>"
	-Wno-switch
	-Wno-unused-value
	-Wno-dangling-else
	-Wno-parentheses
	-Wno-format
	-Wno-nonportable-include-path
	-Wno-microsoft-template
	-Wno-pragma-pack
	-Wno-logical-op-parentheses
	-Wno-tautological-constant-out-of-range-compare
	-Wno-shift-negative-value
	-Wno-unused-command-line-argument
)

set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(CMAKE_INSTALL_RPATH "@executable_path;@executable_path/Modules;@executable_path/../../..;@executable_path/../../../Modules")
set(CMAKE_SHARED_LIBRARY_SUFFIX ".dylib")
set(CMAKE_SHARED_LIBRARY_PREFIX "")

# Copy data and binary assets from source to binary tree to provide a working Orbiter environment
add_custom_target(CopyData ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/BinAssets/ ${CMAKE_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Scenarios/ ${CMAKE_BINARY_DIR}/Scenarios
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Textures/ ${ORBITER_BINARY_TEXTURES_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Meshes/ ${CMAKE_BINARY_DIR}/Meshes
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Script/ ${CMAKE_BINARY_DIR}/Script
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/GravityModels/ ${CMAKE_BINARY_DIR}/GravityModels
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Kernels/ ${CMAKE_BINARY_DIR}/Kernels
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Flights/ ${CMAKE_BINARY_DIR}/Flights
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/lib/Lua
)
set_target_properties(CopyData
	PROPERTIES
	FOLDER Data
)

set(OrbiterTgt Orbiter)

set(ORBITER_LIB OrbiterCore "-Wl,-undefined,dynamic_lookup")
set(ORBITER_SDK_LIB "-Wl,-force_load,$<TARGET_FILE:Orbitersdk>" OrbiterPlatform)
set(LUAINTERPRETER_LIB $<TARGET_LINKER_FILE:LuaInterpreter>)

option(ORBITER_SPICE "Use SPICE ephemerides for celestial bodies (needs Kernels/orbiter.bsp)" ON)
set(SPICE_BUILD ${ORBITER_SPICE})
if(SPICE_BUILD AND NOT EXISTS ${CMAKE_SOURCE_DIR}/Kernels/orbiter.bsp)
	message(WARNING "Kernels/orbiter.bsp not found (build it with spkmerge from Kernels/orbiter.mrg). Using the VSOP87/ELP82 celestial bodies instead.")
	set(SPICE_BUILD FALSE)
endif()

set(ORBITER_PLANET_TEXTURE_INSTALL_DIR
	${ORBITER_BINARY_ROOT_DIR}/Textures
	CACHE
	PATH
	"Planet texture installation directory"
)

if(DEFINED ENV{ORBITER_PLANET_TEXTURE_INSTALL_DIR})
	set(ORBITER_PLANET_TEXTURE_INSTALL_DIR $ENV{ORBITER_PLANET_TEXTURE_INSTALL_DIR})
	message(STATUS "Using environment: ORBITER_PLANET_TEXTURE_INSTALL_DIR=${ORBITER_PLANET_TEXTURE_INSTALL_DIR}")
endif()

string(COMPARE NOTEQUAL ${ORBITER_PLANET_TEXTURE_INSTALL_DIR} "${ORBITER_BINARY_ROOT_DIR}/Textures" CUSTOM_PLANET_TEXTURE_DIR)

if (${CUSTOM_PLANET_TEXTURE_DIR})
	set(ORBITER_PLANET_TEXTURE_INSTALL_DIR_W "${ORBITER_PLANET_TEXTURE_INSTALL_DIR}")
	configure_file(${ORBITER_SOURCE_DIR}/Orbiter.cfg.in ${ORBITER_BINARY_ROOT_DIR}/Orbiter.cfg)
	install(FILES ${ORBITER_BINARY_ROOT_DIR}/Orbiter.cfg DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
endif()

option(ORBITER_BUILD_METALCLIENT
	"Build the Metal graphics client"
	ON
)

option(ORBITER_BUILD_XRSOUND
	"Build XRSound module to enable sound in Orbiter"
	ON
)

option(ORBITER_MAKE_DOC
	"Create documentation"
	OFF
)

option(ORBITER_MAKE_TESTS
	"Build unit tests"
	ON
)

option(ORBITER_SANITIZER
	"Build binaries with Address Sanitizer"
	OFF
)

option(ORBITER_TRACY_PROFILER
	"Build orbiter with profiler support"
	OFF
)

include(cmake/tracy.cmake)
include(cmake/OrbiterHelp.cmake)

if (ORBITER_MAKE_DOC)
	find_package(Doxygen REQUIRED)
endif()

include(cmake/sanitizer.cmake)
if (ORBITER_SANITIZER)
	enable_sanitizer(${ORBITER_SANITIZER})
endif()

# Given a source directory (srcdir) and a target root directory (tgtroot),
# generate a list of all files found in srcdir (srclist) and a list of output files
# when the source directory is copied to the target root (tgtlist)
function(srcdir_to_tgtlist srcdir tgtroot srclist tgtlist)
	file(GLOB_RECURSE infiles
		RELATIVE ${srcdir}
		${srcdir}/*
	)
	set(local_srclist ${infiles})
	list(TRANSFORM local_srclist PREPEND "${srcdir}/")
	set(local_tgtlist ${infiles})
	list(TRANSFORM local_tgtlist PREPEND "${tgtroot}/")
	set(srclist ${local_srclist} PARENT_SCOPE)
	set(tgtlist ${local_tgtlist} PARENT_SCOPE)
endfunction()

install(DIRECTORY Scenarios Textures Meshes Config GravityModels Kernels Script Flights BinAssets/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)

set(CMAKE_MODULE_PATH
	${ORBITER_BINARY_ROOT_DIR}
	${CMAKE_MODULE_PATH}
)

install(FILES readme.txt LICENSE
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)

add_subdirectory(Extern)
add_subdirectory(Src)
add_subdirectory(Utils)
add_subdirectory(Orbitersdk)
add_subdirectory(OVP)
add_subdirectory(Sound)
add_subdirectory(Html)

if(ORBITER_MAKE_DOC)
	add_subdirectory(Doc)
endif()

if (ORBITER_MAKE_TESTS)
	enable_testing()
	add_subdirectory(Tests)
endif()

# Modules are shared libraries, which CMake places by LIBRARY_OUTPUT_DIRECTORY
# on macOS. The module CMake files only set RUNTIME_OUTPUT_DIRECTORY (where
# Windows puts DLLs), so carry that over.
function(orbiter_fix_module_outputs dir)
	get_property(targets DIRECTORY ${dir} PROPERTY BUILDSYSTEM_TARGETS)
	foreach(tgt ${targets})
		get_target_property(type ${tgt} TYPE)
		if(type STREQUAL "SHARED_LIBRARY" OR type STREQUAL "MODULE_LIBRARY")
			get_target_property(rtdir ${tgt} RUNTIME_OUTPUT_DIRECTORY)
			get_target_property(libdir ${tgt} LIBRARY_OUTPUT_DIRECTORY)
			if(rtdir AND NOT libdir)
				set_target_properties(${tgt} PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${rtdir})
			endif()
		endif()
	endforeach()
	get_property(subdirs DIRECTORY ${dir} PROPERTY SUBDIRECTORIES)
	foreach(sub ${subdirs})
		orbiter_fix_module_outputs(${sub})
	endforeach()
endfunction()
orbiter_fix_module_outputs(${CMAKE_SOURCE_DIR})
