# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.19)

# Set the project name
project (Orbiter VERSION 21.7.24)

if(WIN32) # Windows: upstream
set(CMAKE_CXX_STANDARD 17)
else() # Linux port
# not upstream: C++20 per the port plan (upstream uses 17)
set(CMAKE_CXX_STANDARD 20)
endif() # WIN32
set(CMAKE_CXX_STANDARD_REQUIRED OFF)
set(CMAKE_CXX_EXTENSIONS OFF)

if(WIN32) # Windows: upstream
#Add /permissive if using C++20 or higher
#add_compile_options(/permissive)
else() # Linux port
# upstream's /permissive note is MSVC-only
endif() # WIN32

# Some functions to simplify debugging CMake scripts
include(CMakePrintHelpers)

# Support for including runtime libraries in packages
set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP TRUE)
include(InstallRequiredSystemLibraries)

# Set the pools for compilers that can't run concurrently
set_property(GLOBAL PROPERTY JOB_POOLS latex=1 soffice=1 htmlhelp=1)

# Group projects in VS
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Locally defined cmake modules
set(CMAKE_MODULE_PATH
	${CMAKE_SOURCE_DIR}/cmake
	${CMAKE_MODULE_PATH}
)

if(WIN32) # Windows: upstream
if(MSVC AND NOT CMAKE_SIZEOF_VOID_P EQUAL 8)
	set(BUILD64 0)
	set(ARCH "x86")
  add_link_options("/LARGEADDRESSAWARE")
else()
	set(BUILD64 1)
	set(ARCH "x64")
endif()
else() # Linux port
# 32-bit MSVC branch left out: the Linux build is x86_64 only
set(BUILD64 1)
set(ARCH "x64")
endif() # WIN32

if(WIN32) # Windows: upstream
# This is important for building using vs generator (jarmonik)
if (NOT ${CMAKE_GENERATOR} STREQUAL "Ninja")
	add_compile_options(/MP)
endif()
else() # Linux port
# /MP left out: Ninja and make already compile in parallel
endif() # WIN32

# Define directories and file paths for some common binaries
set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")

set(ORBITER_SOURCE_ROOT_DIR ${CMAKE_SOURCE_DIR})
set(ORBITER_SOURCE_DIR ${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter)
set(ORBITER_SOURCE_MODULE_DIR ${ORBITER_SOURCE_ROOT_DIR}/Src/Plugin)
set(ORBITER_SOURCE_SDK_DIR ${ORBITER_SOURCE_ROOT_DIR}/Orbitersdk)
set(ORBITER_SOURCE_SDK_INCLUDE_DIR ${ORBITER_SOURCE_SDK_DIR}/include)
set(ORBITER_SOURCE_HTML_DIR ${ORBITER_SOURCE_ROOT_DIR}/Html)

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
if(NOT WIN32) # Linux port
# not upstream: Orbiter scans Modules/Startup at launch, so the folder exists before any module lands in it
file(MAKE_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR} ${ORBITER_BINARY_PLUGIN_DIR})
endif() # NOT WIN32

set(ORBITER_INSTALL_ROOT_DIR "Orbiter") # This path is relative to CMAKE_INSTALL_PREFIX
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

set(ORBITER_SDK_LIB $<TARGET_FILE:Orbitersdk>)
if(WIN32) # Windows: upstream
set(ORBITER_DLGCTRL_LIB $<TARGET_FILE:DlgCtrl>)
set(LUAINTERPRETER_LIB $<TARGET_LINKER_FILE:LuaInterpreter>)
else() # Linux port
# not upstream: by target name (a path to a prefix-less .so links as -l<name>; the target also carries Qt)
set(ORBITER_DLGCTRL_LIB DlgCtrl)
set(LUAINTERPRETER_LIB LuaInterpreter)
endif() # WIN32

set(EXTERN_DIR "${CMAKE_SOURCE_DIR}/Extern")

if(WIN32) # Windows: upstream
set(HTMLHELP_DIR "${EXTERN_DIR}/Htmlhelp")
set(HTMLHELP_INCLUDE_DIR ${HTMLHELP_DIR}/include)
set(HTMLHELP_LIB_DIR ${HTMLHELP_DIR}/lib-${ARCH})
set(HTMLHELP_LIB ${HTMLHELP_LIB_DIR}/Htmlhelp.Lib)
else() # Linux port
# HTMLHELP_* left out: CHM help is replaced by opening the Html pages
endif() # WIN32

set(ORBITER_TOOL_MESHC "$<TARGET_FILE:meshc>")
if(WIN32) # Windows: upstream
set(ldoc $<TARGET_FILE:lua::exe> ${ORBITER_BINARY_ROOT_DIR}/packages/LDoc/ldoc.lua -v --multimodule)
else() # Linux port
# packages/ldoc: CopyLDoc writes lower case, upstream's LDoc only resolves on a case-insensitive disk
# LUA_PATH/LUA_CPATH: Windows Lua also searches its own folder ('!' in its default paths), where CopyLDoc puts Penlight and lfs
set(ldoc ${CMAKE_COMMAND} -E env "LUA_PATH='${ORBITER_BINARY_ROOT_DIR}/?.lua$<SEMICOLON>${ORBITER_BINARY_ROOT_DIR}/?/init.lua$<SEMICOLON>$<SEMICOLON>'"
	"LUA_CPATH='${ORBITER_BINARY_ROOT_DIR}/?.so$<SEMICOLON>$<SEMICOLON>'" $<TARGET_FILE:lua::exe> ${ORBITER_BINARY_ROOT_DIR}/packages/ldoc/ldoc.lua -v --multimodule)
endif() # WIN32

if(WIN32) # Windows: upstream
find_package(OpenGL QUIET)
find_package(HTMLHelp)
else() # Linux port
# OpenGL lookup left out: graphics are Vulkan only
# HTMLHelp and MFC lookups left out: Windows-only
endif() # WIN32
find_package(LATEX)
if(WIN32) # Windows: upstream
find_package(MFC)
endif() # WIN32

if(WIN32) # Windows: upstream
# We don't query Qt with find_package because we need the 64-bit version here
set(Qt5_x64_DIR
	"Qt5_x64_DIR_NOTFOUND"
	CACHE
	PATH
	"Qt5-x64 installation path."
)
else() # Linux port
# Qt6 replaces the Qt5_x64 lookup and also carries the Launchpad and dialogs
find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets DBus)
endif() # WIN32

if(WIN32) # Windows: upstream
if(MSVC)
  add_compile_options(/we4311 /DNOMINMAX)
  add_link_options("/NODEFAULTLIB:\"LIBCMTD\"")
else()
  set(CMAKE_BUILD_WITH_INSTALL_RPATH true)
endif()
else() # Linux port
# /we4311 left out: g++ already rejects pointer-to-smaller-int casts; NOMINMAX has no counterpart
set(CMAKE_BUILD_WITH_INSTALL_RPATH true)
# Windows finds DLLs next to the exe and in Modules (Orbiter adds it to PATH); $ORIGIN/.. reaches Modules from Modules/<sub>, $ORIGIN/../.. the exe folder
set(CMAKE_INSTALL_RPATH "$ORIGIN" "$ORIGIN/.." "$ORIGIN/../..")
# DLL naming: modules are <name>.so, as upstream loads <name>.dll
set(CMAKE_SHARED_LIBRARY_PREFIX "")
set(CMAKE_SHARED_MODULE_PREFIX "")
endif() # WIN32

# Copy data and binary assets from source to binary tree to provide a working Orbiter environment
if(NOT WIN32) # Linux port
# copy_directory_if_different: skips re-copying 228 MB of textures on every build
# BinAssets copy left out: msvcr71/msvcp71.dll are the Windows runtime for old add-ons
endif() # NOT WIN32
if(WIN32) # Windows: upstream
add_custom_target(CopyData ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/BinAssets/ ${CMAKE_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Scenarios/ ${CMAKE_BINARY_DIR}/Scenarios
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Textures/ ${ORBITER_BINARY_TEXTURES_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Meshes/ ${CMAKE_BINARY_DIR}/Meshes
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Script/ ${CMAKE_BINARY_DIR}/Script
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/GravityModels/ ${CMAKE_BINARY_DIR}/GravityModels
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Flights/ ${CMAKE_BINARY_DIR}/Flights
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/lib/Lua
)
else() # Linux port
add_custom_target(CopyData ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/Scenarios/ ${CMAKE_BINARY_DIR}/Scenarios
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/Textures/ ${ORBITER_BINARY_TEXTURES_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/Meshes/ ${CMAKE_BINARY_DIR}/Meshes
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/Script/ ${CMAKE_BINARY_DIR}/Script
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/GravityModels/ ${CMAKE_BINARY_DIR}/GravityModels
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${CMAKE_SOURCE_DIR}/Flights/ ${CMAKE_BINARY_DIR}/Flights
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/lib/Lua
)
endif() # WIN32
set_target_properties(CopyData
	PROPERTIES
	FOLDER Data
)

if(WIN32) # Windows: upstream
# =====================================================================================
# work around Html Help compiler bug returning error code

set(HHC_CMD ${CMAKE_CURRENT_BINARY_DIR}/cmake/hhc_fix.bat)
file(TO_NATIVE_PATH "${HTML_HELP_COMPILER}" HHC_PATH_W)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/hhc_fix.bat.in ${HHC_CMD})

else() # Linux port
# hhc_fix.bat left out: hhc.py below always exits 0
# not upstream: the Html Help compiler is cmake/hhc.py; its .chm is a zip of the project's pages, which Orbiter's help viewer reads
find_package(Python3 REQUIRED COMPONENTS Interpreter)
set(HHC_CMD ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/cmake/hhc.py)
endif() # WIN32

# =====================================================================================
# work around pdftex returning an error because of doxygen bug

if(WIN32) # Windows: upstream
set(PDFTEX_CMD ${CMAKE_CURRENT_BINARY_DIR}/cmake/pdftex_fix.bat)
file(TO_NATIVE_PATH "${PDFLATEX_COMPILER}" PDFTEX_PATH_W)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/pdftex_fix.bat.in ${PDFTEX_CMD})


# =====================================================================================
else() # Linux port
set(PDFTEX_CMD ${CMAKE_CURRENT_BINARY_DIR}/cmake/pdftex_fix.sh) # pdftex_fix.bat as a shell script
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/pdftex_fix.sh.in ${PDFTEX_CMD} @ONLY)
endif() # WIN32

set(OrbiterTgt Orbiter)
if(WIN32) # Windows: upstream
set(ORBITER_LIB $<TARGET_LINKER_FILE:Orbiter>)
else() # Linux port
set(ORBITER_LIB "") # no import library on ELF: modules bind the exe's exported oapi* symbols at dlopen
endif() # WIN32

# =====================================================================================
# Configure Orbiter.cfg for externally installed planet textures

if(WIN32) # Windows: upstream
set(ORBITER_PLANET_TEXTURE_INSTALL_DIR
	${ORBITER_BINARY_ROOT_DIR}/Textures
	CACHE
	PATH
	"Planet texture installation directory (e.g. C:/Orbiter2016/Textures)"
)
else() # Linux port
set(ORBITER_PLANET_TEXTURE_INSTALL_DIR
	${ORBITER_BINARY_ROOT_DIR}/Textures
	CACHE
	PATH
	"Planet texture installation directory (e.g. ~/Orbiter2016/Textures)"
)
endif() # WIN32

if(DEFINED ENV{ORBITER_PLANET_TEXTURE_INSTALL_DIR})
  set(ORBITER_PLANET_TEXTURE_INSTALL_DIR $ENV{ORBITER_PLANET_TEXTURE_INSTALL_DIR})
  message(STATUS "Using environment: ORBITER_PLANET_TEXTURE_INSTALL_DIR=${ORBITER_PLANET_TEXTURE_INSTALL_DIR}")
endif()

string(COMPARE NOTEQUAL ${ORBITER_PLANET_TEXTURE_INSTALL_DIR} "${ORBITER_BINARY_ROOT_DIR}/Textures" CUSTOM_PLANET_TEXTURE_DIR)

if (${CUSTOM_PLANET_TEXTURE_DIR})
	string(REPLACE  "/" "\\" ORBITER_PLANET_TEXTURE_INSTALL_DIR_W "${ORBITER_PLANET_TEXTURE_INSTALL_DIR}")

	configure_file(${ORBITER_SOURCE_DIR}/Orbiter.cfg.in ${ORBITER_BINARY_ROOT_DIR}/Orbiter.cfg)
	install(FILES ${ORBITER_BINARY_ROOT_DIR}/Orbiter.cfg DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
endif()


# =====================================================================================
# Find required and optional build tools

if(WIN32) # Windows: upstream
option(
	ORBITER_BUILD_D3D9CLIENT
	"Build the D3D9 graphics client"
	ON
)
else() # Linux port
option(
	ORBITER_BUILD_VULKANCLIENT
	"Build the Vulkan graphics client"
	ON
)
endif() # WIN32

option(
	ORBITER_BUILD_XRSOUND
	"Build XRSound module to enable sound in Orbiter"
	ON
)

option(ORBITER_MAKE_DOC
	"Create documentation"
	OFF
)

if(NOT WIN32) # Linux port
# not upstream: the help files need only Python here, so they come with every build
option(ORBITER_MAKE_HELP
	"Create the help files (.chm)"
	ON
)

endif() # NOT WIN32
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

if(NOT WIN32) # Linux port
# PipeWire replaces the irrKlang library
endif() # NOT WIN32
if(ORBITER_BUILD_XRSOUND)
if(WIN32) # Windows: upstream
	set(IRRKLANG_DIR "" CACHE PATH "Path to the irrKlang library")
else() # Linux port
	find_package(PkgConfig REQUIRED)
	pkg_check_modules(PIPEWIRE REQUIRED IMPORTED_TARGET libpipewire-0.3)
endif() # WIN32
endif()

if(WIN32) # Windows: upstream
if(ORBITER_BUILD_D3D9CLIENT)
	find_package(DXSDK REQUIRED)
	set(DXSDK_DIR
		${DXSDK_DIR}
		CACHE
		PATH
		"Installation directory of DirectX SDK (June 2010)"
	)
	if(${DXSDK_DIR} STREQUAL "DXSDK_DIR-NOTFOUND")
		message(FATAL_ERROR "DirectX SDK not found. The D3D9 graphics client will not be built.")
	endif()
endif()
else() # Linux port
# Vulkan 1.4 and glslangValidator replace the DirectX SDK (June 2010)
if(ORBITER_BUILD_VULKANCLIENT)
	find_package(Vulkan 1.4 REQUIRED COMPONENTS glslangValidator)
endif()
endif() # WIN32

if (ORBITER_MAKE_DOC)
	find_package(Doxygen REQUIRED)
endif()

if(NOT WIN32) # Linux port
# not upstream: .rc scripts compile to C++ resource tables
include(cmake/rcresources.cmake)

endif() # NOT WIN32
# Enable Address SANitizer
include(cmake/sanitizer.cmake)
if(WIN32) # Windows: upstream
if (ORBITER_SANITIZER AND MSVC)
	enable_sanitizer(${ORBITER_SANITIZER})
endif()
else() # Linux port
# the option is ON/OFF, so the sanitizer name is passed here
if (ORBITER_SANITIZER)
	enable_sanitizer(address)
endif()
endif() # WIN32


# Given a source directory (srcdir) and a target root directory (tgtroot),
# generate a list of all files found in srcdir (srclist) and a list of output files
# when the source directory is copied to the target root (tgtlist)
# For a copy_directory command, srclist can be used as dependencies, and tgtlist as output

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


# Installation
if(WIN32) # Windows: upstream
install(DIRECTORY Scenarios Textures Meshes Config GravityModels Script Flights BinAssets/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
else() # Linux port
# BinAssets left out: it only holds msvcr71/msvcp71.dll, the Windows runtime for old add-ons
install(DIRECTORY Scenarios Textures Meshes Config GravityModels Script Flights
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
endif() # WIN32

set(CMAKE_MODULE_PATH
	${ORBITER_BINARY_ROOT_DIR}
    ${CMAKE_MODULE_PATH}
)

# Package required runtime libraries
INSTALL(FILES ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
	COMPONENT RuntimeLibraries
)

# readme and license
if(WIN32) # Windows: upstream
INSTALL(FILES readme.txt
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
else() # Linux port
INSTALL(FILES readme_linux.txt
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
	RENAME readme.txt
)
endif() # WIN32
INSTALL(FILES LICENSE
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
#COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/Doc/TransX\ readme.txt ${ORBITER_BINARY_DOC_DIR}/TransX/TransX\ readme.txt

# Sub-projects
add_subdirectory(Extern)
if(NOT WIN32) # Linux port
# only DLLEXPORT/DLLCLBK/OAPIFUNC symbols leave the exe or a module, as with Windows DLLs
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
endif() # NOT WIN32
add_subdirectory(Src)
add_subdirectory(Utils)
add_subdirectory(Orbitersdk)
add_subdirectory(OVP)
add_subdirectory(Sound)
add_subdirectory(Html)

if(ORBITER_MAKE_DOC)
	add_subdirectory(Doc)
endif()

# =============================================================================
# Set up Distribution package options

configure_file(${CMAKE_SOURCE_DIR}/cmake/CPackOptions.cmake.in
	${CMAKE_BINARY_DIR}/OrbiterCPackOptions.cmake)

if(WIN32) # Windows: upstream
set(CPACK_GENERATOR "WIX;ZIP")
else() # Linux port
# TGZ replaces the WIX installer and ZIP
set(CPACK_GENERATOR "TGZ")
endif() # WIN32
set(CPACK_PACKAGE_NAME OpenOrbiter)
set(CPACK_PACKAGE_VENDOR Orbitersim)
if(WIN32) # Windows: upstream
set(CPACK_PACKAGE_DESCRIPTION "Orbiter is an Open-source spaceflight simulator for Windows PCs.")
else() # Linux port
set(CPACK_PACKAGE_DESCRIPTION "Orbiter is an Open-source spaceflight simulator for Linux PCs.")
endif() # WIN32
set(CPACK_PACKAGE_HOMEPAGE_URL http://orbit.medphys.ucl.ac.uk)
set(CPACK_PACKAGE_EXECUTABLES Orbiter;OpenOrbiter)
if(NOT WIN32) # Linux port
# not upstream: the OpenOrbiter shortcut is a launcher script next to Orbiter (a tarball has no start menu)
install(PROGRAMS cmake/OpenOrbiter.sh DESTINATION ${ORBITER_INSTALL_ROOT_DIR} RENAME OpenOrbiter)
# not upstream: deps.list, what OpenOrbiter checks and offers to install, taken from the installed programs (the RuntimeLibraries counterpart)
install(CODE "execute_process(COMMAND bash \"${ORBITER_SOURCE_ROOT_DIR}/cmake/deps_list.sh\" \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${ORBITER_INSTALL_ROOT_DIR}\")")
endif() # NOT WIN32
set(CPACK_COMPONENTS_GROUPING ALL_COMPONENTS_IN_ONE)
set(CPACK_COMPONENT_Unspecified_DISPLAY_NAME "Orbiter Core Components")
set(CPACK_COMPONENT_Unspecified_REQUIRED TRUE)
set(CPACK_COMPONENT_RuntimeLibraries_DESCRIPTION "System libraries used by Orbiter. Install if Orbiter fails to launch.")
set(CPACK_RESOURCE_FILE_LICENSE ${ORBITER_SOURCE_ROOT_DIR}/cmake/LICENSE.rtf)
set(CPACK_RESOURCE_FILE_README ${ORBITER_SOURCE_ROOT_DIR}/README.md)
if(WIN32) # Windows: upstream
set(CPACK_WIX_UI_BANNER ${ORBITER_SOURCE_ROOT_DIR}/cmake/install_banner.bmp)
set(CPACK_WIX_UI_DIALOG ${ORBITER_SOURCE_ROOT_DIR}/cmake/install_bkg.bmp)
set(CPACK_WIX_PRODUCT_ICON ${ORBITER_SOURCE_ROOT_DIR}/cmake/orbiter_install.ico)
set(CPACK_WIX_UPGRADE_GUID AB3F6483-A1A4-4ECD-ADAE-D33DF3A5AADA)
set(CPACK_WIX_COMPONENT_INSTALL)
else() # Linux port
# CPACK_WIX_* (banner, dialog, icon, upgrade GUID) left out: no WIX installer on Linux
endif() # WIN32
set(CPACK_PROJECT_CONFIG_FILE "${CMAKE_BINARY_DIR}/OrbiterCPackOptions.cmake")

include(OrbiterCPackOptions)
include(CPack)

cpack_add_component(Unspecified
	DISPLAY_NAME "OrbiterCore"
	DESCRIPTION "Orbiter Core Components"
	REQUIRED
)
cpack_add_component(RuntimeLibraries
	DISPLAY_NAME "RuntimeLibraries"
	DESCRIPTION "System libraries used by Orbiter. Install if Orbiter fails to launch."
)

# There appears to be a CMake (3.20.5) bug that prevents CMAKE_INSTALL_PREFIX to be set
# correctly in the CPACK_PRE_BUILD_SCRIPTS, so the script has been disabled for now
# This means that the planet texture installation directory is not reset to default
# for the packages. This needs to be fixed

#set(CPACK_PRE_BUILD_SCRIPTS ${ORBITER_SOURCE_ROOT_DIR}/cmake/cpack_install.cmake)

if (ORBITER_MAKE_TESTS)
      enable_testing()
      add_subdirectory(Tests)
endif()
