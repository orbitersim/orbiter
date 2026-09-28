# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.15)

project (plsplit VERSION 2021.1)

add_executable(plsplit
	plsplit.cpp
)

if(NOT WIN32) # Linux port
# not upstream: OrbiterPlatform.h for the windows.h integer types; WIC (ReadPNG) -> libpng
find_package(PNG REQUIRED)
target_include_directories(plsplit PRIVATE ${ORBITER_SOURCE_SDK_INCLUDE_DIR})
target_link_libraries(plsplit PNG::PNG)

endif() # NOT WIN32
if(WIN32) # Windows: upstream
set_target_properties(plsplit
	PROPERTIES
	LINK_FLAGS "/SUBSYSTEM:CONSOLE"
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_BINARY_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_BINARY_DIR}
)
else() # Linux port
set_target_properties(plsplit
	PROPERTIES
	# LINK_FLAGS "/SUBSYSTEM:CONSOLE" left out: MSVC linker flag, ELF has no subsystem
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_BINARY_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_BINARY_DIR}
)
endif() # WIN32

if(WIN32) # Windows: upstream
add_custom_command(
	TARGET plsplit PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/DxTex.exe ${CMAKE_CURRENT_BINARY_DIR}
)
else() # Linux port
# not upstream: dxtex replaces the Windows DxTex.exe (BMP -> DXT1/DXT5 .dds for plsplit and pltex), built on tileedit's fastdxt
add_executable(dxtex
	dxtex.cpp
)
target_include_directories(dxtex PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../tileedit/qt/extern/fastdxt)
target_link_libraries(dxtex fastdxt)
set_target_properties(dxtex
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_BINARY_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_BINARY_DIR}
)
endif() # WIN32

if(WIN32) # Windows: upstream
# Installation - this is currently skipped
else() # Linux port
# PRE_BUILD copy of DxTex.exe replaced by the dxtex target: it builds next to plsplit
endif() # WIN32

if(NOT WIN32) # Linux port
# Installation - this is currently skipped
# INSTALLDIR came from the parent ExternalProject (left out): ORBITER_INSTALL_UTILS_DIR, where the parent installed plsplit
endif() # NOT WIN32
if(WIN32) # Windows: upstream
install(TARGETS
	plsplit
	RUNTIME
	DESTINATION ${INSTALLDIR}
)
install(FILES DxTex.exe
	DESTINATION ${INSTALLDIR}
)
else() # Linux port
install(TARGETS
	plsplit
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)
endif() # WIN32
if(NOT WIN32) # Linux port
# install(FILES DxTex.exe) replaced by dxtex, installed where the parent installed DxTex.exe
install(TARGETS dxtex RUNTIME DESTINATION ${ORBITER_INSTALL_UTILS_DIR}) # not upstream: dxtex replaces DxTex.exe
endif() # NOT WIN32
