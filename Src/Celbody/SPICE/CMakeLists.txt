# Copyright (c) Matthew Hume
# Licensed under the MIT License

#add_subdirectory(cspice)

set(CELBODY "spice")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)

if(NOT BUILD64 AND CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  set(SPICE_PLATFORM "cspice/win_x86/")
  set(SPICE_LIB_EXTENSION ".lib")
endif()
if(NOT BUILD64 AND CMAKE_CXX_COMPILER_ID STREQUAL "GNUCXX")
	set(SPICE_PLATFORM "cspice/linux_x86/")
  set(SPICE_LIB_EXTENSION ".so")
endif()
if(BUILD64 AND CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
	set(SPICE_PLATFORM "cspice/win_x64/")
  set(SPICE_LIB_EXTENSION ".lib")
endif()
if(BUILD64 AND CMAKE_CXX_COMPILER_ID STREQUAL "GNUCXX")
	set(SPICE_PLATFORM "cspice/linux_x64/")
  set(SPICE_LIB_EXTENSION ".so")
endif()

add_library(${CELBODY} SHARED
  spice.cpp  
)

add_dependencies(${CELBODY}
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(${CELBODY}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	${CMAKE_SOURCE_DIR}/Src/Celbody/SPICE/${SPICE_PLATFORM}/include/
)

target_link_libraries(${CELBODY}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${CMAKE_SOURCE_DIR}/Src/Celbody/SPICE/${SPICE_PLATFORM}/lib/cspice${SPICE_LIB_EXTENSION}
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)

set_target_properties(${CELBODY}
	PROPERTIES
	FOLDER Celbody
)

#Installation
install(TARGETS
	${CELBODY}
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_CELBODY_DIR}
)
install(DIRECTORY Config
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
