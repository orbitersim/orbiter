# Copyright (c) Matthew Hume
# Licensed under the MIT License

#add_subdirectory(cspice)

set(CELBODY "spice")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)

# CSPICE N0067 for Apple Silicon, from the NAIF toolkit (MacM1_OSX_clang_64bit)
set(SPICE_PLATFORM "cspice/macos_arm64")
set(SPICE_LIB_EXTENSION ".a")

add_library(${CELBODY} SHARED
  spice.cpp  
)

add_dependencies(${CELBODY}
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(${CELBODY}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include/
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
