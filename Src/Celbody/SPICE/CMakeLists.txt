# Copyright (c) Matthew Hume
# Licensed under the MIT License

#add_subdirectory(cspice)

set(CELBODY "spice")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)

add_library(${CELBODY} SHARED
	${CELBODY}.cpp
	cspice/win_x86/include/SpiceUsr.h
)

add_dependencies(${CELBODY}
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(${CELBODY}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(${CELBODY}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${CMAKE_SOURCE_DIR}/Src/Celbody/SPICE/cspice/win_x86/lib/cspice.lib
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
