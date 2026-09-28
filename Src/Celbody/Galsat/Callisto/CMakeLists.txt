# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(CELBODY "Callisto")

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)
endif() # WIN32

add_library(${CELBODY} SHARED
	${CELBODY}.cpp
)

add_dependencies(${CELBODY}
	Galsat
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(${CELBODY}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..
)

target_link_libraries(${CELBODY}
	${GALSAT_LIB}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)

set_target_properties(${CELBODY}
	PROPERTIES
	FOLDER Celbody/Galsat
)

#Installation
if(WIN32) # Windows: upstream
install(TARGETS
	${CELBODY}
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_CELBODY_DIR}
)
else() # Linux port
install(TARGETS
	${CELBODY}
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_CELBODY_DIR}
)
endif() # WIN32
install(DIRECTORY Config
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
