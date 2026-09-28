# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)
endif() # WIN32

add_library(Galsat SHARED
	Galsat.cpp
	Lieske.cpp
)

set_target_properties(Galsat
	PROPERTIES
	FOLDER Celbody/Galsat
)

add_dependencies(Galsat
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(Galsat
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(Galsat
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET Galsat PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/Config/Jupiter/Data
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/ephem_e15.dat ${CMAKE_BINARY_DIR}/Config/Jupiter/Data/
)

if(WIN32) # Windows: upstream
set(GALSAT_LIB $<TARGET_LINKER_FILE:Galsat>)
else() # Linux port
set(GALSAT_LIB Galsat) # target name: CMake links a raw path to a prefix-less .so as -l<name>
endif() # WIN32

add_subdirectory(Io)
add_subdirectory(Europa)
add_subdirectory(Ganymede)
add_subdirectory(Callisto)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS
	Galsat
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS
	Galsat
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32
install(FILES ephem_e15.dat
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Jupiter/Data
)
