# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)
endif() # WIN32

add_library(Satsat SHARED
	Satsat.cpp
	Tass17.cpp
)

set_target_properties(Satsat
	PROPERTIES
	FOLDER Celbody/Satsat
)

add_dependencies(Satsat
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(Satsat
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(Satsat
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET Satsat PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/Config/Saturn/Data
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/tass17.dat ${CMAKE_BINARY_DIR}/Config/Saturn/Data/
)

if(WIN32) # Windows: upstream
set(SATSAT_LIB $<TARGET_LINKER_FILE:Satsat>)
else() # Linux port
set(SATSAT_LIB Satsat) # target name: CMake links a raw path to a prefix-less .so as -l<name>
endif() # WIN32

add_subdirectory(Mimas)
add_subdirectory(Enceladus)
add_subdirectory(Tethys)
add_subdirectory(Dione)
add_subdirectory(Rhea)
add_subdirectory(Titan)
add_subdirectory(Hyperion)
add_subdirectory(Iapetus)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS
	Satsat
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS
	Satsat
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32
install(FILES tass17.dat
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Saturn/Data
)
