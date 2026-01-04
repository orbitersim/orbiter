set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)

add_subdirectory(Config)
add_subdirectory(cspice)
add_subdirectory(src)

add_library(SPICE SHARED
	src/spice.cpp
)

set_target_properties(SPICE
	PROPERTIES
	FOLDER Celbody/Vsop87
)

add_dependencies(SPICE
	${OrbiterTgt}
)

target_include_directories(SPICE
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(SPICE
	${ORBITER_LIB}
)

set(SPICE_LIB $<TARGET_LINKER_FILE:SPICE>)

# Installation
install(TARGETS
	SPICE
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
