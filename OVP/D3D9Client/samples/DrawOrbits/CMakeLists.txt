# Licensed under the MIT License

add_library(DrawOrbits MODULE
	Draw.cpp
	Orbit.cpp
	Orbit.h
	Reference.cpp
	Reference.h
	Tools.cpp
	Tools.h
	Orbits.rc
)

add_dependencies(DrawOrbits
	D3D9Client
)

target_link_libraries(DrawOrbits
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(DrawOrbits
	PUBLIC ${INCLUDE_TARGET_DIR}
)

set_target_properties(DrawOrbits
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER OVP
)

install(TARGETS DrawOrbits
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
