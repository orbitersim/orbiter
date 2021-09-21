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
	orbiter.lib
	orbitersdk.lib
)

target_include_directories(DrawOrbits
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_directories(DrawOrbits PUBLIC
	${ORBITER_SOURCE_SDK_LIB_DIR}
)

set_target_properties(DrawOrbits
	PROPERTIES
	COMPILE_FLAGS ${Flags}
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER OVP
)

install(TARGETS DrawOrbits
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
