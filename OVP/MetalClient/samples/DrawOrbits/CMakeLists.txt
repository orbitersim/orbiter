# Licensed under the MIT License

add_library(DrawOrbits SHARED
	Draw.cpp
	Orbit.cpp
	Orbit.h
	Reference.cpp
	Reference.h
	Tools.cpp
	Tools.h
)

target_include_directories(DrawOrbits
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${INCLUDE_TARGET_DIR}
)

target_link_libraries(DrawOrbits
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(DrawOrbits
	${OrbiterTgt}
	Orbitersdk
	imgui
	MetalClient
	MetalClient_Interface
)

set_target_properties(DrawOrbits
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	FOLDER OVP
)

install(TARGETS DrawOrbits
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
