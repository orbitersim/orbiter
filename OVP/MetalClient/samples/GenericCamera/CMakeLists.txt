# Licensed under the MIT License

add_library(GenericCamera SHARED
	MFD.cpp
	Shell.cpp
	MFD.h
	Shell.h
)

target_include_directories(GenericCamera
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${INCLUDE_TARGET_DIR}
)

target_link_libraries(GenericCamera
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(GenericCamera
	${OrbiterTgt}
	Orbitersdk
	imgui
	MetalClient
	MetalClient_Interface
)

set_target_properties(GenericCamera
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	FOLDER OVP
)

install(TARGETS GenericCamera
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
