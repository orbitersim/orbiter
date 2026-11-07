# Licensed under the MIT License

add_library(DX9ExtMFD SHARED
	ExtMFD.cpp
	MFDWindow.cpp
	MFDWindow.h
)

target_include_directories(DX9ExtMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${INCLUDE_TARGET_DIR}
)

target_link_libraries(DX9ExtMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(DX9ExtMFD
	${OrbiterTgt}
	Orbitersdk
	imgui
	MetalClient
	MetalClient_Interface
)

set_target_properties(DX9ExtMFD
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	FOLDER OVP
)

install(TARGETS DX9ExtMFD
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
