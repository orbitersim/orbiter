# Licensed under the MIT License

add_library(DX9ExtMFD MODULE
	ExtMFD.cpp
	MFDWindow.cpp
	MFDWindow.h
	ExtMFD.rc
)

add_dependencies(DX9ExtMFD
	D3D9Client
)

target_link_libraries(DX9ExtMFD
	${OrbiterTgt}
	Orbitersdk
)

target_include_directories(DX9ExtMFD
	PUBLIC ${INCLUDE_TARGET_DIR}
)

set_target_properties(DX9ExtMFD
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER OVP
)

install(TARGETS DX9ExtMFD
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
