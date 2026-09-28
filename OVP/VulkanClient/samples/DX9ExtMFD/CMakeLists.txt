# Licensed under the MIT License

add_library(DX9ExtMFD MODULE
	ExtMFD.cpp
	MFDWindow.cpp
	MFDWindow.h
)
orbiter_rc_resources(DX9ExtMFD ExtMFD.rc)

add_dependencies(DX9ExtMFD
	D3D9Client
)

# ${OrbiterTgt} left out: the orbiter symbols resolve against the executable at dlopen (no import library on ELF)
target_link_libraries(DX9ExtMFD
	Orbitersdk
	Qt6::Widgets
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
