# Licensed under the MIT License

add_library(GenericCamera MODULE
	MFD.cpp
	Shell.cpp
	MFD.h
	Shell.h
)
orbiter_rc_resources(GenericCamera GenericCamera.rc)

add_dependencies(GenericCamera
	D3D9Client
)

# ${OrbiterTgt} left out: the orbiter symbols resolve against the executable at dlopen (no import library on ELF)
target_link_libraries(GenericCamera
	Orbitersdk
	Qt6::Widgets
)

target_include_directories(GenericCamera
	PUBLIC ${INCLUDE_TARGET_DIR}
)

set_target_properties(GenericCamera
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER OVP
)

install(TARGETS GenericCamera
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
