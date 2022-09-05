# Licensed under the MIT License

add_library(GenericCamera MODULE
	MFD.cpp
	Shell.cpp
	MFD.h
	Shell.h
	GenericCamera.rc
)

add_dependencies(GenericCamera
	D3D9Client
)

target_link_libraries(GenericCamera
	${OrbiterTgt}
	Orbitersdk
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
