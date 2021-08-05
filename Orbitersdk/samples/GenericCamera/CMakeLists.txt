# Licensed under the MIT License

add_library(GenericCamera MODULE
	MFD.cpp
	Shell.cpp
	MFD.h
	Shell.h
	GenericCamera.rc
)

target_link_libraries(GenericCamera
	orbiter.lib
	orbitersdk.lib
)

target_include_directories(GenericCamera
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_directories(GenericCamera PUBLIC
	${ORBITER_SOURCE_SDK_LIB_DIR}
)

#add_dependencies(GenericCamera
#	Orbiter
#	Orbitersdk
#)

set_target_properties(GenericCamera
	PROPERTIES
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
)
