# Licensed under the MIT License

add_library(DX9ExtMFD MODULE
	ExtMFD.cpp
	MFDWindow.cpp
	MFDWindow.h
	ExtMFD.rc
)

target_link_libraries(DX9ExtMFD
	orbiter.lib
	orbitersdk.lib
)

target_include_directories(DX9ExtMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_directories(DX9ExtMFD PUBLIC
	${ORBITER_SOURCE_SDK_LIB_DIR}
)


#add_dependencies(DX9ExtMFD
#	Orbiter
#	Orbitersdk
#)

set_target_properties(DX9ExtMFD
	PROPERTIES
	COMPILE_FLAGS ${Flags}
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
)
