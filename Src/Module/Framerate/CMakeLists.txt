set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})

add_library(Framerate SHARED
	Framerate.cpp
	Framerate.rc
	../Common/Dialog/Graph.cpp
	../Common/Dialog/TabDlg.cpp
)

target_include_directories(Framerate
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(Framerate
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	UxTheme.lib
)

add_dependencies(Framerate
	Orbiter
	Orbitersdk
)

set_target_properties(Framerate
	PROPERTIES
	FOLDER Modules
)
