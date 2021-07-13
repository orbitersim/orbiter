# The target directory for plugin DLLs
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})

add_library(AscentMFD SHARED
	AscentMFD.cpp
	AscentMFD.rc
)

target_include_directories(AscentMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(AscentMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(AscentMFD
	Orbiter
	Orbitersdk
)

set_target_properties(AscentMFD
	PROPERTIES
	FOLDER SDKSamples
)
