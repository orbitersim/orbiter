# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# The target directory for plugin DLLs
if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})
endif() # WIN32

if(WIN32) # Windows: upstream
add_library(DialogTemplate SHARED
	DialogTemplate.cpp
	DialogTemplate.rc
)
else() # Linux port
add_library(DialogTemplate SHARED
	DialogTemplate.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(DialogTemplate DialogTemplate.rc)
endif() # NOT WIN32

target_include_directories(DialogTemplate
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(DialogTemplate
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(DialogTemplate
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(DialogTemplate
	PROPERTIES
	FOLDER SDKSamples
)
