# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# The target directory for plugin DLLs
if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})
endif() # WIN32

if(WIN32) # Windows: upstream
add_library(MFDTemplate SHARED
	MFDTemplate.cpp
	MFDTemplate.rc
)
else() # Linux port
add_library(MFDTemplate SHARED
	MFDTemplate.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(MFDTemplate MFDTemplate.rc)
endif() # NOT WIN32

target_include_directories(MFDTemplate
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(MFDTemplate
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(MFDTemplate
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(MFDTemplate
	PROPERTIES
	FOLDER SDKSamples
)
