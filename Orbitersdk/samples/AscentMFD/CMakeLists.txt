# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# The target directory for plugin DLLs
if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})
endif() # WIN32

if(WIN32) # Windows: upstream
add_library(AscentMFD MODULE
	AscentMFD.cpp
	AscentMFD.rc
)
else() # Linux port
add_library(AscentMFD MODULE
	AscentMFD.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(AscentMFD AscentMFD.rc)
endif() # NOT WIN32

add_dependencies(AscentMFD
	${OrbiterTgt}
	Orbitersdk
)

target_link_libraries(AscentMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

target_include_directories(AscentMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

set_target_properties(AscentMFD
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER Modules
)

install(TARGETS AscentMFD
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
