# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})
endif() # WIN32

if(WIN32) # Windows: upstream
add_library(AtmConfig SHARED
	AtmConfig.cpp
	AtmConfig.rc
)
else() # Linux port
add_library(AtmConfig SHARED
	AtmConfig.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(AtmConfig AtmConfig.rc)
endif() # NOT WIN32

target_include_directories(AtmConfig
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(AtmConfig
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(AtmConfig
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(AtmConfig
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS AtmConfig
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)
else() # Linux port
install(TARGETS AtmConfig
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)
endif() # WIN32
