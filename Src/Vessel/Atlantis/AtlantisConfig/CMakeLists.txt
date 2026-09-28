# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})
endif() # WIN32

if(WIN32) # Windows: upstream
add_library(AtlantisConfig SHARED
	AtlantisConfig.cpp
	AConfigurator.rc
)
else() # Linux port
add_library(AtlantisConfig SHARED
	AtlantisConfig.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(AtlantisConfig AConfigurator.rc)
endif() # NOT WIN32

target_include_directories(AtlantisConfig
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(AtlantisConfig
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(AtlantisConfig
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(AtlantisConfig
	PROPERTIES
	FOLDER Vessels/Atlantis
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS AtlantisConfig
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)
else() # Linux port
install(TARGETS AtlantisConfig
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)
endif() # WIN32
