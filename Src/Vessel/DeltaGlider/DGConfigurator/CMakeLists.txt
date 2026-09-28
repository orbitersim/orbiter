# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})
endif() # WIN32

if(WIN32) # Windows: upstream
add_library(DGConfigurator SHARED
	DGConfigurator.cpp
	DGConfigurator.rc
)
else() # Linux port
add_library(DGConfigurator SHARED
	DGConfigurator.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(DGConfigurator DGConfigurator.rc)
endif() # NOT WIN32

target_include_directories(DGConfigurator
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(DGConfigurator
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(DGConfigurator
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(DGConfigurator
	PROPERTIES
	FOLDER Vessels/DeltaGlider
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS DGConfigurator
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)
else() # Linux port
install(TARGETS DGConfigurator
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)
endif() # WIN32
