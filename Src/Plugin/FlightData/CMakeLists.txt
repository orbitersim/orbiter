# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(FlightData SHARED
	FlightData.cpp
)

target_include_directories(FlightData
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(FlightData
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(FlightData
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(FlightData
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS FlightData
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS FlightData
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

install(FILES FlightData.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
if(NOT WIN32) # Linux port
# not upstream: also in the build tree, so Orbiter run from there finds it
file(COPY FlightData.png DESTINATION ${ORBITER_BINARY_TEXTURES_DIR}/MenuInfoBar)
endif() # NOT WIN32
