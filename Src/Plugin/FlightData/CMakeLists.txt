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
install(TARGETS FlightData
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

install(FILES FlightData.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
