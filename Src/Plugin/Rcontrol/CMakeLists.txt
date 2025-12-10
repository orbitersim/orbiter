# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(Rcontrol SHARED
	Rcontrol.cpp
)

target_include_directories(Rcontrol
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(Rcontrol
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	imgui
)

add_dependencies(Rcontrol
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(Rcontrol
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS Rcontrol
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

install(FILES Rcontrol.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
