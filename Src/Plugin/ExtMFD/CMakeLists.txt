# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(ExtMFD SHARED
	ExtMFD.cpp
	MFDWindow.cpp
)

target_include_directories(ExtMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(ExtMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(ExtMFD
	${OrbiterTgt}
	Orbitersdk
	imgui
)

set_target_properties(ExtMFD
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS ExtMFD
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS ExtMFD
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

install(FILES ExtMFD.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
if(NOT WIN32) # Linux port
# not upstream: also in the build tree, so Orbiter run from there finds it
file(COPY ExtMFD.png DESTINATION ${ORBITER_BINARY_TEXTURES_DIR}/MenuInfoBar)
endif() # NOT WIN32
