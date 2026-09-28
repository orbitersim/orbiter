# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(Notes SHARED
	Notes.cpp
)

target_include_directories(Notes
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(Notes
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(Notes
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(Notes
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS Notes
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS Notes
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

install(FILES Notes.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
if(NOT WIN32) # Linux port
# not upstream: also in the build tree, so Orbiter run from there finds it
file(COPY Notes.png DESTINATION ${ORBITER_BINARY_TEXTURES_DIR}/MenuInfoBar)
endif() # NOT WIN32
