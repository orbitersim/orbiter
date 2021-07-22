# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(ScnEditor SHARED
	ScnEditor.cpp
	Editor.cpp
	Convert.cpp
	ScnEditor.rc
)

target_include_directories(ScnEditor
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(ScnEditor
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	comctl32.lib
)

add_dependencies(ScnEditor
	Orbiter
	Orbitersdk
	DlgCtrl
)

set_target_properties(ScnEditor
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS ScnEditor
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

add_subdirectory(Help)
