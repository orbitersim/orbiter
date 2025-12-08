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

set_target_properties(ScnEditor
	PROPERTIES
	FOLDER Modules
)

# Deploy the API header file
add_custom_command(
	OUTPUT ${ORBITER_BINARY_SDK_DIR}/include/ScnEditorAPI.h
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/include
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/ScnEditorAPI.h ${ORBITER_BINARY_SDK_DIR}/include
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ScnEditorAPI.h
)
add_custom_target(deploy_api
	DEPENDS ${ORBITER_BINARY_SDK_DIR}/include/ScnEditorAPI.h
)

add_dependencies(ScnEditor
	${OrbiterTgt}
	Orbitersdk
	DlgCtrl
	deploy_api
)

# Installation
install(TARGETS ScnEditor
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
install(FILES ScnEditorAPI.h
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/include
)

install(FILES ScnEdit.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)

if(ORBITER_MAKE_DOC)
	add_subdirectory(Help)
endif()