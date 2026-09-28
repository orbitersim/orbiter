# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
add_library(ScnEditor SHARED
	ScnEditor.cpp
	Editor.cpp
	Convert.cpp
	ScnEditor.rc
)
else() # Linux port
add_library(ScnEditor SHARED
	ScnEditor.cpp
	Editor.cpp
	Convert.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(ScnEditor ScnEditor.rc)
endif() # NOT WIN32

target_include_directories(ScnEditor
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

if(WIN32) # Windows: upstream
target_link_libraries(ScnEditor
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	comctl32.lib
)
else() # Linux port
target_link_libraries(ScnEditor
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	# comctl32 left out: the common controls are Qt widgets
)
endif() # WIN32

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
if(WIN32) # Windows: upstream
install(TARGETS ScnEditor
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS ScnEditor
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32
install(FILES ScnEditorAPI.h
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/include
)

install(FILES ScnEdit.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
if(NOT WIN32) # Linux port
# not upstream: also in the build tree, so Orbiter run from there finds it
file(COPY ScnEdit.png DESTINATION ${ORBITER_BINARY_TEXTURES_DIR}/MenuInfoBar)
endif() # NOT WIN32

if(WIN32) # Windows: upstream
if(ORBITER_MAKE_DOC)
	add_subdirectory(Help)
endif()
else() # Linux port
if(ORBITER_MAKE_DOC OR ORBITER_MAKE_HELP) # not upstream: ORBITER_MAKE_HELP
	add_subdirectory(Help)
endif()
endif() # WIN32
