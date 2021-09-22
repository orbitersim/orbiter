# Licensed under the MIT License

add_library(TerrainToolBox MODULE
	gcPropertyTree.h
	gcTableView.cpp
	QTree.h
	QTree.cpp	
	ToolBox.h
	ToolBox.cpp
	ToolBox.rc
	ExportImport.cpp
	Basics.cpp
	Layer.cpp
	resource.h
)

add_dependencies(TerrainToolBox
	D3D9Client
)

target_link_libraries(TerrainToolBox
	orbiter.lib
	orbitersdk.lib
	Msimg32.lib
	Comctl32.lib
)

target_include_directories(TerrainToolBox
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_directories(TerrainToolBox
	PUBLIC ${ORBITER_SOURCE_SDK_LIB_DIR}
)

set_target_properties(TerrainToolBox
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER OVP
)

install(TARGETS TerrainToolBox
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
