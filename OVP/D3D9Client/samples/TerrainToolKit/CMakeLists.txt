# Licensed under the MIT License

add_library(TerrainToolKit MODULE
	gcPropertyTree.h
	gcTableView.cpp
	QTree.h
	QTree.cpp	
	ToolKit.h
	ToolKit.cpp
	ToolKit.rc
	ExportImport.cpp
	Basics.cpp
	Layer.cpp
	resource.h
)

add_dependencies(TerrainToolKit
	D3D9Client
)

target_link_libraries(TerrainToolKit
	${OrbiterTgt}
	Orbitersdk
	Msimg32.lib
	Comctl32.lib
)

target_include_directories(TerrainToolKit
	PUBLIC ${INCLUDE_TARGET_DIR}
)

set_target_properties(TerrainToolKit
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	FOLDER OVP
)

install(TARGETS TerrainToolKit
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
) 
