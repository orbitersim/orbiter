# Licensed under the MIT License

add_library(TerrainToolKit MODULE
	gcPropertyTree.h
	gcTableView.cpp
	QTree.h
	QTree.cpp	
	ToolKit.h
	ToolKit.cpp
	ExportImport.cpp
	Basics.cpp
	Layer.cpp
	resource.h
)
orbiter_rc_resources(TerrainToolKit ToolKit.rc)

add_dependencies(TerrainToolKit
	D3D9Client
)

# ${OrbiterTgt} left out: the orbiter symbols resolve against the executable at dlopen (no import library on ELF)
target_link_libraries(TerrainToolKit
	Orbitersdk
	Qt6::Widgets
	# Msimg32.lib, Comctl32.lib left out: AlphaBlend and the common controls are Qt
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
