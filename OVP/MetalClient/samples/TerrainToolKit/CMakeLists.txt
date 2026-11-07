# Licensed under the MIT License

add_library(TerrainToolKit SHARED
	gcPropertyTree.h
	gcTableView.cpp
	QTree.h
	QTree.cpp
	ToolKit.h
	ToolKit.cpp
	ExportImport.cpp
	Basics.cpp
	Layer.cpp
)

target_include_directories(TerrainToolKit
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${INCLUDE_TARGET_DIR}
)

target_link_libraries(TerrainToolKit
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(TerrainToolKit
	${OrbiterTgt}
	Orbitersdk
	imgui
	MetalClient
	MetalClient_Interface
)

set_target_properties(TerrainToolKit
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	FOLDER OVP
)

install(TARGETS TerrainToolKit
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
