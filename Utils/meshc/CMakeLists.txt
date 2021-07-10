add_executable(meshc
	meshc.cpp
	Mesh.cpp
)

target_include_directories(meshc
	PUBLIC ${ORBITER_SOURCE_DIR}
)

set_target_properties(meshc
	PROPERTIES
	FOLDER Tools
)
