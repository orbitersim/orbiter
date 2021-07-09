add_executable(meshc
	meshc.cpp
	Mesh.cpp
)

target_include_directories(meshc
	PUBLIC ${ORBITER_SOURCE_DIR}
)
