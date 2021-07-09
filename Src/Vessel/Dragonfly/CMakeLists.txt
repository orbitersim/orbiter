set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

find_package(OpenGL)

if(OPENGL_FOUND) # OpenGL is required for this target

add_library(Dragonfly SHARED
	Dragonfly.cpp
	internal.cpp
	instruments.cpp
	matrix.cpp
	quaternion.cpp
	vectors.cpp
	hsystems.cpp
	thermal.cpp
	esystems.cpp
)

target_include_directories(Dragonfly
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(Dragonfly
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	OpenGL::GL
	OpenGL::GLU
)

add_dependencies(Dragonfly
	Orbiter
	Orbitersdk
)

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes
)

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/Dragonfly
)

endif()