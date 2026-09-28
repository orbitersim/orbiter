set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(TEXTURE2_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures2)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(THUMBNAIL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Thumbnail)

if(NOT WIN32) # Linux port
# not upstream: the root lookup is left out (Vulkan graphics), but the ADI ball is drawn offscreen with OpenGL
find_package(OpenGL)
find_package(Qt6 COMPONENTS OpenGL)

endif() # NOT WIN32
if(OPENGL_FOUND) # OpenGL is required for this target

if(WIN32) # Windows: upstream
add_library(Dragonfly SHARED
	Dragonfly.cpp
	Internal.cpp
	instruments.cpp
	Matrix.cpp
	quaternion.cpp
	vectors.cpp
	Hsystems.cpp
	Thermal.cpp
	Esystems.cpp
	panel.rc
)
else() # Linux port
add_library(Dragonfly SHARED
	Dragonfly.cpp
	Internal.cpp
	instruments.cpp
	Matrix.cpp
	quaternion.cpp
	vectors.cpp
	Hsystems.cpp
	Thermal.cpp
	Esystems.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(Dragonfly panel.rc)
endif() # NOT WIN32

set_target_properties(Dragonfly
	PROPERTIES
	FOLDER Vessels
)

target_include_directories(Dragonfly
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

if(WIN32) # Windows: upstream
target_link_libraries(Dragonfly
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	OpenGL::GL
	OpenGL::GLU
)
else() # Linux port
target_link_libraries(Dragonfly
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	OpenGL::GL
	OpenGL::GLU
	Qt6::OpenGL # not upstream: QOpenGLContext/QOffscreenSurface/QOpenGLFramebufferObject replace WGL and the DIB section
)
endif() # WIN32

add_dependencies(Dragonfly
	${OrbiterTgt}
	Orbitersdk
)

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

if(WIN32) # Windows: upstream
add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes
)
else() # Linux port
add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/Dragonfly # not upstream: was Meshes, but the module loads "Dragonfly/Dragonfly" (install uses Meshes/Dragonfly)
)
endif() # WIN32

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/Dragonfly
)

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE2_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES2_DIR}/Dragonfly
)

add_custom_command(
	TARGET Dragonfly PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${THUMBNAIL_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Images/Vessels/Default
)

if(WIN32) # Windows: upstream
install(TARGETS Dragonfly
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS Dragonfly
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes/Dragonfly
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/Dragonfly
)
install(DIRECTORY ${TEXTURE2_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES2_DIR}/Dragonfly
)
install(DIRECTORY ${THUMBNAIL_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Images/Vessels/Default
)

else()

message(WARNING "OpenGL libraries not found. Skipping Dragonfly build.")

endif()
