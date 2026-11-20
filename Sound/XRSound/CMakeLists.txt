## XRSound
set(ASSET_DIR ${CMAKE_CURRENT_SOURCE_DIR}/assets)
set(SOLUTION_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)

# Deploy assets
file(GLOB config_files CONFIGURE_DEPENDS ${ASSET_DIR}/XRSound/*.cfg)
set(assets_src ${config_files} ${ASSET_DIR}/XRSound/ReadMe.txt)
set(assets_tgt ${assets_src})
list(TRANSFORM assets_tgt REPLACE "^${ASSET_DIR}/XRSound/" "${ORBITER_BINARY_ROOT_DIR}/XRSound/")

add_custom_command(
	OUTPUT ${assets_tgt}
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/XRSound
	COMMAND ${CMAKE_COMMAND} -E copy ${assets_src} ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	DEPENDS ${assets_src}
)
add_custom_command(
	OUTPUT "${ORBITER_BINARY_ROOT_DIR}/Doc/XRSound User Manual.pdf"
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/Doc
	COMMAND ${CMAKE_COMMAND} -E copy "${ASSET_DIR}/Doc/XRSound User Manual.pdf" ${ORBITER_BINARY_ROOT_DIR}/Doc/
	DEPENDS "${ASSET_DIR}/Doc/XRSound User Manual.pdf"
)
add_custom_command(
	OUTPUT ${ORBITER_BINARY_SDK_DIR}/XRSound/XRSound.h
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/XRSound
	COMMAND ${CMAKE_COMMAND} -E copy ${SOLUTION_DIR}/XRSound.h ${ORBITER_BINARY_SDK_DIR}/XRSound/
	DEPENDS ${SOLUTION_DIR}/XRSound.h
)
add_directory_copy(${ASSET_DIR}/XRSound/Default ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default assets_tgt)

add_custom_target(XRSound_assets ALL
	DEPENDS
		${assets_tgt}
		"${ORBITER_BINARY_ROOT_DIR}/Doc/XRSound User Manual.pdf"
		${ORBITER_BINARY_SDK_DIR}/XRSound/XRSound.h
)

install(DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/XRSound
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_SDK_DIR}/XRSound
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/XRSound
)
install(FILES "${ORBITER_BINARY_ROOT_DIR}/Doc/XRSound User Manual.pdf"
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}/
)

add_subdirectory(src)
