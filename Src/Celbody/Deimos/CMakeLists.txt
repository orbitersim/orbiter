set(CELBODY "Deimos")

add_custom_target(${CELBODY} ALL
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${CELBODY}.dll ${CMAKE_BINARY_DIR}/Modules/Celbody/
	BYPRODUCTS ${CMAKE_BINARY_DIR}/Modules/Celbody/${CELBODY}.dll
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)
