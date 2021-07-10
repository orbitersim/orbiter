set(CELBODY "Proteus")

add_custom_target(${CELBODY} ALL
	COMMAND echo .
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)

set_target_properties(${CELBODY}
	PROPERTIES
	FOLDER Celbody
)
