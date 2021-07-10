set(CELBODY "Saturn")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)

add_library(${CELBODY} SHARED
	${CELBODY}.cpp
)

add_dependencies(${CELBODY}
	Orbiter
	Orbitersdk
	Vsop87
	Satsat
)

target_include_directories(${CELBODY}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..
)

target_link_libraries(${CELBODY}
	${VSOP87_LIB}
	$<TARGET_LINKER_FILE:Satsat>
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/Config/${CELBODY}/Data/
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../Data/Vsop87B_sat.dat ${CMAKE_BINARY_DIR}/Config/${CELBODY}/Data/Vsop87B.dat
)

set_target_properties(${CELBODY}
	PROPERTIES
	FOLDER Celbody/Vsop87
)
