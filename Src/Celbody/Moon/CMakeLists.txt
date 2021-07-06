set(CELBODY "Moon")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)

add_library(${CELBODY} SHARED
	${CELBODY}.cpp
	ELP82.cpp
)

add_dependencies(${CELBODY}
	Orbiter
	Orbitersdk
)

target_include_directories(${CELBODY}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(${CELBODY}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET ${CELBODY} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)
