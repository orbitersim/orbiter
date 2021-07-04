set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody)

add_library(Venus SHARED
	Venus.cpp
)

add_dependencies(Venus
	Vsop87
)

target_include_directories(Venus
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..
)

target_link_libraries(Venus
	${CMAKE_BINARY_DIR}/Src/Celbody/Vsop87/Vsop87.lib
	${CMAKE_BINARY_DIR}/Src/Orbiter/Orbiter.lib
	${CMAKE_BINARY_DIR}/Core/Orbitersdk/Orbitersdk.lib
)

add_custom_command(
	TARGET Venus PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config	
)

add_custom_command(
	TARGET Venus PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../Data/Vsop87B_ven.dat ${CMAKE_BINARY_DIR}/Config/Venus/Data/Vsop87B.dat
)