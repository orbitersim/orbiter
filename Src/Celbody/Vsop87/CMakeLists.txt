set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)

add_library(Vsop87 SHARED
	Vsop87.cpp
)

add_dependencies(Vsop87
	Orbiter
)

target_include_directories(Vsop87
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(Vsop87
	${CMAKE_BINARY_DIR}/Src/Orbiter/Orbiter.lib
)

add_subdirectory(Sol)
add_subdirectory(Sun)
add_subdirectory(Mercury)
add_subdirectory(Venus)
