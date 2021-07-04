set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Venus/Atmosphere)

add_library(VenusAtm2006 SHARED
	VenusAtm2006.cpp
)

add_dependencies(VenusAtm2006
	Orbiterlib
	Orbitersdk
)

target_include_directories(VenusAtm2006
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(VenusAtm2006
	${CMAKE_BINARY_DIR}/Src/Orbiter/Orbiter.lib
	${CMAKE_BINARY_DIR}/Core/Orbitersdk/Orbitersdk.lib
)
