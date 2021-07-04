set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Mars/Atmosphere)

add_library(MarsAtm2006 SHARED
	MarsAtm2006.cpp
)

add_dependencies(MarsAtm2006
	Orbiterlib
	Orbitersdk
)

target_include_directories(MarsAtm2006
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(MarsAtm2006
	${CMAKE_BINARY_DIR}/Src/Orbiter/Orbiter.lib
	${CMAKE_BINARY_DIR}/Core/Orbitersdk/Orbitersdk.lib
)
