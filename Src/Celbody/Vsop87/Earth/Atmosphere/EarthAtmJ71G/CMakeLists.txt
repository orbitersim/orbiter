set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Earth/Atmosphere)

add_library(EarthAtmJ71G SHARED
	EarthAtmJ71G.cpp
)

add_dependencies(EarthAtmJ71G
	Orbiter
	Orbitersdk
)

target_include_directories(EarthAtmJ71G
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(EarthAtmJ71G
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)
