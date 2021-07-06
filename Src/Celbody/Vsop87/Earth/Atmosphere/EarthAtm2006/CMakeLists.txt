set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Earth/Atmosphere)

add_library(EarthAtm2006 SHARED
	EarthAtm2006.cpp
)

add_dependencies(EarthAtm2006
	Orbiter
	Orbitersdk
)

target_include_directories(EarthAtm2006
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(EarthAtm2006
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)
