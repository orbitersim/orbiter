set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Earth/Atmosphere)

add_library(EarthAtmNRLMSISE00 SHARED
	EarthAtmNRLMSISE00.cpp
	nrlmsise-00.c
	nrlmsise-00_data.c
)

add_dependencies(EarthAtmNRLMSISE00
	Orbiterlib
	Orbitersdk
)

target_include_directories(EarthAtmNRLMSISE00
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(EarthAtmNRLMSISE00
	${CMAKE_BINARY_DIR}/Src/Orbiter/Orbiter.lib
	${CMAKE_BINARY_DIR}/Core/Orbitersdk/Orbitersdk.lib
)
