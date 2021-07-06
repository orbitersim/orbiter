set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Earth/Atmosphere)

add_library(EarthAtmNRLMSISE00 SHARED
	EarthAtmNRLMSISE00.cpp
	nrlmsise-00.c
	nrlmsise-00_data.c
)

add_dependencies(EarthAtmNRLMSISE00
	Orbiter
	Orbitersdk
)

target_include_directories(EarthAtmNRLMSISE00
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(EarthAtmNRLMSISE00
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)
