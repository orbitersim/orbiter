set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)

add_library(Galsat SHARED
	Galsat.cpp
	Lieske.cpp
)

add_dependencies(Galsat
	Orbiter
	Orbitersdk
)

target_include_directories(Galsat
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(Galsat
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET Galsat PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/Config/Jupiter/Data
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/ephem_e15.dat ${CMAKE_BINARY_DIR}/Config/Jupiter/Data/
)

set(GALSAT_LIB $<TARGET_LINKER_FILE:Galsat>)

add_subdirectory(Io)
add_subdirectory(Europa)
add_subdirectory(Ganymede)
add_subdirectory(Callisto)
