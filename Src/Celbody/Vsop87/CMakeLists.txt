if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)
endif() # WIN32

add_library(Vsop87 SHARED
	Vsop87.cpp
)

set_target_properties(Vsop87
	PROPERTIES
	FOLDER Celbody/Vsop87
)

add_dependencies(Vsop87
	${OrbiterTgt}
)

target_include_directories(Vsop87
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(Vsop87
	${ORBITER_LIB}
)

if(WIN32) # Windows: upstream
set(VSOP87_LIB $<TARGET_LINKER_FILE:Vsop87>)
else() # Linux port
set(VSOP87_LIB Vsop87) # target name: CMake links a raw path to a prefix-less .so as -l<name>
endif() # WIN32

add_subdirectory(Sun)
add_subdirectory(Mercury)
add_subdirectory(Venus)
add_subdirectory(Earth)
add_subdirectory(Mars)
add_subdirectory(Jupiter)
add_subdirectory(Saturn)
add_subdirectory(Uranus)
add_subdirectory(Neptune)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS
	Vsop87
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS
	Vsop87
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32
