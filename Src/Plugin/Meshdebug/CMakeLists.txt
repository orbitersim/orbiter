add_library(Meshdebug SHARED
	Meshdebug.cpp
	Meshdebug.rc
)

target_include_directories(Meshdebug
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(Meshdebug
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(Meshdebug
	Orbiter
	Orbitersdk
)

set_target_properties(Meshdebug
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS Meshdebug
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
