add_executable(pltex
	Pltex.cpp
)

set_target_properties(pltex
	PROPERTIES
	FOLDER Tools
)

# Installation
install(TARGETS
	pltex
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)