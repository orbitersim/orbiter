# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(Orbiter_ng
	Orbiter_ng.cpp
	Orbiter_ng.rc
)

set_target_properties(Orbiter_ng
	PROPERTIES
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	FOLDER Core
)

#Installation
install(TARGETS
	Orbiter_ng
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
	)
