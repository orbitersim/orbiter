# Copyright (c) Gondos
# Licensed under the MIT License

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/SolarSail.lua.cfg
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/SolarSail.lua
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels/SolarSail
)

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/SolarSail.scn
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Scenarios/Lua
)

