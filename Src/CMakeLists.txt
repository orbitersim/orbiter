# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Orbiter)
add_subdirectory(Orbitersdk)
add_subdirectory(DlgCtrl)
add_subdirectory(Celbody)
add_subdirectory(Vessel)
add_subdirectory(Module)
add_subdirectory(Plugin)

# ==========================================================================
# SDK Sample Installation
# ==========================================================================

set(IgnoreFiles
	PATTERN CMakeLists.txt EXCLUDE
)

install(DIRECTORY Module/LuaScript/LuaInline/ DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples/LuaInline ${IgnoreFiles})
install(DIRECTORY Module/LuaScript/LuaInterpreter/ DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples/LuaInterpreter/ ${IgnoreFiles})
