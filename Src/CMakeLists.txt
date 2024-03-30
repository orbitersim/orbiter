# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Orbiter)
add_subdirectory(Orbitersdk)
add_subdirectory(DlgCtrl)
add_subdirectory(Celbody)
add_subdirectory(Vessel)
add_subdirectory(Module)
add_subdirectory(Plugin)

if(BUILD_ORBITER_SERVER)
	add_subdirectory(Orbiter_ng)
endif()


# ==========================================================================
# SDK Sample Installation
# ==========================================================================

install(DIRECTORY Plugin/Common/Dialog/ DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples/Common/Dialog/ ${IgnoreFiles})
install(DIRECTORY Vessel/Common/ DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples/Common/Vessel/ ${IgnoreFiles})
install(DIRECTORY Vessel/Common/ DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples/Common/ ${IgnoreFiles})
