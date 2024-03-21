# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(AscentMFD)
add_subdirectory(DialogTemplate)
add_subdirectory(LaunchpadParamTemplate)
add_subdirectory(MFDTemplate)
add_subdirectory(Lua)

install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/AscentMFD/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/AscentMFD)
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/DialogTemplate/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DialogTemplate)
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/LaunchpadParamTemplate/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/LaunchpadParamTemplate)
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/MFDTemplate/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/MFDTemplate)
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/Lua/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/Lua)

