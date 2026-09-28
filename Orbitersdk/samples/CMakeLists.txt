# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(NOT WIN32) # Linux port
# not upstream: module dialogs and drawing are Qt (the SDK's HWND/HDC are QWidget/QPainter)
set(CMAKE_AUTOMOC ON)
link_libraries(Qt6::Widgets)

endif() # NOT WIN32
add_subdirectory(AscentMFD)
add_subdirectory(DialogTemplate)
add_subdirectory(LaunchpadParamTemplate)
add_subdirectory(MFDTemplate)
add_subdirectory(Lua)

set(Exclude
	PATTERN CMakeLists.txt EXCLUDE
)

install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/AscentMFD/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/AscentMFD ${Exclude})
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/DialogTemplate/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DialogTemplate ${Exclude})
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/LaunchpadParamTemplate/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/LaunchpadParamTemplate ${Exclude})
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/MFDTemplate/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/MFDTemplate ${Exclude})
install(DIRECTORY ${ORBITER_SOURCE_SDK_DIR}/samples/Lua/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/Lua ${Exclude})

