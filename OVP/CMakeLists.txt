# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(OVP_GC_SOURCE_DIR ${CMAKE_SOURCE_DIR}/OVP/D3D9Client)
set(OVP_INCLUDE_TARGET_DIR ${ORBITER_BINARY_SDK_DIR}/include)
set(OVP_InterfaceBuilder ${OVP_GC_SOURCE_DIR}/samples/InterfaceBuilder/InterfaceBuilder.exe)

add_custom_command(OUTPUT ${OVP_INCLUDE_TARGET_DIR}/gcCoreAPI.h ${OVP_INCLUDE_TARGET_DIR}/gcGUI.h
	DEPENDS copy_sdk_include ${OVP_GC_SOURCE_DIR}/gcGUI.h ${OVP_GC_SOURCE_DIR}/gcCore.h ${OVP_GC_SOURCE_DIR}/gcCore.cpp
	COMMAND ${CMAKE_COMMAND} -E make_directory ${OVP_INCLUDE_TARGET_DIR}
	COMMAND ${OVP_InterfaceBuilder} ${OVP_GC_SOURCE_DIR}/gcCore.h ${OVP_INCLUDE_TARGET_DIR}/gcCoreAPI.h ${OVP_GC_SOURCE_DIR}/gcCore.cpp
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${OVP_GC_SOURCE_DIR}/gcGUI.h ${OVP_INCLUDE_TARGET_DIR}/gcGUI.h
	VERBATIM
)
add_custom_target(OVP_Interface
	DEPENDS ${OVP_INCLUDE_TARGET_DIR}/gcCoreAPI.h ${OVP_INCLUDE_TARGET_DIR}/gcGUI.h
)
add_custom_target(D3D9Client_Interface DEPENDS OVP_Interface)
set_target_properties(OVP_Interface D3D9Client_Interface PROPERTIES FOLDER OVP)
add_dependencies(Orbitersdk OVP_Interface)

install(FILES ${OVP_INCLUDE_TARGET_DIR}/gcCoreAPI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)
install(FILES ${OVP_INCLUDE_TARGET_DIR}/gcGUI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)

if(ORBITER_BUILD_D3D9CLIENT)
	add_subdirectory(D3D9Client)
endif()
