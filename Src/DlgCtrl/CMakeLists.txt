# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
add_library(DlgCtrl STATIC
	DlgCtrl.cpp
	DlgCtrlPropertyList.cpp
	DlgCtrlSwitch.cpp
)
else() # Linux port
add_library(DlgCtrl STATIC
	DlgCtrl.cpp
	DlgCtrlPropertyList.cpp
	DlgCtrlSwitch.cpp
	${CMAKE_SOURCE_DIR}/Orbitersdk/include/DlgCtrl.h
)
endif() # WIN32

target_include_directories(DlgCtrl
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

if(NOT WIN32) # Linux port
# the controls are Qt widgets; modules (shared objects) link this archive too
target_link_libraries(DlgCtrl PUBLIC Qt6::Widgets)

endif() # NOT WIN32
if(WIN32) # Windows: upstream
set_target_properties(DlgCtrl
	PROPERTIES
	FOLDER Core
)
else() # Linux port
set_target_properties(DlgCtrl
	PROPERTIES
	FOLDER Core
	AUTOMOC ON
	POSITION_INDEPENDENT_CODE ON
)
endif() # WIN32

# Copy library to its Orbitersdk/lib location so that external project integrated into the build can find it
if(WIN32) # Windows: upstream
add_custom_command(TARGET DlgCtrl
	POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/lib
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_DLGCTRL_LIB} ${ORBITER_BINARY_SDK_DIR}/lib/ 
)
else() # Linux port
add_custom_command(TARGET DlgCtrl
	POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/lib
	COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:DlgCtrl> ${ORBITER_BINARY_SDK_DIR}/lib/ 
)
endif() # WIN32

# Installation to SDK lib dir for inclusion by addons
install(TARGETS DlgCtrl
	ARCHIVE
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib
)
