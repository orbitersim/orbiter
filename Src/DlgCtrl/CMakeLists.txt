add_library(DlgCtrl STATIC
	DlgCtrl.cpp
	DlgCtrlPropertyList.cpp
	DlgCtrlSwitch.cpp
)

target_include_directories(DlgCtrl
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

set_target_properties(DlgCtrl
	PROPERTIES
	FOLDER Core
)
