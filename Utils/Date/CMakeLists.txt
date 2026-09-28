# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
if(MFC_FOUND)
    add_executable(Date
            Date.cpp
            Date.rc
            DateDlg.cpp
            Convert.cpp
            StdAfx.cpp
    )

    set_target_properties(Date
            PROPERTIES
            FOLDER Tools
    )

    add_definitions(-D_AFXDLL)

    set_target_properties(Date
            PROPERTIES
            LINK_FLAGS "/SUBSYSTEM:WINDOWS"
            FOLDER Utils
    )

    install(TARGETS
            Date
            RUNTIME
            DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
    )
else()
    message(WARNING "MFC not found: not building Date utility")
endif()
else() # Linux port
# if(MFC_FOUND) left out: MFC is replaced by Qt 6 Widgets + ResDialog, which builds the dialogs from Date.rc
    add_executable(Date
            Date.cpp
            DateDlg.cpp
            Convert.cpp
            StdAfx.cpp
            ${ORBITER_SOURCE_DIR}/ResDialog.cpp # not upstream: .rc dialog templates -> Qt widgets
            ${ORBITER_SOURCE_DIR}/ResDialog.h
    )
    orbiter_rc_resources(Date Date.rc EXE SYMBOL OrbiterResources) # ResDialog finds an exe's table as OrbiterResources

    target_include_directories(Date # not upstream: ResDialog.h, OrbiterResource.h
            PRIVATE ${ORBITER_SOURCE_DIR}
            PRIVATE ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
    )

    target_link_libraries(Date
            Qt6::Widgets
    )

    set_target_properties(Date
            PROPERTIES
            FOLDER Tools
            AUTOMOC ON
    )

    # add_definitions(-D_AFXDLL) left out: no MFC DLL

    set_target_properties(Date
            PROPERTIES
            # LINK_FLAGS "/SUBSYSTEM:WINDOWS" left out: ELF executables have no subsystem
            FOLDER Utils
    )

    install(TARGETS
            Date
            RUNTIME
            DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
    )
# else() warning "MFC not found" left out: Qt 6 is required by the top-level CMakeLists
endif() # WIN32
