# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
if(MFC_FOUND)
    add_executable(Shipedit
            Shipedit.cpp
            Shipedit.rc
            Mesh.cpp
            ShipeditDlg.cpp
            TransformDlg.cpp
            Vecmat.cpp
            StdAfx.cpp
    )

    target_include_directories(Shipedit
            PUBLIC ${ORBITER_SOURCE_DIR}
    )

    set_target_properties(Shipedit
            PROPERTIES
            FOLDER Tools
    )

    add_definitions(-D_AFXDLL)

    set_target_properties(Shipedit
            PROPERTIES
            LINK_FLAGS "/SUBSYSTEM:WINDOWS"
            FOLDER Utils
    )

    install(TARGETS
            Shipedit
            RUNTIME
            DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils
    )
else()
    message(WARNING "MFC not found: not building Shipedit utility")
endif()
else() # Linux port
# if(MFC_FOUND) left out: MFC is replaced by Qt 6 Widgets + ResDialog, which builds the dialogs from Shipedit.rc
    add_executable(Shipedit
            Shipedit.cpp
            Mesh.cpp
            ShipeditDlg.cpp
            TransformDlg.cpp
            Vecmat.cpp
            StdAfx.cpp
            ${ORBITER_SOURCE_DIR}/ResDialog.cpp # not upstream: .rc dialog templates -> Qt widgets
            ${ORBITER_SOURCE_DIR}/ResDialog.h
    )
    orbiter_rc_resources(Shipedit Shipedit.rc EXE SYMBOL OrbiterResources) # ResDialog finds an exe's table as OrbiterResources

    target_include_directories(Shipedit
            PUBLIC ${ORBITER_SOURCE_DIR}
            PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR} # not upstream: D3D7 types became the SDK's NTVERTEX/MATERIAL/FVECTOR3; OrbiterResource.h
    )

    target_link_libraries(Shipedit
            Qt6::Widgets
    )

    set_target_properties(Shipedit
            PROPERTIES
            FOLDER Tools
            AUTOMOC ON
    )

    # add_definitions(-D_AFXDLL) left out: no MFC DLL

    set_target_properties(Shipedit
            PROPERTIES
            # LINK_FLAGS "/SUBSYSTEM:WINDOWS" left out: ELF executables have no subsystem
            FOLDER Utils
    )

    install(TARGETS
            Shipedit
            RUNTIME
            DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils
    )
# else() warning "MFC not found" left out: Qt 6 is required by the top-level CMakeLists
endif() # WIN32
