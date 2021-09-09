# Copyright (c) Martin Schweiger
# Licensed under the MIT License

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
