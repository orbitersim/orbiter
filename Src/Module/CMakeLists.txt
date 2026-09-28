# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(NOT WIN32) # Linux port
# not upstream: module dialogs and drawing are Qt (the SDK's HWND/HDC are QWidget/QPainter)
set(CMAKE_AUTOMOC ON)
link_libraries(Qt6::Widgets)

endif() # NOT WIN32
add_subdirectory(LuaScript)
