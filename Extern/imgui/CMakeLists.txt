project(imgui)

Include(FetchContent)

FetchContent_Declare(
  imgui
  GIT_REPOSITORY https://github.com/ocornut/imgui.git
  GIT_TAG v1.92.5-docking
  PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/imconfig.h .
  UPDATE_DISCONNECTED 1
)
FetchContent_MakeAvailable(imgui)

install(FILES ${imgui_SOURCE_DIR}/misc/fonts/Roboto-Medium.ttf fa-solid-900.ttf ${imgui_SOURCE_DIR}/misc/fonts/Cousine-Regular.ttf Lekton-Bold.ttf architext.regular.ttf
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Fonts
)

install(FILES ${imgui_SOURCE_DIR}/imgui.h ${imgui_SOURCE_DIR}/imconfig.h
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/include
)

add_library(imgui INTERFACE)

set(IMGUI_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
file(COPY ${imgui_SOURCE_DIR}/imgui.h ${imgui_SOURCE_DIR}/imconfig.h DESTINATION ${IMGUI_INCLUDE_DIR})
target_include_directories(imgui INTERFACE ${IMGUI_INCLUDE_DIR})

FetchContent_Declare(
  implot
  GIT_REPOSITORY https://github.com/epezent/implot.git
  GIT_TAG 3da8bd3
#  UPDATE_DISCONNECTED 1
)
FetchContent_MakeAvailable(implot)
add_library(implot INTERFACE)
install(FILES ${implot_SOURCE_DIR}/implot.h
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/include
)
set(IMPLOT_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
file(COPY ${implot_SOURCE_DIR}/implot.h DESTINATION ${IMPLOT_INCLUDE_DIR})
target_include_directories(implot INTERFACE ${IMPLOT_INCLUDE_DIR})



### Node editor

FetchContent_Declare(
  nodeedit
  GIT_REPOSITORY https://github.com/thedmd/imgui-node-editor.git
  GIT_TAG 1e0e494f1a86885eb332ca518543108ccd88d13e
#  UPDATE_DISCONNECTED 1
)
FetchContent_MakeAvailable(nodeedit)

set(NODEEDIT_INCLUDE_DIR ${nodeedit_SOURCE_DIR}/include)
set(IMGUI_NODE_EDITOR_SOURCES
	"${nodeedit_SOURCE_DIR}/imgui_node_editor.cpp"
	"${nodeedit_SOURCE_DIR}/imgui_node_editor_api.cpp"
	"${nodeedit_SOURCE_DIR}/imgui_canvas.cpp"
	"${nodeedit_SOURCE_DIR}/crude_json.cpp"
)

set(IMGUI_NODE_EDITOR_HEADERS
    "${nodeedit_SOURCE_DIR}/imgui_node_editor.h"
)

set(IMGUI_NODE_EDITOR_INC_PATH "${nodeedit_SOURCE_DIR}/")

add_library(nodeedit OBJECT
        ${IMGUI_NODE_EDITOR_SOURCES}
        ${IMGUI_NODE_EDITOR_HEADERS}
)
target_include_directories(nodeedit
    PUBLIC
	"${imgui_SOURCE_DIR}"
	"${nodeedit_SOURCE_DIR}"
)
