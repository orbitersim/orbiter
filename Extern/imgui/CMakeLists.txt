project(imgui)

Include(FetchContent)

FetchContent_Declare(
    freetype
    GIT_REPOSITORY https://gitlab.freedesktop.org/freetype/freetype.git
    GIT_TAG VER-2-14-1   # pick a stable release
	EXCLUDE_FROM_ALL
)

FetchContent_MakeAvailable(freetype)
install(TARGETS freetype RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

FetchContent_Declare(
  imgui
  GIT_REPOSITORY https://github.com/ocornut/imgui.git
  GIT_TAG v1.92.5-docking
  PATCH_COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/imconfig.h .
  UPDATE_DISCONNECTED 1
)
FetchContent_MakeAvailable(imgui)

install(FILES ${imgui_SOURCE_DIR}/misc/fonts/Roboto-Medium.ttf fa-solid-900.ttf ${imgui_SOURCE_DIR}/misc/fonts/Cousine-Regular.ttf Lekton-Bold.ttf architext.regular.ttf NotoSansJP-Regular.ttf
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
