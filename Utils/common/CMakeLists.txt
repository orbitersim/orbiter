# Copyright (c) Martin Schweiger
# Licensed under the MIT License

FetchContent_MakeAvailable(imgui)

add_library(UtilsDDS STATIC
	DDSWrite.cpp
)
target_include_directories(UtilsDDS PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(UtilsDDS PROPERTIES FOLDER Tools)

add_library(UtilsImGui STATIC
	UtilApp.mm
	${ORBITER_SOURCE_DIR}/imgui_impl_orbiter.cpp
	${imgui_SOURCE_DIR}/imgui.cpp
	${imgui_SOURCE_DIR}/imgui_draw.cpp
	${imgui_SOURCE_DIR}/imgui_tables.cpp
	${imgui_SOURCE_DIR}/imgui_widgets.cpp
	${imgui_SOURCE_DIR}/backends/imgui_impl_metal.mm
)
target_include_directories(UtilsImGui
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${imgui_SOURCE_DIR}
	PRIVATE ${imgui_SOURCE_DIR}/backends ${ORBITER_SOURCE_DIR}
)
target_link_libraries(UtilsImGui PUBLIC OrbiterPlatform)
target_compile_options(UtilsImGui PRIVATE "$<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-arc>")
set_target_properties(UtilsImGui PROPERTIES FOLDER Tools)
