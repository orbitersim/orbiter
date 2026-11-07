add_library(OrbiterPlatform SHARED
	compat_win.cpp
	pathfix.c
	moduleinfo.cpp
	compat_thread.cpp
	mac/platform_mac.mm
	mac/joystick_mac.mm
	mac/image_mac.mm
	mac/help_mac.mm
	mac/console_mac.mm
)

target_include_directories(OrbiterPlatform
	PUBLIC ${ORBITER_COMPAT_INCLUDE_DIR}
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include
)

target_link_libraries(OrbiterPlatform
	PUBLIC "-framework AppKit"
	PUBLIC "-framework QuartzCore"
	PUBLIC "-framework Metal"
	PUBLIC "-framework IOKit"
	PUBLIC "-framework GameController"
	PUBLIC "-framework UniformTypeIdentifiers"
	PUBLIC "-framework ImageIO"
	PUBLIC "-framework CoreGraphics"
	PUBLIC "-framework Carbon"
	PUBLIC "-framework WebKit"
)

target_compile_options(OrbiterPlatform PRIVATE "$<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-arc>")

set_target_properties(OrbiterPlatform PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	INSTALL_NAME_DIR "@rpath"
	FOLDER Core
)
