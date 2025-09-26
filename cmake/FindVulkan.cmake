find_path(VULKAN_DIR
	Include/vulkan/vulkan.hpp
	HINTS "C:/VulkanSDK/1.2.189.2"
	PATHS ENV VK_SDK_PATH
)

if(${VULKAN_DIR} STREQUAL "VULKAN_DIR-NOTFOUND")
	set(VULKAN_FOUND FALSE)
else()
	set(VULKAN_FOUND TRUE)
endif()
