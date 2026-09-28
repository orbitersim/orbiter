# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
if(ORBITER_BUILD_D3D9CLIENT)
	add_subdirectory(D3D9Client)
endif()
else() # Linux port
# D3D9Client → VulkanClient
if(ORBITER_BUILD_VULKANCLIENT)
	add_subdirectory(VulkanClient)
endif()
endif() # WIN32
