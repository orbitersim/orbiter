// not upstream: texture files and pixel conversions (the D3DX texture file functions and LockRect readbacks)

#ifndef __VKTEXFILE_H
#define __VKTEXFILE_H

#include "VkCore.h"
#include <vector>

#define VKTEX_FROM_FILE 0xFFFFFFFE   // D3DX_FROM_FILE: mip count (or size) as stored in the file

// image file formats, numbered as D3DXIMAGE_FILEFORMAT
enum VkImageFileFormat { VKIFF_BMP = 0, VKIFF_JPG = 1, VKIFF_TGA = 2, VKIFF_PNG = 3, VKIFF_DDS = 4 };

// how the texture view maps the stored channels (D3D formats Vulkan has no direct counterpart for)
enum VkSwz { SWZ_NONE = 0, SWZ_NOALPHA, SWZ_LUM, SWZ_ALPHA, SWZ_LUMALPHA }; // X8R8G8B8, L8/L16, A8, A8L8

struct VkImageInfo {                 // D3DXIMAGE_INFO
	UINT Width, Height, Depth, MipLevels;
	VkFormat Format;
	VkSwz Swizzle;
	bool Cube;
	VkImageFileFormat ImageFileFormat;
};

// image in memory: levels of each layer (cube faces), tightly packed rows (4x4 blocks for BCn)
struct VkPixels {
	UINT w = 0, h = 0, depth = 1, levels = 0, layers = 0;
	VkFormat fmt = VK_FORMAT_UNDEFINED;
	VkSwz swz = SWZ_NONE;
	std::vector<std::vector<BYTE>> data; // [layer * levels + level]
	std::vector<BYTE> &Level (UINT level, UINT layer = 0) { return data[layer * levels + level]; }
	const std::vector<BYTE> &Level (UINT level, UINT layer = 0) const { return data[layer * levels + level]; }
};

VkComponentMapping VkSwizzleMap (VkSwz swz);
VkSwz VkSwizzleOf (const VkComponentMapping &m);   // a texture view's mapping back to VkSwz
VkDeviceSize VkLevelSize (VkFormat fmt, UINT w, UINT h, UINT d = 1);

bool VkGetImageInfoFromFile (const char *path, VkImageInfo *info);
bool VkLoadPixels (const char *path, VkPixels &px);         // DDS as stored; other files as B8G8R8A8
bool VkLoadPixelsFromMemory (const BYTE *buf, size_t n, VkPixels &px);

// format, size and mip chain changes on the CPU (BCn decoded/encoded); w/h 0 keep the size, levels 0 = full chain
bool VkConvertPixels (const VkPixels &in, VkPixels &out, VkFormat fmt, VkSwz swz, UINT w, UINT h, UINT levels);

// texture with the pixels uploaded; D3DX's call: file, size (0 = file), mips (VKTEX_FROM_FILE, 0 = full chain), format
VkTex *VkCreateTexture (VkDev *dev, const VkPixels &px, VkImageUsageFlags usage);
VkTex *VkCreateTextureFromFile (VkDev *dev, const char *path, UINT w, UINT h, UINT mips, VkFormat fmt, VkSwz swz,
	VkImageUsageFlags usage, VkImageInfo *info = NULL);
bool VkLoadTextureLevel (VkTex *t, UINT level, UINT layer, const VkPixels &src); // D3DXLoadSurfaceFrom*: converts to fit

// GPU → CPU (GetRenderTargetData + LockRect); multisampled images are resolved
bool VkReadPixels (VkDev *dev, VkTex *t, VkPixels &px, UINT levels = 1);

bool VkSavePixels (const char *path, VkImageFileFormat fmt, const VkPixels &px); // DDS keeps the format

#endif // !__VKTEXFILE_H
