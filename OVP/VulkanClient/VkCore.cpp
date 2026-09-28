// not upstream: Vulkan objects standing in for the Direct3D 9 runtime

#define VMA_IMPLEMENTATION
#include "VkCore.h"
#include "VkShader.h"
#include "Log.h"
#include <QVulkanInstance>
#include <cstring>
#include <algorithm>

VkExtFunctions vkx;

// VkBuf

VkBuf::VkBuf (VkDev *_dev, VkDeviceSize _size, VkBufferUsageFlags usage, bool host, bool readback)
{
	dev = _dev;
	size = _size;
	mapped = NULL;
	buf = VK_NULL_HANDLE;
	alloc = VK_NULL_HANDLE;
	VkBufferCreateInfo bi = { VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO };
	bi.size = size;
	bi.usage = usage | (host ? 0 : VK_BUFFER_USAGE_TRANSFER_DST_BIT);
	VmaAllocationCreateInfo ai = {};
	ai.usage = VMA_MEMORY_USAGE_AUTO;
	if (host) ai.flags = (readback ? VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT : VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT) | VMA_ALLOCATION_CREATE_MAPPED_BIT;
	VmaAllocationInfo info;
	VKCHECK(vmaCreateBuffer (dev->vma, &bi, &ai, &buf, &alloc, &info));
	if (host) mapped = info.pMappedData;
}

VkBuf::~VkBuf ()
{
	VmaAllocator vma = dev->vma;
	VkBuffer b = buf;
	VmaAllocation a = alloc;
	dev->Defer ([vma, b, a]() { vmaDestroyBuffer (vma, b, a); });
}

void VkBuf::Upload (const void *data, VkDeviceSize n, VkDeviceSize offset)
{
	if (mapped) {
		memcpy ((char*)mapped + offset, data, n);
		return;
	}
	VkBuf staging (dev, n, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, true);
	memcpy (staging.Map(), data, n);
	VkCommandBuffer cmd = dev->BeginOneTime ();
	VkBufferCopy region = { 0, offset, n };
	vkCmdCopyBuffer (cmd, staging.buf, buf, 1, &region);
	VkMemoryBarrier2 mb = { VK_STRUCTURE_TYPE_MEMORY_BARRIER_2 };
	mb.srcStageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
	mb.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
	mb.dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	mb.dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT;
	VkDependencyInfo di = { VK_STRUCTURE_TYPE_DEPENDENCY_INFO };
	di.memoryBarrierCount = 1;
	di.pMemoryBarriers = &mb;
	vkCmdPipelineBarrier2 (cmd, &di);
	dev->EndOneTime (cmd);
}

// VkVertexDecl

UINT VkVertexDecl::Location (VkDeclUsage usage, BYTE index)
{
	switch (usage) {
	case DECL_POSITION: return 0;
	case DECL_NORMAL:   return 1;
	case DECL_TANGENT:  return 2;
	case DECL_COLOR:    return 3 + index;   // 3, 4
	case DECL_TEXCOORD: return 5 + index;   // 5..12
	}
	return 0;
}

VkVertexDecl::VkVertexDecl (const VkDeclElement *elem, int n, UINT stride0, UINT stride1)
{
	UINT stride[2] = { stride0, stride1 };
	UINT end[2] = { 0, 0 };
	for (int i = 0; i < n; i++) {
		VkVertexInputAttributeDescription2EXT a = { VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT };
		a.location = Location (elem[i].usage, elem[i].index);
		a.binding = elem[i].stream;
		a.format = elem[i].format;
		a.offset = elem[i].offset;
		attr.push_back (a);
		UINT e = elem[i].offset + VkFormatBlockSize (elem[i].format);
		if (elem[i].stream < 2 && e > end[elem[i].stream]) end[elem[i].stream] = e;
	}
	for (UINT s = 0; s < 2; s++) {
		if (!end[s]) continue;
		VkVertexInputBindingDescription2EXT b = { VK_STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT };
		b.binding = s;
		b.stride = stride[s] ? stride[s] : end[s]; // D3D9 takes the stride from SetStreamSource; declarations here carry it
		b.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		b.divisor = 1;
		bind.push_back (b);
	}
}

// Formats

UINT VkFormatBlockSize (VkFormat fmt, UINT *blockW)
{
	if (blockW) *blockW = 1;
	switch (fmt) {
	case VK_FORMAT_R8_UNORM:
	case VK_FORMAT_S8_UINT:
		return 1;
	case VK_FORMAT_R8G8_UNORM:
	case VK_FORMAT_R16_SFLOAT:
	case VK_FORMAT_R16_UNORM:
	case VK_FORMAT_R16_SINT:
	case VK_FORMAT_D16_UNORM:
	case VK_FORMAT_B5G6R5_UNORM_PACK16:
	case VK_FORMAT_R5G6B5_UNORM_PACK16:
	case VK_FORMAT_A1R5G5B5_UNORM_PACK16:
	case VK_FORMAT_B4G4R4A4_UNORM_PACK16:
	case VK_FORMAT_A4R4G4B4_UNORM_PACK16:
		return 2;
	case VK_FORMAT_R8G8B8_UNORM:
	case VK_FORMAT_B8G8R8_UNORM:
		return 3;
	case VK_FORMAT_R8G8B8A8_UNORM:
	case VK_FORMAT_B8G8R8A8_UNORM:
	case VK_FORMAT_R8G8B8A8_SRGB:
	case VK_FORMAT_B8G8R8A8_SRGB:
	case VK_FORMAT_A2R10G10B10_UNORM_PACK32:
	case VK_FORMAT_A2B10G10R10_UNORM_PACK32:
	case VK_FORMAT_R16G16_SFLOAT:
	case VK_FORMAT_R16G16_UNORM:
	case VK_FORMAT_R32_SFLOAT:
	case VK_FORMAT_R32_UINT:
	case VK_FORMAT_D32_SFLOAT:
	case VK_FORMAT_D24_UNORM_S8_UINT:
	case VK_FORMAT_X8_D24_UNORM_PACK32:
		return 4;
	case VK_FORMAT_D32_SFLOAT_S8_UINT:
	case VK_FORMAT_R16G16B16A16_SFLOAT:
	case VK_FORMAT_R16G16B16A16_UNORM:
	case VK_FORMAT_R32G32_SFLOAT:
	case VK_FORMAT_R16G16B16A16_SINT:
		return 8;
	case VK_FORMAT_R32G32B32_SFLOAT:
		return 12;
	case VK_FORMAT_R32G32B32A32_SFLOAT:
		return 16;
	case VK_FORMAT_BC1_RGB_UNORM_BLOCK:
	case VK_FORMAT_BC1_RGBA_UNORM_BLOCK:
	case VK_FORMAT_BC4_UNORM_BLOCK:
		if (blockW) *blockW = 4;
		return 8;
	case VK_FORMAT_BC2_UNORM_BLOCK:
	case VK_FORMAT_BC3_UNORM_BLOCK:
	case VK_FORMAT_BC5_UNORM_BLOCK:
	case VK_FORMAT_BC7_UNORM_BLOCK:
		if (blockW) *blockW = 4;
		return 16;
	default:
		LogErr("VkFormatBlockSize: unknown format %d", (int)fmt);
		return 4;
	}
}

// VkTex

static bool IsDepthFormat (VkFormat f)
{
	return f == VK_FORMAT_D16_UNORM || f == VK_FORMAT_D32_SFLOAT || f == VK_FORMAT_D24_UNORM_S8_UINT ||
		f == VK_FORMAT_D32_SFLOAT_S8_UINT || f == VK_FORMAT_X8_D24_UNORM_PACK32 || f == VK_FORMAT_S8_UINT;
}

static bool HasStencil (VkFormat f)
{
	return f == VK_FORMAT_D24_UNORM_S8_UINT || f == VK_FORMAT_D32_SFLOAT_S8_UINT || f == VK_FORMAT_S8_UINT;
}

VkTex::VkTex (VkDev *_dev, UINT _w, UINT _h, UINT _levels, VkFormat _fmt, VkImageUsageFlags _usage,
	UINT _layers, bool _cube, VkSampleCountFlagBits _samples)
{
	dev = _dev;
	w = _w, h = _h;
	levels = _levels ? _levels : 1;
	if (_levels == 0) { // D3D9: 0 = the full mip chain
		UINT d = std::max (w, h);
		while (d > 1) { d >>= 1; levels++; }
	}
	fmt = _fmt;
	cube = _cube;
	depth = 1;
	layers = cube ? 6 : (_layers ? _layers : 1);
	samples = _samples;
	usage = _usage;
	layout = VK_IMAGE_LAYOUT_UNDEFINED;
	swizzle = { VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY };
	external = false;
	img = VK_NULL_HANDLE;
	view = VK_NULL_HANDLE;
	alloc = VK_NULL_HANDLE;

	VkImageCreateInfo ii = { VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO };
	ii.flags = cube ? VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT : 0;
	if (dev->sampleLocations && (samples > VK_SAMPLE_COUNT_1_BIT) && (usage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT))
		ii.flags |= VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT; // SetMultisampleAA(false) moves the samples
	ii.imageType = VK_IMAGE_TYPE_2D;
	ii.format = fmt;
	ii.extent = { w, h, 1 };
	ii.mipLevels = levels;
	ii.arrayLayers = layers;
	ii.samples = samples;
	ii.tiling = VK_IMAGE_TILING_OPTIMAL;
	ii.usage = usage;
	ii.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
	ii.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
	VmaAllocationCreateInfo ai = {};
	ai.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE;
	VkResult r = vmaCreateImage (dev->vma, &ii, &ai, &img, &alloc, NULL);
	if (r < 0) {
		LogErr("VkTex: vmaCreateImage failed (%d) %ux%u levels=%u format=%d usage=0x%X", (int)r, w, h, levels, (int)fmt, usage);
		img = VK_NULL_HANDLE;
		return;
	}

	if (!(usage & (VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_STORAGE_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT))) return; // transfer-only: no view
	VkImageViewCreateInfo vi = { VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO };
	vi.image = img;
	vi.viewType = cube ? VK_IMAGE_VIEW_TYPE_CUBE : (layers > 1 ? VK_IMAGE_VIEW_TYPE_2D_ARRAY : VK_IMAGE_VIEW_TYPE_2D);
	vi.format = fmt;
	vi.subresourceRange = { Aspect(), 0, levels, 0, layers };
	if (IsDepth()) vi.subresourceRange.aspectMask = HasStencil (fmt) && fmt == VK_FORMAT_S8_UINT ? VK_IMAGE_ASPECT_STENCIL_BIT : VK_IMAGE_ASPECT_DEPTH_BIT;
	VKCHECK(vkCreateImageView (dev->dev, &vi, NULL, &view));
}

VkTex::VkTex (VkDev *_dev, VkImage image, VkFormat _fmt, UINT _w, UINT _h)
{
	dev = _dev;
	img = image;
	fmt = _fmt;
	w = _w, h = _h;
	levels = layers = depth = 1;
	cube = false;
	samples = VK_SAMPLE_COUNT_1_BIT;
	usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
	layout = VK_IMAGE_LAYOUT_UNDEFINED;
	swizzle = { VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY };
	external = true;
	alloc = VK_NULL_HANDLE;
	VkImageViewCreateInfo vi = { VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO };
	vi.image = img;
	vi.viewType = VK_IMAGE_VIEW_TYPE_2D;
	vi.format = fmt;
	vi.subresourceRange = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1 };
	VKCHECK(vkCreateImageView (dev->dev, &vi, NULL, &view));
}

VkTex::VkTex (VkDev *_dev, VkFormat _fmt, UINT _w, UINT _h, UINT _d, VkImageUsageFlags _usage)
{
	dev = _dev;
	w = _w, h = _h, depth = _d;
	levels = layers = 1;
	fmt = _fmt;
	cube = false;
	samples = VK_SAMPLE_COUNT_1_BIT;
	usage = _usage;
	layout = VK_IMAGE_LAYOUT_UNDEFINED;
	swizzle = { VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY, VK_COMPONENT_SWIZZLE_IDENTITY };
	external = false;
	img = VK_NULL_HANDLE;
	view = VK_NULL_HANDLE;
	alloc = VK_NULL_HANDLE;
	VkImageCreateInfo ii = { VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO };
	ii.imageType = VK_IMAGE_TYPE_3D;
	ii.format = fmt;
	ii.extent = { w, h, depth };
	ii.mipLevels = 1;
	ii.arrayLayers = 1;
	ii.samples = samples;
	ii.tiling = VK_IMAGE_TILING_OPTIMAL;
	ii.usage = usage;
	ii.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
	ii.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
	VmaAllocationCreateInfo ai = {};
	ai.usage = VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE;
	VkResult r = vmaCreateImage (dev->vma, &ii, &ai, &img, &alloc, NULL);
	if (r < 0) {
		LogErr("VkTex: vmaCreateImage failed (%d) %ux%ux%u format=%d", (int)r, w, h, depth, (int)fmt);
		img = VK_NULL_HANDLE;
		return;
	}
	VkImageViewCreateInfo vi = { VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO };
	vi.image = img;
	vi.viewType = VK_IMAGE_VIEW_TYPE_3D;
	vi.format = fmt;
	vi.subresourceRange = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1 };
	VKCHECK(vkCreateImageView (dev->dev, &vi, NULL, &view));
}

void (*VkTex::uiRelease) (uint64_t set, DWORD gen) = NULL;

VkTex::~VkTex ()
{
	dev->ForgetTexture (this);
	if (uiSet && uiRelease) {
		auto f = uiRelease;
		uint64_t s = uiSet;
		DWORD g = uiGen;
		dev->Defer ([f, s, g]() { f (s, g); });
	}
	VkDevice d = dev->dev;
	VmaAllocator vma = dev->vma;
	VkImageView v = view;
	VkImage i = external ? VK_NULL_HANDLE : img;
	VmaAllocation a = alloc;
	dev->Defer ([d, vma, v, i, a]() {
		if (v) vkDestroyImageView (d, v, NULL);
		if (i) vmaDestroyImage (vma, i, a);
	});
}

void VkTex::SetSwizzle (VkComponentMapping swz)
{
	if (!img || external) return;
	VkImageView old = view;
	VkDevice d = dev->dev;
	dev->Defer ([d, old]() { vkDestroyImageView (d, old, NULL); });
	VkImageViewCreateInfo vi = { VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO };
	vi.image = img;
	vi.viewType = depth > 1 ? VK_IMAGE_VIEW_TYPE_3D : (cube ? VK_IMAGE_VIEW_TYPE_CUBE : (layers > 1 ? VK_IMAGE_VIEW_TYPE_2D_ARRAY : VK_IMAGE_VIEW_TYPE_2D));
	vi.format = fmt;
	vi.components = swz;
	swizzle = swz;
	vi.subresourceRange = { Aspect(), 0, levels, 0, layers };
	VKCHECK(vkCreateImageView (dev->dev, &vi, NULL, &view));
}

bool VkTex::IsDepth () const
{
	return IsDepthFormat (fmt);
}

VkImageAspectFlags VkTex::Aspect () const
{
	if (!IsDepthFormat (fmt)) return VK_IMAGE_ASPECT_COLOR_BIT;
	if (fmt == VK_FORMAT_S8_UINT) return VK_IMAGE_ASPECT_STENCIL_BIT;
	return VK_IMAGE_ASPECT_DEPTH_BIT | (HasStencil (fmt) ? VK_IMAGE_ASPECT_STENCIL_BIT : 0);
}

void VkTex::Transition (VkCommandBuffer cmd, VkImageLayout to)
{
	if (layout == to) return;
	VkImageMemoryBarrier2 b = { VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2 };
	b.srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	b.srcAccessMask = VK_ACCESS_2_MEMORY_WRITE_BIT;
	b.dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	b.dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT;
	b.oldLayout = layout;
	b.newLayout = to;
	b.srcQueueFamilyIndex = b.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
	b.image = img;
	b.subresourceRange = { Aspect(), 0, levels, 0, layers };
	VkDependencyInfo di = { VK_STRUCTURE_TYPE_DEPENDENCY_INFO };
	di.imageMemoryBarrierCount = 1;
	di.pImageMemoryBarriers = &b;
	vkCmdPipelineBarrier2 (cmd, &di);
	layout = to;
}

void VkTex::Upload (UINT level, UINT layer, const void *data, VkDeviceSize size, UINT rowPitch)
{
	UINT bw;
	UINT bs = VkFormatBlockSize (fmt, &bw);
	UINT lw = std::max (1u, w >> level), lh = std::max (1u, h >> level);
	VkBuf staging (dev, size, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, true);
	memcpy (staging.Map(), data, size);
	VkCommandBuffer cmd = dev->BeginOneTime ();
	VkImageLayout keep = layout == VK_IMAGE_LAYOUT_UNDEFINED ? VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL : layout;
	Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
	VkBufferImageCopy region = {};
	region.bufferRowLength = rowPitch ? (rowPitch / bs) * bw : 0;
	region.imageSubresource = { Aspect(), level, layer, 1 };
	region.imageExtent = { lw, lh, depth };
	vkCmdCopyBufferToImage (cmd, staging.buf, img, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);
	Transition (cmd, keep);
	Written (level);
	dev->EndOneTime (cmd);
}

void VkTex::GenerateMips (VkCommandBuffer cmd)
{
	if (levels < 2) return;
	Transition (cmd, VK_IMAGE_LAYOUT_GENERAL);
	for (UINT l = 1; l < levels; l++) {
		VkImageBlit2 blit = { VK_STRUCTURE_TYPE_IMAGE_BLIT_2 };
		blit.srcSubresource = { Aspect(), l-1, 0, layers };
		blit.srcOffsets[1] = { (int)std::max (1u, w >> (l-1)), (int)std::max (1u, h >> (l-1)), 1 };
		blit.dstSubresource = { Aspect(), l, 0, layers };
		blit.dstOffsets[1] = { (int)std::max (1u, w >> l), (int)std::max (1u, h >> l), 1 };
		VkBlitImageInfo2 bi = { VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2 };
		bi.srcImage = bi.dstImage = img;
		bi.srcImageLayout = bi.dstImageLayout = VK_IMAGE_LAYOUT_GENERAL;
		bi.regionCount = 1;
		bi.pRegions = &blit;
		bi.filter = VK_FILTER_LINEAR;
		vkCmdBlitImage2 (cmd, &bi);
		VkMemoryBarrier2 mb = { VK_STRUCTURE_TYPE_MEMORY_BARRIER_2 };
		mb.srcStageMask = mb.dstStageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
		mb.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
		mb.dstAccessMask = VK_ACCESS_2_TRANSFER_READ_BIT;
		VkDependencyInfo di = { VK_STRUCTURE_TYPE_DEPENDENCY_INFO };
		di.memoryBarrierCount = 1;
		di.pMemoryBarriers = &mb;
		vkCmdPipelineBarrier2 (cmd, &di);
	}
	Transition (cmd, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
}

// VkSurf

VkSurf::VkSurf (VkTex *_tex, UINT _level, UINT _layer)
{
	tex = _tex;
	dev = tex->Device (); // kept: the surface may outlive the texture (GetSurfaceLevel held a reference in D3D9)
	owner = false;
	level = _level;
	layer = _layer;
	w = std::max (1u, tex->w >> level);
	h = std::max (1u, tex->h >> level);
	MakeView ();
}

void VkSurf::MakeView ()
{
	view = VK_NULL_HANDLE;
	if (!tex->img || !(tex->usage & (VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT))) return;
	// always an own view: attachments need the identity swizzle, and SetSwizzle may replace the texture's view
	VkImageViewCreateInfo vi = { VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO };
	vi.image = tex->img;
	vi.viewType = VK_IMAGE_VIEW_TYPE_2D;
	vi.format = tex->fmt;
	vi.subresourceRange = { tex->Aspect(), level, 1, layer, 1 };
	VKCHECK(vkCreateImageView (tex->Device()->dev, &vi, NULL, &view));
}

VkSurf::VkSurf (VkDev *_dev, UINT _w, UINT _h, VkFormat fmt, VkImageUsageFlags usage, VkSampleCountFlagBits samples)
{
	dev = _dev;
	owner = true;
	level = layer = 0;
	w = _w, h = _h;
	tex = new VkTex (dev, w, h, 1, fmt, usage, 1, false, samples);
	MakeView ();
}

VkSurf::~VkSurf ()
{
	if (dev) dev->ForgetTarget (this);
	if (view) { // always an own view (MakeView)
		VkImageView v = view;
		VkDevice vd = dev->dev;
		dev->Defer ([vd, v]() { vkDestroyImageView (vd, v, NULL); });
	}
	if (owner) delete tex;
}

// VkSamplerDesc

bool VkSamplerDesc::operator== (const VkSamplerDesc &s) const
{
	return mag == s.mag && min == s.min && mip == s.mip && u == s.u && v == s.v && w == s.w &&
		aniso == s.aniso && mipBias == s.mipBias && noMip == s.noMip;
}

// VkDev

static const char *devExt[] = {
	VK_KHR_SWAPCHAIN_EXTENSION_NAME,
	VK_EXT_SHADER_OBJECT_EXTENSION_NAME,
	VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME,
	VK_EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME,
};

VkDev::VkDev (QVulkanInstance *inst, VkPhysicalDevice _phys)
{
	qinst = inst;
	instance = inst->vkInstance();
	phys = _phys;
	dev = VK_NULL_HANDLE;
	queue = VK_NULL_HANDLE;
	vma = VK_NULL_HANDLE;
	iFrame = 0;
	recording = false;
	rtColor = rtDepth = NULL;
	rtExtra[0] = rtExtra[1] = rtExtra[2] = NULL;
	streamStride[0] = streamStride[1] = 0;
	rendering = false;
	scissorSet = false;
	viewport = {};
	scissor = {};
	timelineValue = 0;
	memset (frame, 0, sizeof(frame));
	defTex[0] = defTex[1] = defTex[2] = NULL;
	curVS = curFS = VK_NULL_HANDLE;
	copyVS = copyFS = VK_NULL_HANDLE;
	cbActive = NULL;
	cbSlots = NULL;

	st.depthTest = true;       // D3DRS_ZENABLE defaults to TRUE with an automatic depth buffer
	st.depthWrite = true;
	st.depthFunc = VK_COMPARE_OP_LESS_OR_EQUAL;
	st.cull = VK_CULL_MODE_BACK_BIT;   // D3DCULL_CCW
	st.fill = VK_POLYGON_MODE_FILL;
	st.blend = false;
	st.blendSeparate = false;
	st.blendEq = { VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ZERO, VK_BLEND_OP_ADD, VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ZERO, VK_BLEND_OP_ADD };
	st.writeMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
	st.stencil = false;
	st.stencilOp = VK_COMPARE_OP_ALWAYS;
	st.stencilRef = 0;
	st.stencilMask = 0xFFFFFFFF;
	st.stencilPass = st.stencilFail = st.stencilZFail = VK_STENCIL_OP_KEEP;
	st.biasConst = st.biasSlope = 0.0f;
	st.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
	st.decl = NULL;
	st.msaa = true;             // D3DRS_MULTISAMPLEANTIALIAS defaults to TRUE
	sampleLocations = false;
	sampleLocationCounts = 0;

	vkGetPhysicalDeviceProperties (phys, &props);
	vkGetPhysicalDeviceFeatures (phys, &features);
	CreateDevice ();
}

void VkDev::CreateDevice ()
{
	// queue: the first family with graphics (presentation is checked against the window surface by the framework)
	UINT n = 0;
	vkGetPhysicalDeviceQueueFamilyProperties (phys, &n, NULL);
	std::vector<VkQueueFamilyProperties> qf(n);
	vkGetPhysicalDeviceQueueFamilyProperties (phys, &n, qf.data());
	queueFamily = n;
	for (UINT i = 0; i < n; i++)
		if (qf[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) { queueFamily = i; break; }
	if (queueFamily == n) { LogErr("VkDev: no graphics queue"); return; }

	// features: query what the device has, then switch on what the client uses
	VkPhysicalDeviceVertexInputDynamicStateFeaturesEXT fvi = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT };
	VkPhysicalDeviceExtendedDynamicState3FeaturesEXT fds3 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT, &fvi };
	VkPhysicalDeviceShaderObjectFeaturesEXT fso = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT, &fds3 };
	VkPhysicalDeviceVulkan14Features f14 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES, &fso };
	VkPhysicalDeviceVulkan13Features f13 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES, &f14 };
	VkPhysicalDeviceVulkan12Features f12 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES, &f13 };
	VkPhysicalDeviceFeatures2 f2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2, &f12 };
	vkGetPhysicalDeviceFeatures2 (phys, &f2);

	if (!f13.dynamicRendering || !f13.synchronization2 || !f12.timelineSemaphore || !f12.scalarBlockLayout || !f14.pushDescriptor ||
		!fso.shaderObject || !fvi.vertexInputDynamicState || !fds3.extendedDynamicState3ColorBlendEnable ||
		!fds3.extendedDynamicState3ColorBlendEquation || !fds3.extendedDynamicState3ColorWriteMask) {
		LogErr("VkDev: %s lacks a required feature (dynamic rendering, sync2, timeline semaphores, scalar block layout, "
			"push descriptors, shader objects, dynamic vertex input or dynamic blend state)", props.deviceName);
		return;
	}

	VkPhysicalDeviceFeatures2 e2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 };
	e2.features.samplerAnisotropy = features.samplerAnisotropy;
	e2.features.textureCompressionBC = features.textureCompressionBC;
	e2.features.fillModeNonSolid = features.fillModeNonSolid;
	e2.features.independentBlend = features.independentBlend;
	e2.features.depthClamp = features.depthClamp;
	e2.features.depthBiasClamp = features.depthBiasClamp;
	e2.features.shaderClipDistance = features.shaderClipDistance;
	e2.features.largePoints = features.largePoints; // point sprites (beacons, stars) larger than 1 px
	VkPhysicalDeviceVulkan12Features e12 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES };
	e12.timelineSemaphore = VK_TRUE;
	e12.scalarBlockLayout = VK_TRUE; // shader constants keep the client's tightly packed struct layout
	VkPhysicalDeviceVulkan13Features e13 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES };
	e13.dynamicRendering = VK_TRUE;
	e13.synchronization2 = VK_TRUE;
	e13.maintenance4 = f13.maintenance4;
	e13.shaderDemoteToHelperInvocation = f13.shaderDemoteToHelperInvocation; // glslang emits discard as demote for SPIR-V 1.6
	VkPhysicalDeviceVulkan14Features e14 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES };
	e14.pushDescriptor = VK_TRUE;
	VkPhysicalDeviceShaderObjectFeaturesEXT eso = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT };
	eso.shaderObject = VK_TRUE;
	VkPhysicalDeviceExtendedDynamicState3FeaturesEXT eds3 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT };
	eds3.extendedDynamicState3PolygonMode = fds3.extendedDynamicState3PolygonMode;
	eds3.extendedDynamicState3RasterizationSamples = fds3.extendedDynamicState3RasterizationSamples;
	eds3.extendedDynamicState3SampleMask = fds3.extendedDynamicState3SampleMask;
	eds3.extendedDynamicState3AlphaToCoverageEnable = fds3.extendedDynamicState3AlphaToCoverageEnable;
	eds3.extendedDynamicState3ColorBlendEnable = VK_TRUE;
	eds3.extendedDynamicState3ColorBlendEquation = VK_TRUE;
	eds3.extendedDynamicState3ColorWriteMask = VK_TRUE;
	eds3.extendedDynamicState3DepthClampEnable = fds3.extendedDynamicState3DepthClampEnable;
	VkPhysicalDeviceVertexInputDynamicStateFeaturesEXT evi = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT };
	evi.vertexInputDynamicState = VK_TRUE;
	e2.pNext = &e12; e12.pNext = &e13; e13.pNext = &e14; e14.pNext = &eso; eso.pNext = &eds3; eds3.pNext = &evi;
	features = e2.features;

	// D3DRS_MULTISAMPLEANTIALIAS off: every sample at the pixel centre, which needs VK_EXT_sample_locations with dynamic enable
	std::vector<const char*> ext (devExt, devExt + sizeof(devExt)/sizeof(devExt[0]));
	UINT ne = 0;
	vkEnumerateDeviceExtensionProperties (phys, NULL, &ne, NULL);
	std::vector<VkExtensionProperties> ep(ne);
	vkEnumerateDeviceExtensionProperties (phys, NULL, &ne, ep.data());
	bool hasSL = false;
	for (auto &e : ep) if (!strcmp (e.extensionName, VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME)) hasSL = true;
	if (hasSL && fds3.extendedDynamicState3SampleLocationsEnable) {
		VkPhysicalDeviceSampleLocationsPropertiesEXT slp = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT };
		VkPhysicalDeviceProperties2 pp = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2, &slp };
		vkGetPhysicalDeviceProperties2 (phys, &pp);
		if (slp.variableSampleLocations && slp.sampleLocationCoordinateRange[0] <= 0.5f && slp.sampleLocationCoordinateRange[1] >= 0.5f) {
			ext.push_back (VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME);
			eds3.extendedDynamicState3SampleLocationsEnable = VK_TRUE;
			sampleLocationCounts = slp.sampleLocationSampleCounts;
			sampleLocations = true;
		}
	}

	float prio = 1.0f;
	VkDeviceQueueCreateInfo qi = { VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO };
	qi.queueFamilyIndex = queueFamily;
	qi.queueCount = 1;
	qi.pQueuePriorities = &prio;
	VkDeviceCreateInfo di = { VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO, &e2 };
	di.queueCreateInfoCount = 1;
	di.pQueueCreateInfos = &qi;
	di.enabledExtensionCount = (UINT)ext.size();
	di.ppEnabledExtensionNames = ext.data();
	VkResult r = vkCreateDevice (phys, &di, NULL, &dev);
	if (r < 0) { LogErr("VkDev: vkCreateDevice failed (%d)", (int)r); dev = VK_NULL_HANDLE; return; }
	vkGetDeviceQueue (dev, queueFamily, 0, &queue);

#define VKX(name) vkx.name = (PFN_vk##name)vkGetDeviceProcAddr (dev, "vk" #name)
	VKX(CreateShadersEXT);
	VKX(DestroyShaderEXT);
	VKX(CmdBindShadersEXT);
	VKX(CmdSetVertexInputEXT);
	VKX(CmdSetPolygonModeEXT);
	VKX(CmdSetRasterizationSamplesEXT);
	VKX(CmdSetSampleMaskEXT);
	VKX(CmdSetAlphaToCoverageEnableEXT);
	VKX(CmdSetColorBlendEnableEXT);
	VKX(CmdSetColorBlendEquationEXT);
	VKX(CmdSetColorWriteMaskEXT);
	VKX(CmdSetDepthClampEnableEXT);
	if (sampleLocations) {
		VKX(CmdSetSampleLocationsEnableEXT);
		VKX(CmdSetSampleLocationsEXT);
	}
#undef VKX

	VmaAllocatorCreateInfo ai = {};
	ai.vulkanApiVersion = VK_API_VERSION_1_4;
	ai.physicalDevice = phys;
	ai.device = dev;
	ai.instance = instance;
	VKCHECK(vmaCreateAllocator (&ai, &vma));

	VkSemaphoreTypeCreateInfo sti = { VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO };
	sti.semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE;
	sti.initialValue = 0;
	VkSemaphoreCreateInfo si = { VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO, &sti };
	VKCHECK(vkCreateSemaphore (dev, &si, NULL, &timeline));

	VkCommandPoolCreateInfo pi = { VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO };
	pi.queueFamilyIndex = queueFamily;
	pi.flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT | VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
	VKCHECK(vkCreateCommandPool (dev, &pi, NULL, &oneTimePool));
	for (int i = 0; i < NFRAMES; i++) {
		VKCHECK(vkCreateCommandPool (dev, &pi, NULL, &frame[i].pool));
		VkCommandBufferAllocateInfo ci = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO };
		ci.commandPool = frame[i].pool;
		ci.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
		ci.commandBufferCount = 1;
		VKCHECK(vkAllocateCommandBuffers (dev, &ci, &frame[i].cmd));
		frame[i].transient = new VkBuf (this, 16 << 20, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT |
			VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT, true);
		frame[i].transientUsed = 0;
		frame[i].done = 0;
	}

	// set 0: pushed per draw
	VkDescriptorSetLayoutBinding b[MAXBINDINGS];
	for (int i = 0; i < MAXBINDINGS; i++) {
		b[i] = {};
		b[i].binding = i;
		b[i].descriptorType = i < NUBOS ? VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER : VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		b[i].descriptorCount = 1;
		b[i].stageFlags = VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT;
	}
	VkDescriptorSetLayoutCreateInfo li = { VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO };
	li.flags = VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT;
	li.bindingCount = MAXBINDINGS;
	li.pBindings = b;
	VKCHECK(vkCreateDescriptorSetLayout (dev, &li, NULL, &setLayout));
	VkPushConstantRange pr = { VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT, 0, PUSHCONST };
	VkPipelineLayoutCreateInfo pli = { VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO };
	pli.setLayoutCount = 1;
	pli.pSetLayouts = &setLayout;
	pli.pushConstantRangeCount = 1;
	pli.pPushConstantRanges = &pr;
	VKCHECK(vkCreatePipelineLayout (dev, &pli, NULL, &pipeLayout));

	LogAlw("VkDev: %s, Vulkan %u.%u.%u", props.deviceName, VK_API_VERSION_MAJOR(props.apiVersion),
		VK_API_VERSION_MINOR(props.apiVersion), VK_API_VERSION_PATCH(props.apiVersion));
	LogAlw("VkDev: per-draw multisampling off (sample locations): %s", sampleLocations ? "Yes" : "No");
}

VkDev::~VkDev ()
{
	if (!dev) return;
	vkDeviceWaitIdle (dev);
	for (int i = 0; i < 3; i++) delete defTex[i];
	for (int i = 0; i < NFRAMES; i++) {
		delete frame[i].transient;
		frame[i].transient = NULL;
	}
	for (int i = 0; i < NFRAMES; i++) ReleaseFrame (i);
	for (auto &s : samplers) vkDestroySampler (dev, s.second, NULL);
	if (copyVS) vkx.DestroyShaderEXT (dev, copyVS, NULL);
	if (copyFS) vkx.DestroyShaderEXT (dev, copyFS, NULL);
	vkDestroyPipelineLayout (dev, pipeLayout, NULL);
	vkDestroyDescriptorSetLayout (dev, setLayout, NULL);
	for (int i = 0; i < NFRAMES; i++) vkDestroyCommandPool (dev, frame[i].pool, NULL);
	vkDestroyCommandPool (dev, oneTimePool, NULL);
	vkDestroySemaphore (dev, timeline, NULL);
	vmaDestroyAllocator (vma);
	vkDestroyDevice (dev, NULL);
}

void VkDev::ReleaseFrame (int i)
{
	std::vector<std::function<void()>> rel;
	{
		std::lock_guard<std::mutex> lock (queueLock);
		rel.swap (frame[i].release);
	}
	for (auto &f : rel) f();
}

void VkDev::Defer (std::function<void()> release)
{
	std::lock_guard<std::mutex> lock (queueLock);
	frame[iFrame].release.push_back (release);
}

void VkDev::WaitIdle ()
{
	std::lock_guard<std::mutex> lock (queueLock);
	vkQueueWaitIdle (queue);
}

// frames

VkCommandBuffer VkDev::BeginFrame ()
{
	iFrame = (iFrame + 1) % NFRAMES;
	Frame &f = frame[iFrame];
	if (f.done) {
		VkSemaphoreWaitInfo wi = { VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO };
		wi.semaphoreCount = 1;
		wi.pSemaphores = &timeline;
		wi.pValues = &f.done;
		VKCHECK(vkWaitSemaphores (dev, &wi, UINT64_MAX));
	}
	ReleaseFrame (iFrame);
	f.transientUsed = 0;
	VKCHECK(vkResetCommandPool (dev, f.pool, 0));
	VkCommandBufferBeginInfo bi = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO };
	bi.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
	VKCHECK(vkBeginCommandBuffer (f.cmd, &bi));
	recording = true;
	ReplayState ();
	return f.cmd;
}

void VkDev::EndFrame (VkSemaphore wait, VkSemaphore signal)
{
	if (!recording) return;
	EndRendering ();
	Frame &f = frame[iFrame];
	VKCHECK(vkEndCommandBuffer (f.cmd));
	recording = false;

	VkCommandBufferSubmitInfo ci = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO };
	ci.commandBuffer = f.cmd;
	VkSemaphoreSubmitInfo ws = { VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO };
	ws.semaphore = wait;
	ws.stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	VkSemaphoreSubmitInfo ss[2] = { { VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO }, { VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO } };
	std::lock_guard<std::mutex> lock (queueLock);
	f.done = ++timelineValue;
	ss[0].semaphore = timeline;
	ss[0].value = f.done;
	ss[0].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	ss[1].semaphore = signal;
	ss[1].stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	VkSubmitInfo2 si = { VK_STRUCTURE_TYPE_SUBMIT_INFO_2 };
	si.waitSemaphoreInfoCount = wait ? 1 : 0;
	si.pWaitSemaphoreInfos = &ws;
	si.commandBufferInfoCount = 1;
	si.pCommandBufferInfos = &ci;
	si.signalSemaphoreInfoCount = signal ? 2 : 1;
	si.pSignalSemaphoreInfos = ss;
	VKCHECK(vkQueueSubmit2 (queue, 1, &si, VK_NULL_HANDLE));
}

void VkDev::Flush ()
{
	if (!recording) return;
	EndFrame (VK_NULL_HANDLE, VK_NULL_HANDLE);
	WaitIdle ();
	BeginFrame ();
}

VkCommandBuffer VkDev::BeginOneTime ()
{
	oneTimeLock.lock (); // loader threads record uploads too; released in EndOneTime
	VkCommandBuffer cmd;
	VkCommandBufferAllocateInfo ci = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO };
	ci.commandPool = oneTimePool;
	ci.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	ci.commandBufferCount = 1;
	VKCHECK(vkAllocateCommandBuffers (dev, &ci, &cmd));
	VkCommandBufferBeginInfo bi = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO };
	bi.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
	VKCHECK(vkBeginCommandBuffer (cmd, &bi));
	return cmd;
}

void VkDev::EndOneTime (VkCommandBuffer cmd)
{
	VKCHECK(vkEndCommandBuffer (cmd));
	VkFenceCreateInfo fi = { VK_STRUCTURE_TYPE_FENCE_CREATE_INFO };
	VkFence fence;
	VKCHECK(vkCreateFence (dev, &fi, NULL, &fence));
	VkCommandBufferSubmitInfo ci = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO };
	ci.commandBuffer = cmd;
	VkSubmitInfo2 si = { VK_STRUCTURE_TYPE_SUBMIT_INFO_2 };
	si.commandBufferInfoCount = 1;
	si.pCommandBufferInfos = &ci;
	{
		std::lock_guard<std::mutex> lock (queueLock);
		VKCHECK(vkQueueSubmit2 (queue, 1, &si, fence));
	}
	VKCHECK(vkWaitForFences (dev, 1, &fence, VK_TRUE, UINT64_MAX));
	vkDestroyFence (dev, fence, NULL);
	vkFreeCommandBuffers (dev, oneTimePool, 1, &cmd);
	oneTimeLock.unlock ();
}

VkDeviceSize VkDev::AllocTransient (VkDeviceSize n, VkDeviceSize align, void **ptr)
{
	Frame &f = frame[iFrame];
	if (align < props.limits.minUniformBufferOffsetAlignment) align = props.limits.minUniformBufferOffsetAlignment;
	VkDeviceSize ofs = (f.transientUsed + align - 1) / align * align;
	if (ofs + n > f.transient->size) {
		LogErr("VkDev: per-frame transient buffer full (%llu bytes)", (unsigned long long)f.transient->size);
		ofs = 0;
	}
	f.transientUsed = ofs + n;
	*ptr = (char*)f.transient->Map() + ofs;
	return ofs;
}

VkBuffer VkDev::TransientBuffer () const
{
	return frame[iFrame].transient->buf;
}

// render targets

void VkDev::SetRenderTarget (VkSurf *color, VkSurf *depth)
{
	if (color == rtColor && depth == rtDepth) return;
	EndRendering ();
	rtColor = color;
	rtDepth = depth;
	// D3D9: setting render target 0 resets the viewport to the whole target
	VkSurf *t = color ? color : depth;
	if (t) SetViewport (0.0f, 0.0f, (float)t->w, (float)t->h);
	scissorSet = false;
}
void VkDev::SetRenderTargetN (UINT idx, VkSurf *color)
{
	if (idx == 0) { SetRenderTarget (color, rtDepth); return; }
	if (idx > 3 || rtExtra[idx - 1] == color) return;
	EndRendering ();
	rtExtra[idx - 1] = color;
}
void VkDev::ForgetTexture (const VkTex *t)
{
	std::lock_guard<std::mutex> lock (constLock);
	for (auto cb : constBufs) cb->DropTexture (t);
}

void VkDev::RegisterConstants (VkConstBuffer *cb, bool add)
{
	std::lock_guard<std::mutex> lock (constLock);
	if (add) constBufs.push_back (cb);
	else constBufs.erase (std::remove (constBufs.begin(), constBufs.end(), cb), constBufs.end());
	if (!add && cbActive == cb) cbActive = NULL;
}

void VkDev::ForgetTarget (const VkSurf *s)
{
	if (s != rtColor && s != rtDepth && s != rtExtra[0] && s != rtExtra[1] && s != rtExtra[2]) return;
	EndRendering ();
	if (rtColor == s) rtColor = NULL;
	if (rtDepth == s) rtDepth = NULL;
	for (auto &e : rtExtra) if (e == s) e = NULL;
}

void VkDev::BeginRendering ()
{
	if (rendering || !recording || (!rtColor && !rtDepth)) return;
	VkCommandBuffer cmd = Cmd();
	VkRenderingAttachmentInfo ca[4] = { { VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO } };
	VkRenderingAttachmentInfo da = { VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO };
	VkRenderingAttachmentInfo sa = { VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO };
	VkRenderingInfo ri = { VK_STRUCTURE_TYPE_RENDERING_INFO };
	VkSurf *t = rtColor ? rtColor : rtDepth;
	ri.renderArea = { { 0, 0 }, { t->w, t->h } };
	ri.layerCount = 1;
	if (rtColor) {
		VkSurf *c[4] = { rtColor, rtExtra[0], rtExtra[1], rtExtra[2] };
		UINT n = 0;
		for (; n < 4 && c[n]; n++) {
			c[n]->tex->Transition (cmd, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);
			c[n]->tex->Written (c[n]->level);
			ca[n] = { VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO };
			ca[n].imageView = c[n]->view;
			ca[n].imageLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
			ca[n].loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
			ca[n].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
		}
		ri.colorAttachmentCount = n;
		ri.pColorAttachments = ca;
	}
	if (rtDepth) {
		rtDepth->tex->Transition (cmd, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);
		da.imageView = rtDepth->view;
		da.imageLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
		da.loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
		da.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
		ri.pDepthAttachment = &da;
		if (rtDepth->tex->Aspect() & VK_IMAGE_ASPECT_STENCIL_BIT) {
			sa = da;
			ri.pStencilAttachment = &sa;
		}
	}
	vkCmdBeginRendering (cmd, &ri);
	rendering = true;
	vkCmdSetViewportWithCount (cmd, 1, &viewport);
	VkRect2D sc = scissorSet ? scissor : ri.renderArea;
	vkCmdSetScissorWithCount (cmd, 1, &sc);
	vkx.CmdSetRasterizationSamplesEXT (cmd, t->tex->samples);
	ApplySampleLocations ();
}

void VkDev::EndRendering ()
{
	if (!rendering) return;
	VkCommandBuffer cmd = Cmd();
	vkCmdEndRendering (cmd);
	rendering = false;
	if (rtColor && (rtColor->tex->usage & VK_IMAGE_USAGE_SAMPLED_BIT)) rtColor->tex->Transition (cmd, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
	for (VkSurf *e : rtExtra) if (e && (e->tex->usage & VK_IMAGE_USAGE_SAMPLED_BIT)) e->tex->Transition (cmd, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
	if (rtDepth && (rtDepth->tex->usage & VK_IMAGE_USAGE_SAMPLED_BIT)) rtDepth->tex->Transition (cmd, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
}

void VkDev::Clear (bool color, bool depth, bool stencil, DWORD argb, float z, DWORD s, const RECT *r)
{
	BeginRendering ();
	if (!rendering) return;
	VkClearAttachment ca[5];
	UINT n = 0;
	for (UINT i = 0; color && rtColor && i < 4 && (i == 0 || rtExtra[i - 1]); i++) { // D3D9 Clear clears every render target
		ca[n] = {};
		ca[n].aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
		ca[n].colorAttachment = i;
		ca[n].clearValue.color = { { ((argb >> 16) & 0xFF)/255.0f, ((argb >> 8) & 0xFF)/255.0f, (argb & 0xFF)/255.0f, ((argb >> 24) & 0xFF)/255.0f } };
		n++;
	}
	if ((depth || stencil) && rtDepth) {
		ca[n] = {};
		if (depth) ca[n].aspectMask |= VK_IMAGE_ASPECT_DEPTH_BIT;
		if (stencil && (rtDepth->tex->Aspect() & VK_IMAGE_ASPECT_STENCIL_BIT)) ca[n].aspectMask |= VK_IMAGE_ASPECT_STENCIL_BIT;
		ca[n].clearValue.depthStencil = { z, s };
		if (ca[n].aspectMask) n++;
	}
	if (!n) return;
	VkSurf *t = rtColor ? rtColor : rtDepth;
	VkClearRect cr = {};
	cr.rect = r ? VkRect2D{ { r->left, r->top }, { UINT(r->right - r->left), UINT(r->bottom - r->top) } } : VkRect2D{ { 0, 0 }, { t->w, t->h } };
	cr.layerCount = 1;
	vkCmdClearAttachments (Cmd(), n, ca, 1, &cr);
}

void VkDev::SetViewport (float x, float y, float w, float h, float minz, float maxz)
{
	// negative height keeps Direct3D's y-up clip space
	viewport = { x, y + h, w, -h, minz, maxz };
	if (rendering) vkCmdSetViewportWithCount (Cmd(), 1, &viewport);
}

void VkDev::SetScissor (const RECT *r)
{
	scissorSet = (r != NULL);
	if (r) scissor = { { r->left, r->top }, { UINT(r->right - r->left), UINT(r->bottom - r->top) } };
	if (!rendering) return;
	VkSurf *t = rtColor ? rtColor : rtDepth;
	VkRect2D sc = scissorSet ? scissor : VkRect2D{ { 0, 0 }, { t->w, t->h } };
	vkCmdSetScissorWithCount (Cmd(), 1, &sc);
}

// dynamic state

void VkDev::SetState (const State &s)
{
	st = s;
	if (recording) ReplayState ();
}

bool VkDev::GetScissor (RECT *r) const
{
	r->left = scissor.offset.x;
	r->top = scissor.offset.y;
	r->right = scissor.offset.x + (LONG)scissor.extent.width;
	r->bottom = scissor.offset.y + (LONG)scissor.extent.height;
	return scissorSet;
}

void VkDev::ReplayState ()
{
	VkCommandBuffer cmd = Cmd();
	vkCmdSetRasterizerDiscardEnable (cmd, VK_FALSE);
	vkCmdSetFrontFace (cmd, VK_FRONT_FACE_CLOCKWISE); // Direct3D front faces are clockwise on screen
	vkCmdSetCullMode (cmd, st.cull);
	vkCmdSetDepthTestEnable (cmd, st.depthTest);
	vkCmdSetDepthWriteEnable (cmd, st.depthWrite);
	vkCmdSetDepthCompareOp (cmd, st.depthFunc);
	vkCmdSetDepthBoundsTestEnable (cmd, VK_FALSE);
	vkCmdSetDepthBiasEnable (cmd, st.biasConst != 0.0f || st.biasSlope != 0.0f);
	vkCmdSetDepthBias (cmd, st.biasConst, 0.0f, st.biasSlope);
	vkCmdSetStencilTestEnable (cmd, st.stencil);
	vkCmdSetStencilOp (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, st.stencilFail, st.stencilPass, st.stencilZFail, st.stencilOp);
	vkCmdSetStencilCompareMask (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, st.stencilMask);
	vkCmdSetStencilWriteMask (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, 0xFFFFFFFF);
	vkCmdSetStencilReference (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, st.stencilRef);
	vkCmdSetPrimitiveTopology (cmd, st.topology);
	vkCmdSetPrimitiveRestartEnable (cmd, VK_FALSE);
	vkCmdSetLineWidth (cmd, 1.0f);
	vkx.CmdSetPolygonModeEXT (cmd, st.fill);
	VkSampleMask mask = 0xFFFFFFFF;
	VkSurf *t = rtColor ? rtColor : rtDepth;
	vkx.CmdSetRasterizationSamplesEXT (cmd, (rendering && t) ? t->tex->samples : VK_SAMPLE_COUNT_1_BIT);
	ApplySampleLocations ();
	vkx.CmdSetSampleMaskEXT (cmd, VK_SAMPLE_COUNT_32_BIT, &mask); // one mask word covers up to 32 samples
	vkx.CmdSetAlphaToCoverageEnableEXT (cmd, VK_FALSE);
	vkx.CmdSetDepthClampEnableEXT (cmd, VK_FALSE);
	VkBool32 blend[4] = { st.blend, st.blend, st.blend, st.blend }; // the same for every MRT attachment, as D3D9 without independent blend
	VkColorBlendEquationEXT eq[4] = { st.blendEq, st.blendEq, st.blendEq, st.blendEq };
	VkColorComponentFlags wm[4] = { st.writeMask, st.writeMask, st.writeMask, st.writeMask };
	vkx.CmdSetColorBlendEnableEXT (cmd, 0, 4, blend);
	vkx.CmdSetColorBlendEquationEXT (cmd, 0, 4, eq);
	vkx.CmdSetColorWriteMaskEXT (cmd, 0, 4, wm);
	if (st.decl) SetVertexDecl (st.decl);
	else vkx.CmdSetVertexInputEXT (cmd, 0, NULL, 0, NULL);
	if (curVS || curFS) BindShaders (curVS, curFS);
	if (cbActive) cbActive->Invalidate (); // push descriptors don't carry over to a new command buffer
}

void VkDev::SetMultisampleAA (bool enable)
{
	st.msaa = enable;
	if (recording) ApplySampleLocations ();
}

// D3DRS_MULTISAMPLEANTIALIAS FALSE: samples rasterized as one at the pixel centre, all written (no effect on single-sampled targets)
void VkDev::ApplySampleLocations ()
{
	if (!sampleLocations) return;
	VkCommandBuffer cmd = Cmd();
	VkSurf *t = rtColor ? rtColor : rtDepth;
	VkSampleCountFlagBits n = (rendering && t) ? t->tex->samples : VK_SAMPLE_COUNT_1_BIT;
	bool centre = !st.msaa && (n > VK_SAMPLE_COUNT_1_BIT) && (sampleLocationCounts & n);
	vkx.CmdSetSampleLocationsEnableEXT (cmd, centre);
	if (centre) {
		VkSampleLocationEXT loc[16];
		for (auto &l : loc) l = { 0.5f, 0.5f };
		VkSampleLocationsInfoEXT si = { VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT };
		si.sampleLocationsPerPixel = n;
		si.sampleLocationGridSize = { 1, 1 };
		si.sampleLocationsCount = (UINT)n;
		si.pSampleLocations = loc;
		vkx.CmdSetSampleLocationsEXT (cmd, &si);
	}
}

void VkDev::SetDepthTest (bool enable)
{
	st.depthTest = enable;
	if (recording) vkCmdSetDepthTestEnable (Cmd(), enable);
}

void VkDev::SetDepthWrite (bool enable)
{
	st.depthWrite = enable;
	if (recording) vkCmdSetDepthWriteEnable (Cmd(), enable);
}

void VkDev::SetDepthFunc (VkCompareOp op)
{
	st.depthFunc = op;
	if (recording) vkCmdSetDepthCompareOp (Cmd(), op);
}

void VkDev::SetCullMode (VkCullModeFlags mode)
{
	st.cull = mode;
	if (recording) vkCmdSetCullMode (Cmd(), mode);
}

void VkDev::SetFillMode (VkPolygonMode mode)
{
	st.fill = mode;
	if (recording) vkx.CmdSetPolygonModeEXT (Cmd(), mode);
}

void VkDev::SetBlend (bool enable)
{
	st.blend = enable;
	VkBool32 b[4] = { enable, enable, enable, enable };
	if (recording) vkx.CmdSetColorBlendEnableEXT (Cmd(), 0, 4, b);
}

void VkDev::SetBlendFunc (VkBlendFactor src, VkBlendFactor dst, VkBlendOp op)
{
	st.blendEq.srcColorBlendFactor = src;
	st.blendEq.dstColorBlendFactor = dst;
	st.blendEq.colorBlendOp = op;
	if (!st.blendSeparate) { // D3DRS_SEPARATEALPHABLENDENABLE off: alpha blends like colour
		st.blendEq.srcAlphaBlendFactor = src;
		st.blendEq.dstAlphaBlendFactor = dst;
		st.blendEq.alphaBlendOp = op;
	}
	if (recording) { VkColorBlendEquationEXT q[4] = { st.blendEq, st.blendEq, st.blendEq, st.blendEq }; vkx.CmdSetColorBlendEquationEXT (Cmd(), 0, 4, q); }
}

void VkDev::SetSrcBlend (VkBlendFactor src)
{
	SetBlendFunc (src, st.blendEq.dstColorBlendFactor, st.blendEq.colorBlendOp);
}

void VkDev::SetDestBlend (VkBlendFactor dst)
{
	SetBlendFunc (st.blendEq.srcColorBlendFactor, dst, st.blendEq.colorBlendOp);
}

void VkDev::SetBlendFuncAlpha (VkBlendFactor src, VkBlendFactor dst, VkBlendOp op)
{
	st.blendSeparate = true;
	st.blendEq.srcAlphaBlendFactor = src;
	st.blendEq.dstAlphaBlendFactor = dst;
	st.blendEq.alphaBlendOp = op;
	if (recording) { VkColorBlendEquationEXT q[4] = { st.blendEq, st.blendEq, st.blendEq, st.blendEq }; vkx.CmdSetColorBlendEquationEXT (Cmd(), 0, 4, q); }
}

void VkDev::SetColorWrite (VkColorComponentFlags mask)
{
	st.writeMask = mask;
	VkColorComponentFlags m[4] = { mask, mask, mask, mask };
	if (recording) vkx.CmdSetColorWriteMaskEXT (Cmd(), 0, 4, m);
}

void VkDev::SetStencil (bool enable, VkCompareOp op, UINT ref, UINT mask, VkStencilOp pass, VkStencilOp fail, VkStencilOp zfail)
{
	st.stencil = enable;
	st.stencilOp = op;
	st.stencilRef = ref;
	st.stencilMask = mask;
	st.stencilPass = pass;
	st.stencilFail = fail;
	st.stencilZFail = zfail;
	if (!recording) return;
	VkCommandBuffer cmd = Cmd();
	vkCmdSetStencilTestEnable (cmd, enable);
	vkCmdSetStencilOp (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, fail, pass, zfail, op);
	vkCmdSetStencilCompareMask (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, mask);
	vkCmdSetStencilReference (cmd, VK_STENCIL_FACE_FRONT_AND_BACK, ref);
}

void VkDev::SetDepthBias (float constant, float slope)
{
	st.biasConst = constant;
	st.biasSlope = slope;
	if (!recording) return;
	vkCmdSetDepthBiasEnable (Cmd(), constant != 0.0f || slope != 0.0f);
	vkCmdSetDepthBias (Cmd(), constant, 0.0f, slope);
}

void VkDev::SetTopology (VkPrimitiveTopology t)
{
	st.topology = t;
	if (recording) vkCmdSetPrimitiveTopology (Cmd(), t);
}

void VkDev::SetVertexDecl (const VkVertexDecl *decl)
{
	st.decl = decl;
	if (!recording) return;
	if (decl) {
		// strides come from SetStreamSource, as in D3D9; the declaration's own are the fallback
		VkVertexInputBindingDescription2EXT b[2];
		UINT n = std::min ((UINT)decl->bind.size(), 2u);
		for (UINT i = 0; i < n; i++) {
			b[i] = decl->bind[i];
			if (b[i].binding < 2 && streamStride[b[i].binding]) b[i].stride = streamStride[b[i].binding];
		}
		vkx.CmdSetVertexInputEXT (Cmd(), n, b, (UINT)decl->attr.size(), decl->attr.data());
	}
	else vkx.CmdSetVertexInputEXT (Cmd(), 0, NULL, 0, NULL);
}

void VkDev::SetStreamSource (UINT stream, VkBuffer vb, VkDeviceSize offset, UINT stride)
{
	bool changed = (stream < 2 && streamStride[stream] != stride);
	if (stream < 2) streamStride[stream] = stride;
	if (!recording || !vb) return;
	if (changed && st.decl) SetVertexDecl (st.decl); // the vertex input state carries the stride as well
	VkDeviceSize s = stride;
	vkCmdBindVertexBuffers2 (Cmd(), stream, 1, &vb, &offset, NULL, &s);
}

void VkDev::SetIndices (VkBuffer ib, VkDeviceSize offset, VkIndexType type)
{
	if (recording && ib) vkCmdBindIndexBuffer (Cmd(), ib, offset, type);
}

// samplers

VkSampler VkDev::Sampler (const VkSamplerDesc &d)
{
	std::lock_guard<std::mutex> lock (queueLock);
	for (auto &s : samplers) if (s.first == d) return s.second;
	VkSamplerCreateInfo si = { VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO };
	si.magFilter = d.mag;
	si.minFilter = d.min;
	si.mipmapMode = d.noMip ? VK_SAMPLER_MIPMAP_MODE_NEAREST : d.mip;
	si.addressModeU = d.u;
	si.addressModeV = d.v;
	si.addressModeW = d.w;
	si.mipLodBias = d.mipBias;
	si.anisotropyEnable = (d.aniso > 1.0f && features.samplerAnisotropy);
	si.maxAnisotropy = std::min (d.aniso, props.limits.maxSamplerAnisotropy);
	si.minLod = 0.0f;
	si.maxLod = d.noMip ? 0.25f : VK_LOD_CLAMP_NONE;
	si.borderColor = VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
	VkSampler s;
	VKCHECK(vkCreateSampler (dev, &si, NULL, &s));
	samplers.push_back ({ d, s });
	return s;
}

// shaders and draws

void VkDev::BindShaders (VkShaderEXT vs, VkShaderEXT fs)
{
	curVS = vs;
	curFS = fs;
	if (!recording) return;
	VkShaderStageFlagBits stages[2] = { VK_SHADER_STAGE_VERTEX_BIT, VK_SHADER_STAGE_FRAGMENT_BIT };
	VkShaderEXT sh[2] = { vs, fs };
	vkx.CmdBindShadersEXT (Cmd(), 2, stages, sh);
}

void VkDev::SetConstantSource (VkConstBuffer *cb, const std::vector<VkSamplerSlot> *smpSlots)
{
	cbActive = cb;
	cbSlots = smpSlots;
	if (cb) cb->Invalidate ();
}

void VkDev::PreDraw ()
{
	if (cbActive && cbSlots && cbActive->IsDirty ()) cbActive->Push (*cbSlots); // may end rendering to change a texture's layout
}

void VkDev::DrawPrimitive (VkPrimitiveTopology t, UINT startVertex, UINT vertexCount)
{
	PreDraw ();
	BeginRendering ();
	if (!rendering || !vertexCount) return;
	SetTopology (t);
	vkCmdDraw (Cmd(), vertexCount, 1, startVertex, 0);
}

void VkDev::DrawIndexedPrimitive (VkPrimitiveTopology t, int baseVertex, UINT startIndex, UINT indexCount)
{
	PreDraw ();
	BeginRendering ();
	if (!rendering || !indexCount) return;
	SetTopology (t);
	vkCmdDrawIndexed (Cmd(), indexCount, 1, startIndex, baseVertex, 0);
}

void VkDev::DrawPrimitiveUP (VkPrimitiveTopology t, UINT vertexCount, const void *vtx, UINT stride)
{
	void *p;
	VkDeviceSize ofs = AllocTransient (vertexCount * stride, 16, &p);
	memcpy (p, vtx, vertexCount * stride);
	SetStreamSource (0, TransientBuffer(), ofs, stride);
	DrawPrimitive (t, 0, vertexCount);
}

void VkDev::DrawIndexedPrimitiveUP (VkPrimitiveTopology t, UINT vertexCount, UINT indexCount, const void *idx, VkIndexType it,
	const void *vtx, UINT stride)
{
	void *pv, *pi;
	UINT isz = indexCount * (it == VK_INDEX_TYPE_UINT32 ? 4 : 2);
	VkDeviceSize vo = AllocTransient (vertexCount * stride, 16, &pv);
	memcpy (pv, vtx, vertexCount * stride);
	VkDeviceSize io = AllocTransient (isz, 16, &pi);
	memcpy (pi, idx, isz);
	SetStreamSource (0, TransientBuffer(), vo, stride);
	SetIndices (TransientBuffer(), io, it);
	DrawIndexedPrimitive (t, 0, 0, indexCount);
}

void VkDev::PrepareSample (VkTex *t)
{
	if (t && recording && t->mipsDirty) { // D3DUSAGE_AUTOGENMIPMAP: the runtime rebuilds the sublevels when the texture is next used
		EndRendering ();
		t->GenerateMips (Cmd());
		t->mipsDirty = false;
	}
	if (!t || !recording || t->layout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) return;
	EndRendering (); // no layout changes inside a rendering pass
	t->Transition (Cmd(), VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
}

VkTex *VkDev::DefaultTexture (VkImageViewType type)
{
	int i = type == VK_IMAGE_VIEW_TYPE_CUBE ? 1 : (type == VK_IMAGE_VIEW_TYPE_3D ? 2 : 0);
	if (!defTex[i]) {
		static const DWORD black = 0xFF000000; // D3D9 samples (0,0,0,1) without a texture
		VkImageUsageFlags u = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
		if (i == 2) defTex[i] = new VkTex (this, VK_FORMAT_B8G8R8A8_UNORM, 1, 1, 1, u);
		else defTex[i] = new VkTex (this, 1, 1, 1, VK_FORMAT_B8G8R8A8_UNORM, u, 1, i == 1);
		for (UINT l = 0; l < defTex[i]->layers; l++) defTex[i]->Upload (0, l, &black, 4);
	}
	return defTex[i];
}

VkResult VkDev::QueuePresent (const VkPresentInfoKHR *pi)
{
	std::lock_guard<std::mutex> lock (queueLock);
	return vkQueuePresentKHR (queue, pi);
}

// surface operations

static void TransferDone (VkCommandBuffer cmd) // later transfers and draws see the result
{
	VkMemoryBarrier2 mb = { VK_STRUCTURE_TYPE_MEMORY_BARRIER_2 };
	mb.srcStageMask = VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT;
	mb.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
	mb.dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	mb.dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT;
	VkDependencyInfo di = { VK_STRUCTURE_TYPE_DEPENDENCY_INFO };
	di.memoryBarrierCount = 1;
	di.pMemoryBarriers = &mb;
	vkCmdPipelineBarrier2 (cmd, &di);
}

void VkDev::StretchRect (VkSurf *src, const RECT *sr, VkSurf *dst, const RECT *dr, VkFilter filter)
{
	if (!recording || !src || !dst || !src->tex->img || !dst->tex->img) return;
	EndRendering ();
	VkCommandBuffer cmd = Cmd ();
	RECT s = sr ? *sr : RECT{ 0, 0, (LONG)src->w, (LONG)src->h };
	RECT d = dr ? *dr : RECT{ 0, 0, (LONG)dst->w, (LONG)dst->h };
	VkTex *st = src->tex, *dt = dst->tex;
	VkTex *tmp = NULL;
	if (st->samples != VK_SAMPLE_COUNT_1_BIT) { // D3D9 resolves a multisampled source
		tmp = new VkTex (this, src->w, src->h, 1, st->fmt, VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT);
		st->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
		tmp->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
		VkImageResolve r = {};
		r.srcSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
		r.dstSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
		r.extent = { src->w, src->h, 1 };
		vkCmdResolveImage (cmd, st->img, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, tmp->img, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &r);
		TransferDone (cmd);
		st = tmp;
	}
	VkTex *ms = NULL;
	RECT bd = d;
	UINT bl = dst->level, by = dst->layer;
	if (dt->samples != VK_SAMPLE_COUNT_1_BIT) { // blits can't write a multisampled image: scale into a temp, then draw that
		if (dt->IsDepth () || d.right <= d.left || d.bottom <= d.top) { delete tmp; return; }
		ms = new VkTex (this, UINT(d.right - d.left), UINT(d.bottom - d.top), 1, dt->fmt, VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT);
		dt = ms;
		bd = RECT{ 0, 0, d.right - d.left, d.bottom - d.top };
		bl = by = 0;
	}
	bool same = (st == dt);
	VkImageLayout sl = same ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
	VkImageLayout dl = same ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
	st->Transition (cmd, sl);
	if (!same) dt->Transition (cmd, dl);
	VkImageBlit2 b = { VK_STRUCTURE_TYPE_IMAGE_BLIT_2 };
	b.srcSubresource = { st->Aspect(), tmp ? 0 : src->level, tmp ? 0 : src->layer, 1 };
	b.srcOffsets[0] = { s.left, s.top, 0 };
	b.srcOffsets[1] = { s.right, s.bottom, 1 };
	b.dstSubresource = { dt->Aspect(), bl, by, 1 };
	b.dstOffsets[0] = { bd.left, bd.top, 0 };
	b.dstOffsets[1] = { bd.right, bd.bottom, 1 };
	VkBlitImageInfo2 bi = { VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2 };
	bi.srcImage = st->img;
	bi.srcImageLayout = sl;
	bi.dstImage = dt->img;
	bi.dstImageLayout = dl;
	bi.regionCount = 1;
	bi.pRegions = &b;
	bi.filter = filter;
	vkCmdBlitImage2 (cmd, &bi);
	TransferDone (cmd);
	if (!ms) dst->tex->Written (dst->level);
	if (ms) DrawCopy (ms, dst, d);
	delete ms;
	delete tmp;
}

void VkDev::DrawCopy (VkTex *src, VkSurf *dst, const RECT &d)
{
	if (!copyVS && !copyFS) {
		static const char *vs = "layout(location = 0) out vec2 uv;\n"
			"void main () { uv = vec2 ((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2); gl_Position = vec4 (uv * 2.0 - 1.0, 0.0, 1.0); }\n";
		static const char *fs = "layout(binding = 4) uniform sampler2D src;\n"
			"layout(location = 0) in vec2 uv;\n"
			"layout(location = 0) out vec4 col;\n"
			"void main () { col = texture (src, uv); }\n";
		std::vector<uint32_t> spv;
		if (VkCompileGLSL ("StretchRect", vs, NULL, VK_SHADER_STAGE_VERTEX_BIT, VkMacros(), spv, NULL)) copyVS = VkCreateShaderObject (this, spv, VK_SHADER_STAGE_VERTEX_BIT);
		if (VkCompileGLSL ("StretchRect", fs, NULL, VK_SHADER_STAGE_FRAGMENT_BIT, VkMacros(), spv, NULL)) copyFS = VkCreateShaderObject (this, spv, VK_SHADER_STAGE_FRAGMENT_BIT);
	}
	if (!copyVS || !copyFS) return;
	VkSurf *oc = rtColor, *od = rtDepth, *oe[3] = { rtExtra[0], rtExtra[1], rtExtra[2] };
	VkViewport ov = viewport;
	VkRect2D os = scissor;
	bool oss = scissorSet;
	EndRendering ();
	src->Transition (Cmd(), VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
	rtColor = dst;
	rtDepth = rtExtra[0] = rtExtra[1] = rtExtra[2] = NULL;
	viewport = { (float)d.left, (float)d.top, float(d.right - d.left), float(d.bottom - d.top), 0.0f, 1.0f }; // same size as src: one texel per pixel
	scissor = { { d.left, d.top }, { UINT(d.right - d.left), UINT(d.bottom - d.top) } };
	scissorSet = true;
	BeginRendering ();
	VkCommandBuffer cmd = Cmd ();
	VkShaderStageFlagBits stages[2] = { VK_SHADER_STAGE_VERTEX_BIT, VK_SHADER_STAGE_FRAGMENT_BIT };
	VkShaderEXT sh[2] = { copyVS, copyFS };
	vkx.CmdBindShadersEXT (cmd, 2, stages, sh);
	vkCmdSetCullMode (cmd, VK_CULL_MODE_NONE);
	vkCmdSetDepthTestEnable (cmd, VK_FALSE);
	vkCmdSetDepthWriteEnable (cmd, VK_FALSE);
	vkCmdSetDepthBiasEnable (cmd, VK_FALSE);
	vkCmdSetStencilTestEnable (cmd, VK_FALSE);
	vkCmdSetPrimitiveTopology (cmd, VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
	vkx.CmdSetPolygonModeEXT (cmd, VK_POLYGON_MODE_FILL);
	vkx.CmdSetVertexInputEXT (cmd, 0, NULL, 0, NULL);
	VkBool32 be = VK_FALSE;
	VkColorComponentFlags wm = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
	vkx.CmdSetColorBlendEnableEXT (cmd, 0, 1, &be);
	vkx.CmdSetColorWriteMaskEXT (cmd, 0, 1, &wm);
	VkSamplerDesc sd = { VK_FILTER_NEAREST, VK_FILTER_NEAREST, VK_SAMPLER_MIPMAP_MODE_NEAREST, VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
		VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, 0.0f, 0.0f, true };
	VkDescriptorImageInfo ii = { Sampler (sd), src->view, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL };
	VkWriteDescriptorSet w = { VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET };
	w.dstBinding = 4;
	w.descriptorCount = 1;
	w.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
	w.pImageInfo = &ii;
	vkCmdPushDescriptorSet (cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeLayout, 0, 1, &w);
	vkCmdDraw (cmd, 3, 1, 0, 0);
	EndRendering ();
	rtColor = oc;
	rtDepth = od;
	for (int i = 0; i < 3; i++) rtExtra[i] = oe[i];
	viewport = ov;
	scissor = os;
	scissorSet = oss;
	ReplayState (); // back to the client's shaders and states
}

void VkDev::CopySurface (VkSurf *src, const RECT *sr, VkSurf *dst, const POINT *dp)
{
	if (!recording || !src || !dst || !src->tex->img || !dst->tex->img) return;
	EndRendering ();
	VkCommandBuffer cmd = Cmd ();
	RECT s = sr ? *sr : RECT{ 0, 0, (LONG)src->w, (LONG)src->h };
	bool same = (src->tex == dst->tex);
	VkImageLayout sl = same ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
	VkImageLayout dl = same ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
	src->tex->Transition (cmd, sl);
	if (!same) dst->tex->Transition (cmd, dl);
	VkImageCopy2 c = { VK_STRUCTURE_TYPE_IMAGE_COPY_2 };
	c.srcSubresource = { src->tex->Aspect(), src->level, src->layer, 1 };
	c.srcOffset = { s.left, s.top, 0 };
	c.dstSubresource = { dst->tex->Aspect(), dst->level, dst->layer, 1 };
	c.dstOffset = { dp ? dp->x : 0, dp ? dp->y : 0, 0 };
	c.extent = { (UINT)(s.right - s.left), (UINT)(s.bottom - s.top), 1 };
	VkCopyImageInfo2 ci = { VK_STRUCTURE_TYPE_COPY_IMAGE_INFO_2 };
	ci.srcImage = src->tex->img;
	ci.srcImageLayout = sl;
	ci.dstImage = dst->tex->img;
	ci.dstImageLayout = dl;
	ci.regionCount = 1;
	ci.pRegions = &c;
	vkCmdCopyImage2 (cmd, &ci);
	TransferDone (cmd);
	dst->tex->Written (dst->level);
}

void VkDev::ColorFill (VkSurf *s, const RECT *r, DWORD argb)
{
	if (!recording || !s || !s->tex->img) return;
	if (s->tex->usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) { // clear through a rendering pass; the device state stays as it was
		VkSurf *oc = rtColor, *od = rtDepth, *oe[3] = { rtExtra[0], rtExtra[1], rtExtra[2] };
		VkViewport ov = viewport;
		EndRendering ();
		rtExtra[0] = rtExtra[1] = rtExtra[2] = NULL;
		VkRect2D os = scissor;
		bool oss = scissorSet;
		SetRenderTarget (s, NULL);
		Clear (true, false, false, argb, 1.0f, 0, r);
		EndRendering ();
		for (int i = 0; i < 3; i++) rtExtra[i] = oe[i];
		SetRenderTarget (oc, od);
		viewport = ov;
		scissor = os;
		scissorSet = oss;
		return;
	}
	EndRendering ();
	VkCommandBuffer cmd = Cmd ();
	s->tex->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
	VkClearColorValue c = { { ((argb >> 16) & 0xFF)/255.0f, ((argb >> 8) & 0xFF)/255.0f, (argb & 0xFF)/255.0f, ((argb >> 24) & 0xFF)/255.0f } };
	VkImageSubresourceRange rr = { VK_IMAGE_ASPECT_COLOR_BIT, s->level, 1, s->layer, 1 };
	vkCmdClearColorImage (cmd, s->tex->img, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, &c, 1, &rr); // whole level: images without attachment use can't clear a rectangle
	TransferDone (cmd);
	s->tex->Written (s->level);
}
