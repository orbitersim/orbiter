// not upstream: Vulkan objects that replace the Direct3D 9 runtime objects (no D3D9 interface emulation)

#ifndef __VKCORE_H
#define __VKCORE_H

#include <vulkan/vulkan.h>
#include "vk_mem_alloc.h"
#include "OrbiterPlatform.h"
#include <vector>
#include <functional>
#include <unordered_map>
#include <mutex>

class QVulkanInstance;
class VkDev;
class VkTex;
class VkConstBuffer;                                 // VkShader.h
struct VkSamplerSlot;
class VkConstTable;                                  // LPD3DXCONSTANTTABLE counterpart (VkShader.h)
struct VkConstEntry;
typedef const VkConstEntry *VkConstHandle;           // D3DXHANDLE of a shader constant

// entry points of VK_EXT_shader_object, VK_EXT_extended_dynamic_state3 and VK_EXT_vertex_input_dynamic_state
struct VkExtFunctions {
	PFN_vkCreateShadersEXT CreateShadersEXT;
	PFN_vkDestroyShaderEXT DestroyShaderEXT;
	PFN_vkCmdBindShadersEXT CmdBindShadersEXT;
	PFN_vkCmdSetVertexInputEXT CmdSetVertexInputEXT;
	PFN_vkCmdSetPolygonModeEXT CmdSetPolygonModeEXT;
	PFN_vkCmdSetRasterizationSamplesEXT CmdSetRasterizationSamplesEXT;
	PFN_vkCmdSetSampleMaskEXT CmdSetSampleMaskEXT;
	PFN_vkCmdSetAlphaToCoverageEnableEXT CmdSetAlphaToCoverageEnableEXT;
	PFN_vkCmdSetColorBlendEnableEXT CmdSetColorBlendEnableEXT;
	PFN_vkCmdSetColorBlendEquationEXT CmdSetColorBlendEquationEXT;
	PFN_vkCmdSetColorWriteMaskEXT CmdSetColorWriteMaskEXT;
	PFN_vkCmdSetDepthClampEnableEXT CmdSetDepthClampEnableEXT;
	PFN_vkCmdSetSampleLocationsEnableEXT CmdSetSampleLocationsEnableEXT; // VK_EXT_sample_locations, when the device has it
	PFN_vkCmdSetSampleLocationsEXT CmdSetSampleLocationsEXT;
};
extern VkExtFunctions vkx;

// Buffer (IDirect3DVertexBuffer9, IDirect3DIndexBuffer9): host visible and persistently mapped when dynamic

class VkBuf {
public:
	VkBuf (VkDev *dev, VkDeviceSize size, VkBufferUsageFlags usage, bool host, bool readback = false);
	~VkBuf ();
	void *Map () const { return mapped; } // Lock; NULL for device-local buffers
	void Upload (const void *data, VkDeviceSize size, VkDeviceSize offset = 0); // UpdateSubresource path
	VkBuffer buf;
	VkDeviceSize size;
private:
	VkDev *dev;
	VmaAllocation alloc;
	void *mapped;
};

// Vertex declaration (IDirect3DVertexDeclaration9): the input layout set with vkCmdSetVertexInputEXT

enum VkDeclUsage { DECL_POSITION, DECL_NORMAL, DECL_TANGENT, DECL_TEXCOORD, DECL_COLOR };

struct VkDeclElement {
	WORD stream;          // vertex buffer binding
	WORD offset;          // byte offset in the vertex
	VkFormat format;      // D3DDECLTYPE counterpart (D3DCOLOR elements are VK_FORMAT_B8G8R8A8_UNORM)
	VkDeclUsage usage;    // semantic
	BYTE index;           // semantic index
};

class VkVertexDecl {
public:
	VkVertexDecl (const VkDeclElement *elem, int n, UINT stride0 = 0, UINT stride1 = 0);
	std::vector<VkVertexInputBindingDescription2EXT> bind;
	std::vector<VkVertexInputAttributeDescription2EXT> attr;
	static UINT Location (VkDeclUsage usage, BYTE index); // shader input location of a semantic
};

// Texture or surface (IDirect3DTexture9, IDirect3DCubeTexture9, IDirect3DSurface9 render targets and depth buffers)

class VkTex {
public:
	VkTex (VkDev *dev, UINT w, UINT h, UINT levels, VkFormat fmt, VkImageUsageFlags usage,
		UINT layers = 1, bool cube = false, VkSampleCountFlagBits samples = VK_SAMPLE_COUNT_1_BIT);
	~VkTex ();

	void Transition (VkCommandBuffer cmd, VkImageLayout layout); // records a barrier to the new layout
	void Upload (UINT level, UINT layer, const void *data, VkDeviceSize size, UINT rowPitch = 0); // whole mip level
	void GenerateMips (VkCommandBuffer cmd); // D3DUSAGE_AUTOGENMIPMAP counterpart
	void Written (UINT level) { if (!level && autoGenMips && levels > 1) mipsDirty = true; } // level 0 changed: sublevels out of date
	VkImageAspectFlags Aspect () const;
	bool IsDepth () const;
	void SetSwizzle (VkComponentMapping swz);   // view channel mapping (X8, L8, A8 and A8L8 formats)

	VkImage img;
	VkImageView view;
	VkFormat fmt;
	UINT w, h, levels, layers;
	bool cube;
	VkSampleCountFlagBits samples;
	VkImageUsageFlags usage;
	VkImageLayout layout;
	VkComponentMapping swizzle; // identity unless SetSwizzle
	bool external;         // swapchain image: not owned
	bool autoGenMips = false; // D3DUSAGE_AUTOGENMIPMAP: the sublevels follow level 0
	bool mipsDirty = false;   // level 0 changed since the sublevels were made

	VkTex (VkDev *dev, VkImage image, VkFormat fmt, UINT w, UINT h); // wraps an image owned elsewhere
	VkTex (VkDev *dev, VkFormat fmt, UINT w, UINT h, UINT d, VkImageUsageFlags usage); // volume texture, one level
	UINT depth;            // 1 unless a volume texture
	VkDev *Device () const { return dev; }

	// ImGui descriptor set of this texture (ImTextureID), made by the client and kept while the texture lives
	uint64_t uiSet = 0;
	VkImageView uiView = VK_NULL_HANDLE; // the view uiSet was made with
	DWORD uiGen = 0;                     // ImGui backend generation uiSet belongs to
	static void (*uiRelease) (uint64_t set, DWORD gen);

private:
	VkDev *dev;
	VmaAllocation alloc;
};

UINT VkFormatBlockSize (VkFormat fmt, UINT *blockW = NULL); // bytes per texel (or per 4x4 block for BCn)

// Surface (IDirect3DSurface9): a mip level/face of a texture, or a standalone target that owns its image
class VkSurf {
public:
	VkSurf (VkTex *tex, UINT level = 0, UINT layer = 0);   // GetSurfaceLevel / GetCubeMapSurface
	VkSurf (VkDev *dev, UINT w, UINT h, VkFormat fmt, VkImageUsageFlags usage,
		VkSampleCountFlagBits samples = VK_SAMPLE_COUNT_1_BIT); // CreateRenderTarget / CreateDepthStencilSurface
	~VkSurf ();
	VkTex *tex;
	UINT level, layer;
	UINT w, h;
	VkImageView view;      // single level and layer, for rendering
private:
	void MakeView ();
	VkDev *dev;
	bool owner;
};

// Sampler state (D3DSAMP_* counterpart), cached by value

struct VkSamplerDesc {
	VkFilter mag, min;
	VkSamplerMipmapMode mip;
	VkSamplerAddressMode u, v, w;
	float aniso;          // 0 = off
	float mipBias;
	bool noMip;           // D3DTEXF_NONE mip filter: base level only
	bool operator== (const VkSamplerDesc &s) const;
};

// Device (IDirect3DDevice9): queue, frames in flight, render state mirror
class VkDev {
public:
	static const int NFRAMES = 2;

	VkDev (QVulkanInstance *inst, VkPhysicalDevice phys);
	~VkDev ();
	bool IsOK () const { return dev != VK_NULL_HANDLE; }
	bool IsRecording () const { return recording; }

	// frame
	VkCommandBuffer BeginFrame ();                   // waits for the frame slot, starts its command buffer
	void EndFrame (VkSemaphore wait, VkSemaphore signal); // submits (wait/signal: swapchain semaphores or null)
	VkCommandBuffer Cmd () const { return frame[iFrame].cmd; }
	void Flush ();                                   // submit the recorded commands and wait (uploads outside a frame)
	void WaitIdle ();
	VkResult QueuePresent (const VkPresentInfoKHR *pi); // vkQueuePresentKHR under the queue lock
	std::mutex &QueueLock () { return queueLock; }  // for code that submits to the queue itself (ImGui backend)
	void Defer (std::function<void()> release);      // SAFE_RELEASE: freed once the GPU is done with this frame

	// one-off commands (uploads, readbacks) outside the frame command buffer
	VkCommandBuffer BeginOneTime ();
	void EndOneTime (VkCommandBuffer cmd);

	// per-frame transient memory (DrawPrimitiveUP data, effect constants)
	VkDeviceSize AllocTransient (VkDeviceSize size, VkDeviceSize align, void **ptr); // offset into TransientBuffer()
	VkBuffer TransientBuffer () const;

	// render targets: dynamic rendering, begun lazily at the first draw or clear
	void SetRenderTarget (VkSurf *color, VkSurf *depth); // SetRenderTarget(0,..) + SetDepthStencilSurface
	VkSurf *GetRenderTarget () const { return rtColor; }
	void SetRenderTargetN (UINT idx, VkSurf *color);  // SetRenderTarget(idx, s): 0 = SetRenderTarget(color, current depth), 1-3 extra MRT colour targets
	VkSurf *GetRenderTargetN (UINT idx) const { return idx == 0 ? rtColor : (idx < 4 ? rtExtra[idx - 1] : NULL); }
	void ForgetTarget (const VkSurf *s);             // a target surface is deleted: unbind it (a new VkSurf may reuse the address)
	void ForgetTexture (const VkTex *t);             // a texture is deleted: drop it from the effects' sampler bindings (D3D9 effects held a reference)
	void RegisterConstants (VkConstBuffer *cb, bool add); // the constant buffers ForgetTexture visits
	std::mutex &ConstLock () { return constLock; }   // guards the constant buffers' texture bindings (textures die on loader threads too)
	VkSurf *GetDepthStencil () const { return rtDepth; }
	void BeginRendering ();                          // no-op while rendering to the same targets
	void EndRendering ();
	bool IsRendering () const { return rendering; }
	void Clear (bool color, bool depth, bool stencil, DWORD argb, float z, DWORD s, const RECT *r = NULL);
	void SetViewport (float x, float y, float w, float h, float minz = 0.0f, float maxz = 1.0f);
	VkViewport GetViewport () const { return { viewport.x, viewport.y + viewport.height, viewport.width, -viewport.height, viewport.minDepth, viewport.maxDepth }; } // as passed to SetViewport
	void SetScissor (const RECT *r);                 // NULL = whole target (D3DRS_SCISSORTESTENABLE off)

	// dynamic state with D3D9 persistence (replayed into each new command buffer)
	void SetDepthTest (bool enable);                 // D3DRS_ZENABLE
	void SetDepthWrite (bool enable);                // D3DRS_ZWRITEENABLE
	void SetDepthFunc (VkCompareOp op);              // D3DRS_ZFUNC
	void SetCullMode (VkCullModeFlags mode);         // D3DRS_CULLMODE (D3D9 culls counter-clockwise faces by default)
	void SetFillMode (VkPolygonMode mode);           // D3DRS_FILLMODE
	void SetBlend (bool enable);                     // D3DRS_ALPHABLENDENABLE
	void SetBlendFunc (VkBlendFactor src, VkBlendFactor dst, VkBlendOp op = VK_BLEND_OP_ADD); // D3DRS_SRCBLEND/DESTBLEND/BLENDOP
	void SetBlendFuncAlpha (VkBlendFactor src, VkBlendFactor dst, VkBlendOp op = VK_BLEND_OP_ADD); // D3DRS_SEPARATEALPHABLENDENABLE
	void SetSrcBlend (VkBlendFactor src);            // D3DRS_SRCBLEND alone
	void SetDestBlend (VkBlendFactor dst);           // D3DRS_DESTBLEND alone
	void SetColorWrite (VkColorComponentFlags mask); // D3DRS_COLORWRITEENABLE
	void SetStencil (bool enable, VkCompareOp op, UINT ref, UINT mask, VkStencilOp pass, VkStencilOp fail, VkStencilOp zfail); // D3DRS_STENCIL*
	void SetDepthBias (float constant, float slope);  // D3DRS_DEPTHBIAS/SLOPESCALEDEPTHBIAS
	void SetTopology (VkPrimitiveTopology t);
	void SetVertexDecl (const VkVertexDecl *decl);    // SetVertexDeclaration
	void SetStreamSource (UINT stream, VkBuffer vb, VkDeviceSize offset, UINT stride); // stride as D3D9 takes it here
	void SetStreamSource (UINT stream, const VkBuf *vb, VkDeviceSize offset, UINT stride) { SetStreamSource (stream, vb ? vb->buf : VK_NULL_HANDLE, offset, stride); }
	void SetIndices (VkBuffer ib, VkDeviceSize offset = 0, VkIndexType type = VK_INDEX_TYPE_UINT16);
	void SetIndices (const VkBuf *ib, VkIndexType type = VK_INDEX_TYPE_UINT16) { SetIndices (ib ? ib->buf : VK_NULL_HANDLE, 0, type); }

	// shaders and draws (SetVertexShader/SetPixelShader, Draw*Primitive*); counts are vertices/indices, not primitives
	void BindShaders (VkShaderEXT vs, VkShaderEXT fs); // kept across frames, as SetVertexShader/SetPixelShader
	void SetConstantSource (VkConstBuffer *cb, const std::vector<VkSamplerSlot> *smpSlots); // pushed before a draw when changed
	VkConstBuffer *GetConstantSource () const { return cbActive; }
	void DrawPrimitive (VkPrimitiveTopology t, UINT startVertex, UINT vertexCount);
	void DrawIndexedPrimitive (VkPrimitiveTopology t, int baseVertex, UINT startIndex, UINT indexCount);
	void DrawPrimitiveUP (VkPrimitiveTopology t, UINT vertexCount, const void *vtx, UINT stride);
	void DrawIndexedPrimitiveUP (VkPrimitiveTopology t, UINT vertexCount, UINT indexCount, const void *idx, VkIndexType it, const void *vtx, UINT stride);
	void PrepareSample (VkTex *tex);                 // a texture read by the next draw must be in SHADER_READ_ONLY layout
	VkTex *DefaultTexture (VkImageViewType type);    // 1x1 black, read by samplers with no texture set

	// surface operations outside draws (StretchRect, UpdateSurface/GetRenderTargetData, ColorFill)
	void StretchRect (VkSurf *src, const RECT *sr, VkSurf *dst, const RECT *dr, VkFilter filter);
	void CopySurface (VkSurf *src, const RECT *sr, VkSurf *dst, const POINT *dp);
	void ColorFill (VkSurf *s, const RECT *r, DWORD argb);

	struct State {
		bool depthTest, depthWrite, blend, blendSeparate, stencil;
		VkCompareOp depthFunc;
		VkCullModeFlags cull;
		VkPolygonMode fill;
		VkColorBlendEquationEXT blendEq;
		VkColorComponentFlags writeMask;
		VkCompareOp stencilOp;
		UINT stencilRef, stencilMask;
		VkStencilOp stencilPass, stencilFail, stencilZFail;
		float biasConst, biasSlope;
		VkPrimitiveTopology topology;
		const VkVertexDecl *decl;
		bool msaa;                                     // D3DRS_MULTISAMPLEANTIALIAS
	};
	void SetMultisampleAA (bool enable);             // D3DRS_MULTISAMPLEANTIALIAS: off gives every sample the pixel centre's coverage
	bool GetMultisampleAA () const { return st.msaa; }
	bool sampleLocations;                            // VK_EXT_sample_locations with dynamic enable: SetMultisampleAA(false) takes effect
	State GetState () const { return st; }           // RenderState::Capture (GetRenderState)
	void SetState (const State &s);                  // RenderState::Restore (SetRenderState)
	bool GetScissor (RECT *r) const;                 // GetScissorRect; false if the scissor test is off

	// descriptors: set 0 is pushed per draw (textures and constant blocks of the bound effect)
	VkSampler Sampler (const VkSamplerDesc &desc);
	VkDescriptorSetLayout SetLayout () const { return setLayout; }
	VkPipelineLayout PipelineLayout () const { return pipeLayout; }

	// identity
	VkInstance instance;
	VkPhysicalDevice phys;
	VkDevice dev;
	VkQueue queue;
	UINT queueFamily;
	VmaAllocator vma;
	VkPhysicalDeviceProperties props;
	VkPhysicalDeviceFeatures features;
	QVulkanInstance *qinst;

	// set 0 layout shared by all shaders: bindings 0-3 constant blocks, 4-31 textures; 128 bytes of push constants
	static const int NUBOS = 4;
	static const int MAXBINDINGS = 32;   // maxPushDescriptors is at least 32
	static const int PUSHCONST = 128;

private:
	void CreateDevice ();
	void ReplayState ();
	void ApplySampleLocations ();
	VkSampleCountFlags sampleLocationCounts;
	void ReleaseFrame (int i);
	void DrawCopy (VkTex *src, VkSurf *dst, const RECT &d); // StretchRect into a multisampled target
	VkShaderEXT copyVS, copyFS;

	struct Frame {
		VkCommandPool pool;
		VkCommandBuffer cmd;
		uint64_t done;                               // timeline value signalled when this frame's work is complete
		std::vector<std::function<void()>> release;
		VkBuf *transient;
		VkDeviceSize transientUsed;
	} frame[NFRAMES];
	int iFrame;
	bool recording;
	VkSemaphore timeline;
	uint64_t timelineValue;
	VkCommandPool oneTimePool;
	std::recursive_mutex oneTimeLock;                // held from BeginOneTime to EndOneTime: the pool is externally synchronized
	std::mutex constLock;
	std::vector<VkConstBuffer*> constBufs;
	std::mutex queueLock;                            // D3DCREATE_MULTITHREADED: loader threads upload textures

	VkSurf *rtColor, *rtDepth;
	VkSurf *rtExtra[3];                                  // MRT colour targets 1-3 (a list ending at the first NULL)
	UINT streamStride[2];
	bool rendering;
	VkViewport viewport;
	VkRect2D scissor;
	bool scissorSet;

	State st;

	void PreDraw ();

	VkShaderEXT curVS, curFS;
	VkConstBuffer *cbActive;
	const std::vector<VkSamplerSlot> *cbSlots;
	std::vector<std::pair<VkSamplerDesc, VkSampler>> samplers;
	VkTex *defTex[3];                    // 2D, cube, 3D
	VkDescriptorSetLayout setLayout;
	VkPipelineLayout pipeLayout;
};

// D3DPRIMITIVETYPE primitive count → the vertex or index count a Vk draw takes
inline UINT VkPrimVerts (VkPrimitiveTopology t, UINT prims)
{
	switch (t) {
	case VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST: return prims * 3;
	case VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP: case VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN: return prims ? prims + 2 : 0;
	case VK_PRIMITIVE_TOPOLOGY_LINE_LIST: return prims * 2;
	case VK_PRIMITIVE_TOPOLOGY_LINE_STRIP: return prims ? prims + 1 : 0;
	default: return prims;
	}
}

#define VKCHECK(x) { VkResult _r = (x); if (_r < 0) LogErr("%s Line:%d VkResult:%d %s", __FILE__, __LINE__, (int)_r, #x); }

#endif // !__VKCORE_H
