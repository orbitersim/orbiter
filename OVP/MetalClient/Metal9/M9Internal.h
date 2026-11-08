#pragma once

#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "d3dx9.h"
#include <vector>
#include <string>
#include <unordered_map>
#include <map>
#include <memory>
#include <functional>
#include <dispatch/dispatch.h>
#include <pthread.h>

namespace m9 {

void Log(const char *fmt, ...);

// ---------------------------------------------------------------------------
// Formats

enum FormatKind { FK_PLAIN, FK_COMPRESSED, FK_DEPTH, FK_CONVERTED };

struct FormatInfo {
	D3DFORMAT d3d;
	MTLPixelFormat pf;
	FormatKind kind;
	UINT bytes;          // bytes per pixel (plain/converted, as seen by the app) or per 4x4 block (compressed)
	UINT gpuBytes;       // bytes per pixel in the Metal texture (converted formats)
	MTLTextureSwizzleChannels swizzle;
	bool hasSwizzle;
	bool hasStencil;
};

const FormatInfo *GetFormat(D3DFORMAT fmt);
UINT RowPitch(const FormatInfo *fi, UINT width);
UINT RowCount(const FormatInfo *fi, UINT height);
void ConvertToGpu(const FormatInfo *fi, const uint8_t *src, UINT srcPitch, uint8_t *dst, UINT dstPitch, UINT w, UINT h);
void ConvertFromGpu(const FormatInfo *fi, const uint8_t *src, UINT srcPitch, uint8_t *dst, UINT dstPitch, UINT w, UINT h);

// ---------------------------------------------------------------------------
// Resources

struct Device;
struct Surface;

enum TexType { TT_2D, TT_CUBE, TT_3D };

struct LockState {
	bool active = false;
	DWORD flags = 0;
	RECT rect = {0, 0, 0, 0};
	std::vector<uint8_t> data;
	UINT pitch = 0;
	UINT slicePitch = 0;
};

struct Texture {
	Device *dev = nullptr;
	IUnknownM9 *iface = nullptr;      // owning interface (texture, cube, volume or standalone surface)
	TexType type = TT_2D;
	D3DFORMAT fmt = D3DFMT_UNKNOWN;
	const FormatInfo *fi = nullptr;
	UINT width = 0, height = 0, depth = 1, levels = 1;
	DWORD usage = 0;
	D3DPOOL pool = D3DPOOL_DEFAULT;
	UINT samples = 1;
	id<MTLTexture> tex = nil;          // GPU storage (nil for system memory resources)
	id<MTLTexture> msaa = nil;         // multisample render target, resolved into tex
	id<MTLTexture> view = nil;         // swizzled view for sampling render targets
	std::vector<std::vector<uint8_t>> sysmem;   // system memory pool storage, per subresource
	std::vector<LockState> locks;      // per subresource
	std::vector<IDirect3DSurface9 *> surfaces; // per subresource (face * levels + level)
	bool isBackBuffer = false;
	bool msaaDirty = false;

	UINT Faces() const { return type == TT_CUBE ? 6 : 1; }
	UINT Sub(UINT face, UINT level) const { return face * levels + level; }
	UINT LevelWidth(UINT level) const { UINT v = width >> level; return v ? v : 1; }
	UINT LevelHeight(UINT level) const { UINT v = height >> level; return v ? v : 1; }
	UINT LevelDepth(UINT level) const { UINT v = depth >> level; return v ? v : 1; }
	bool IsSystem() const { return pool == D3DPOOL_SYSTEMMEM || pool == D3DPOOL_SCRATCH; }
	bool IsDepth() const { return fi && fi->kind == FK_DEPTH; }

	HRESULT Lock(UINT face, UINT level, D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags);
	HRESULT Unlock(UINT face, UINT level);
	HRESULT LockBox(UINT level, D3DLOCKED_BOX *lb, const D3DBOX *box, DWORD flags);
	HRESULT UnlockBox(UINT level);
	IDirect3DSurface9 *GetSurface(UINT face, UINT level);

	// Copy pixel data for one subresource (app-visible format, tightly pitched rows) to/from the texture
	void Upload(UINT face, UINT level, const uint8_t *data, UINT pitch, const RECT *rect, UINT slice = 0, UINT slices = 1);
	void Download(UINT face, UINT level, uint8_t *data, UINT pitch, const RECT *rect);
	id<MTLTexture> SampleTexture();     // texture to bind for sampling (resolves MSAA)
};

struct Surface {
	Texture *tex = nullptr;
	UINT face = 0, level = 0;
	bool owner = false;                // standalone surface owning its texture
	IDirect3DSurface9 *iface = nullptr;
};

struct Buffer {
	Device *dev = nullptr;
	id<MTLBuffer> buf = nil;
	UINT size = 0;
	DWORD usage = 0;
	D3DPOOL pool = D3DPOOL_DEFAULT;
	D3DFORMAT fmt = D3DFMT_UNKNOWN;
	DWORD fvf = 0;
	uint64_t lastUse = 0;              // command buffer serial that last referenced this buffer
	bool locked = false;
};

struct Decl {
	std::vector<D3DVERTEXELEMENT9> elems;
	uint64_t hash = 0;
};

// Attribute indices used by shaders for each vertex element usage
int AttributeIndex(BYTE usage, BYTE usageIndex);

// ---------------------------------------------------------------------------
// Shader programs

enum ParamType { PT_FLOAT, PT_INT, PT_BOOL, PT_STRUCT };

struct Layout;

struct Member {
	std::string name;
	UINT offset = 0;
	UINT size = 0;          // total size in bytes
	UINT rows = 1, cols = 1;  // vectors: rows = 1, cols = components; matrices: rows x cols as in floatRxC... (MSL floatCxR)
	UINT elements = 0;      // array length (0 = not an array)
	UINT stride = 0;        // array element stride
	UINT packed = 0;        // tightly packed size of one element, as D3DX callers lay it out
	ParamType type = PT_FLOAT;
	bool matrix = false;
	std::shared_ptr<Layout> sub; // struct member layout
};

struct Layout {
	std::string structName;
	std::vector<Member> members;
	UINT size = 0;
	UINT align = 4;
	UINT packed = 0;
	const Member *Find(const char *name) const;
};

struct ShaderSource {
	std::string path;
	std::string text;                             // preprocessed source
	std::map<std::string, Layout> structs;        // parsed constant structs
	std::map<std::string, int> textureSlots;      // texture name -> slot
	std::map<std::string, std::string> funcConst; // function -> constant struct type bound at buffer(0)
	std::map<std::string, int> textureTypes;      // texture name -> 2 (2D), 3 (3D), 6 (cube)
};

bool LoadShaderSource(const std::string &path, ShaderSource &src, const D3DXMACRO *defines = nullptr);
id<MTLLibrary> CompileLibrary(id<MTLDevice> dev, const ShaderSource &src, const D3DXMACRO *defines, std::string &errors);
void WriteParam(const Member *m, uint8_t *data, const void *value, UINT bytes, ParamType srcType, UINT count, bool srcMatrix);

// A compiled vertex or pixel function with its constant buffer
struct Program {
	id<MTLFunction> fn = nil;
	Layout constants;
	std::vector<uint8_t> data;
	std::map<std::string, int> textureSlots;
	bool vertex = false;
	std::string name;
};

struct Shader {
	std::shared_ptr<Program> prog;
};

struct ConstantTable {
	std::shared_ptr<Program> prog;
};

// Fixed render states applied by an effect pass
struct PassState {
	D3DRENDERSTATETYPE state;
	DWORD value;
};

struct Pass {
	std::string name;
	std::string vsName, psName;
	id<MTLFunction> vs = nil, ps = nil;
	std::vector<PassState> states;
};

struct Technique {
	std::string name;
	std::vector<Pass> passes;
};

struct EffectParam {
	std::string name;
	const Member *member = nullptr;     // constant data, or nullptr for textures
	int textureSlot = -1;
	IDirect3DBaseTexture9 *texture = nullptr;
};

struct Effect {
	Device *dev = nullptr;
	std::string path;
	id<MTLLibrary> lib = nil;
	ShaderSource src;
	Layout globals;
	std::vector<uint8_t> data;
	std::vector<std::unique_ptr<EffectParam>> params;
	std::vector<std::unique_ptr<Technique>> techniques;
	Technique *current = nullptr;
	int activePass = -1;
	EffectParam *FindParam(D3DXHANDLE h);
	Technique *FindTechnique(D3DXHANDLE h);
	void ApplyPass(int pass);
	~Effect();
};

// ---------------------------------------------------------------------------
// Device

struct SwapChain {
	Device *dev = nullptr;
	HWND hWnd = nullptr;
	CAMetalLayer *layer = nil;
	D3DPRESENT_PARAMETERS pp = {};
	IDirect3DSurface9 *backBuffer = nullptr;
	IDirect3DSurface9 *depthStencil = nullptr;
	bool Create();
	void Destroy();
	HRESULT Present();
};

struct Query {
	D3DQUERYTYPE type;
	uint64_t serial = 0;
	bool issued = false;
	UINT visOffset = 0;
	uint64_t visResult = 0;
};

struct StreamSource {
	IDirect3DVertexBuffer9 *vb = nullptr;
	UINT offset = 0;
	UINT stride = 0;
};

struct UploadChunk {
	id<MTLBuffer> buf = nil;
	UINT used = 0;
	uint64_t serial = 0;
};

struct Device {
	IDirect3DDevice9 *iface = nullptr;
	IDirect3D9 *d3d = nullptr;
	id<MTLDevice> mtl = nil;
	id<MTLCommandQueue> queue = nil;
	id<MTLCommandBuffer> cmd = nil;
	id<MTLRenderCommandEncoder> renc = nil;
	id<MTLBlitCommandEncoder> benc = nil;
	uint64_t serial = 1;                   // serial of the command buffer being recorded
	uint64_t completed = 0;                // last completed serial
	dispatch_semaphore_t inflight;
	std::vector<SwapChain *> chains;
	D3DPRESENT_PARAMETERS pp = {};
	DWORD behaviour = 0;

	DWORD rs[D3DRS_COUNT] = {};
	DWORD ss[20][D3DSAMP_COUNT] = {};
	IDirect3DBaseTexture9 *textures[20] = {};
	StreamSource streams[8];
	IDirect3DIndexBuffer9 *indices = nullptr;
	IDirect3DVertexDeclaration9 *decl = nullptr;
	IDirect3DVertexShader9 *vs = nullptr;
	IDirect3DPixelShader9 *ps = nullptr;
	IDirect3DSurface9 *rt[4] = {};
	IDirect3DSurface9 *ds = nullptr;
	D3DVIEWPORT9 viewport = {};
	RECT scissor = {0, 0, 0, 0};

	Effect *effect = nullptr;              // effect pass providing the shaders, if any
	int effectPass = -1;

	// render pass bookkeeping
	bool passDirty = true;
	bool pendingClear = false;
	DWORD clearFlags = 0;
	D3DCOLOR clearColor = 0;
	float clearZ = 1.0f;
	DWORD clearStencil = 0;
	id<MTLBuffer> visBuffer = nil;
	UINT visNext = 0;

	struct PipelineBinding { uint32_t index; MTLTextureType texType; bool depth; uint32_t size; };
	struct PipelineEntry {
		id<MTLRenderPipelineState> pso = nil;
		std::vector<PipelineBinding> vtex, ftex, vsamp, fsamp, vbuf, fbuf;
	};
	std::unordered_map<uint64_t, PipelineEntry> pipelines;
	id<MTLTexture> dummy2D = nil, dummyCube = nil, dummy3D = nil, dummyDepth = nil;
	id<MTLTexture> Dummy(MTLTextureType type, bool depth);
	std::unordered_map<uint64_t, id<MTLDepthStencilState>> dsStates;
	std::unordered_map<uint64_t, id<MTLSamplerState>> samplers;
	std::vector<UploadChunk> chunks;
	id<MTLBuffer> zeroBuffer = nil;
	id<MTLRenderPipelineState> clearPipeline[4] = {};

	Device();
	~Device();
	bool Init(HWND hWnd, D3DPRESENT_PARAMETERS *pp);
	void SetDefaults();

	id<MTLCommandBuffer> Cmd();
	id<MTLRenderCommandEncoder> RenderEncoder();
	id<MTLBlitCommandEncoder> BlitEncoder();
	// Encode a blit; from threads other than the device thread it runs in its own command buffer
	void EncodeBlit(void (^fn)(id<MTLBlitCommandEncoder>), bool wait = false);
	bool OnDeviceThread() const { return pthread_equal(pthread_self(), owner); }
	pthread_t owner = {};
	void EndEncoders();
	void Submit(bool wait);
	void WaitIdle();
	void FrameEnd();
	bool InFlight(uint64_t s) const { return s > completed; }

	// per-frame transient GPU memory
	id<MTLBuffer> Upload(const void *data, UINT size, UINT &offset, UINT align = 256);

	HRESULT Draw(D3DPRIMITIVETYPE type, UINT vertexStart, UINT vertexCount, const void *upVerts, UINT upStride,
		IDirect3DIndexBuffer9 *ib, const void *upIndices, D3DFORMAT upIndexFmt, UINT indexStart, UINT indexCount, INT baseVertex);
	bool BindState(id<MTLRenderCommandEncoder> enc, const void *upVerts, UINT upStride, UINT upCount);
	const PipelineEntry *Pipeline(id<MTLFunction> vfn, id<MTLFunction> ffn, const Decl *decl, const UINT *strides);
	id<MTLDepthStencilState> DepthStencil();
	id<MTLSamplerState> Sampler(UINT idx);
	void ClearInPass(DWORD flags, D3DCOLOR color, float z, DWORD stencil, const D3DRECT *rect);
	MTLRenderPassDescriptor *PassDescriptor();

	void Blit(Surface *src, const RECT *srcRect, Surface *dst, const RECT *dstRect, bool linear);
	id<MTLRenderPipelineState> BlitPipeline(MTLPixelFormat fmt, bool depth, NSUInteger samples = 1);
	void RefreshMSAA(Texture *t);
	id<MTLRenderPipelineState> OverlayPipeline(NSString *fragment);
	id<MTLDepthStencilState> NoDepth();
	id<MTLDepthStencilState> noDepth = nil;
	std::map<std::string, int> drawLog;
	bool drawLogActive = false;
	std::unordered_map<uint64_t, id<MTLRenderPipelineState>> blitPipelines;
	id<MTLLibrary> utilLib = nil;
	id<MTLSamplerState> blitLinear = nil, blitPoint = nil;
};

Device *CurrentDevice();

template<class T> void Rebind(T *&slot, T *value)
{
	if (slot == value) return;
	if (value) value->BindRef();
	T *old = slot;
	slot = value;
	if (old) old->BindRelease();
}

Texture *CreateTexture(Device *dev, TexType type, UINT w, UINT h, UINT d, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, UINT samples);
void DestroyTexture(Texture *t);

struct Font;

}

inline m9::Surface *SurfImpl(IDirect3DSurface9 *s) { return s ? s->Impl() : nullptr; }
