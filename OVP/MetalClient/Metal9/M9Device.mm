#include "M9Internal.h"
#include "OrbiterPlatform.h"
#import <CoreGraphics/CoreGraphics.h>
#include <algorithm>
#include <atomic>

namespace m9 {

static Device *g_device = nullptr;
Device *CurrentDevice() { return g_device; }

static const char *kUtilSource = R"(
#include <metal_stdlib>
using namespace metal;

struct BlitOut { float4 pos [[position]]; float2 uv; };

struct BlitParams { float4 src; float4 dst; float depth; };

vertex BlitOut BlitVS(uint vid [[vertex_id]], constant BlitParams &p [[buffer(0)]])
{
	float2 c = float2((vid << 1) & 2, vid & 2) * 0.5;
	BlitOut o;
	float2 d = mix(p.dst.xy, p.dst.zw, c);
	o.pos = float4(d.x * 2.0 - 1.0, 1.0 - d.y * 2.0, p.depth, 1.0);
	o.uv = mix(p.src.xy, p.src.zw, c);
	return o;
}

vertex BlitOut BlitVSQuad(uint vid [[vertex_id]], constant BlitParams &p [[buffer(0)]])
{
	float2 c = float2(vid & 1, (vid >> 1) & 1);
	BlitOut o;
	float2 d = mix(p.dst.xy, p.dst.zw, c);
	o.pos = float4(d.x * 2.0 - 1.0, 1.0 - d.y * 2.0, p.depth, 1.0);
	o.uv = mix(p.src.xy, p.src.zw, c);
	return o;
}

fragment float4 BlitPS(BlitOut in [[stage_in]], texture2d<float> t [[texture(0)]], sampler s [[sampler(0)]])
{
	return t.sample(s, in.uv);
}

struct DepthOut { float depth [[depth(any)]]; };

fragment DepthOut BlitDepthPS(BlitOut in [[stage_in]], depth2d<float> t [[texture(0)]], sampler s [[sampler(0)]])
{
	DepthOut o;
	o.depth = t.sample(s, in.uv);
	return o;
}

struct ClearColor { float4 color; };
struct Clear1 { float4 c0 [[color(0)]]; };
struct Clear2 { float4 c0 [[color(0)]]; float4 c1 [[color(1)]]; };
struct Clear3 { float4 c0 [[color(0)]]; float4 c1 [[color(1)]]; float4 c2 [[color(2)]]; };
struct Clear4 { float4 c0 [[color(0)]]; float4 c1 [[color(1)]]; float4 c2 [[color(2)]]; float4 c3 [[color(3)]]; };

struct TextColor { float4 color; };

fragment float4 TextPS(BlitOut in [[stage_in]], texture2d<float> t [[texture(0)]], sampler s [[sampler(0)]], constant TextColor &c [[buffer(0)]])
{
	float a = t.sample(s, in.uv).r * c.color.a;
	return float4(c.color.rgb * a, a);
}

fragment void ClearPS0() {}
fragment Clear1 ClearPS1(constant ClearColor &c [[buffer(0)]]) { Clear1 o; o.c0 = c.color; return o; }
fragment Clear2 ClearPS2(constant ClearColor &c [[buffer(0)]]) { Clear2 o; o.c0 = o.c1 = c.color; return o; }
fragment Clear3 ClearPS3(constant ClearColor &c [[buffer(0)]]) { Clear3 o; o.c0 = o.c1 = o.c2 = c.color; return o; }
fragment Clear4 ClearPS4(constant ClearColor &c [[buffer(0)]]) { Clear4 o; o.c0 = o.c1 = o.c2 = o.c3 = c.color; return o; }
)";

// ---------------------------------------------------------------------------
// Device lifecycle

Device::Device()
{
	inflight = dispatch_semaphore_create(3);
	g_device = this;
}

Device::~Device()
{
	if (cmd) Submit(true);
	for (auto &t : textures) Rebind(t, (IDirect3DBaseTexture9 *)nullptr);
	for (auto &s : streams) Rebind(s.vb, (IDirect3DVertexBuffer9 *)nullptr);
	Rebind(indices, (IDirect3DIndexBuffer9 *)nullptr);
	Rebind(decl, (IDirect3DVertexDeclaration9 *)nullptr);
	Rebind(vs, (IDirect3DVertexShader9 *)nullptr);
	Rebind(ps, (IDirect3DPixelShader9 *)nullptr);
	for (int i = 0; i < 4; i++) {
		if (rt[i]) rt[i]->Release();
		rt[i] = nullptr;
	}
	if (ds) ds->Release();
	ds = nullptr;
	for (auto *c : chains) {
		c->Destroy();
		delete c;
	}
	chains.clear();
	if (g_device == this) g_device = nullptr;
}

void Device::SetDefaults()
{
	memset(rs, 0, sizeof(rs));
	rs[D3DRS_ZENABLE] = D3DZB_TRUE;
	rs[D3DRS_FILLMODE] = D3DFILL_SOLID;
	rs[D3DRS_SHADEMODE] = D3DSHADE_GOURAUD;
	rs[D3DRS_ZWRITEENABLE] = TRUE;
	rs[D3DRS_ALPHATESTENABLE] = FALSE;
	rs[D3DRS_LASTPIXEL] = TRUE;
	rs[D3DRS_SRCBLEND] = D3DBLEND_ONE;
	rs[D3DRS_DESTBLEND] = D3DBLEND_ZERO;
	rs[D3DRS_CULLMODE] = D3DCULL_CCW;
	rs[D3DRS_ZFUNC] = D3DCMP_LESSEQUAL;
	rs[D3DRS_ALPHAFUNC] = D3DCMP_ALWAYS;
	rs[D3DRS_ALPHABLENDENABLE] = FALSE;
	rs[D3DRS_STENCILENABLE] = FALSE;
	rs[D3DRS_STENCILFAIL] = D3DSTENCILOP_KEEP;
	rs[D3DRS_STENCILZFAIL] = D3DSTENCILOP_KEEP;
	rs[D3DRS_STENCILPASS] = D3DSTENCILOP_KEEP;
	rs[D3DRS_STENCILFUNC] = D3DCMP_ALWAYS;
	rs[D3DRS_STENCILREF] = 0;
	rs[D3DRS_STENCILMASK] = 0xFFFFFFFF;
	rs[D3DRS_STENCILWRITEMASK] = 0xFFFFFFFF;
	rs[D3DRS_CCW_STENCILFAIL] = D3DSTENCILOP_KEEP;
	rs[D3DRS_CCW_STENCILZFAIL] = D3DSTENCILOP_KEEP;
	rs[D3DRS_CCW_STENCILPASS] = D3DSTENCILOP_KEEP;
	rs[D3DRS_CCW_STENCILFUNC] = D3DCMP_ALWAYS;
	rs[D3DRS_COLORWRITEENABLE] = rs[D3DRS_COLORWRITEENABLE1] = rs[D3DRS_COLORWRITEENABLE2] = rs[D3DRS_COLORWRITEENABLE3] = 0xF;
	rs[D3DRS_BLENDOP] = D3DBLENDOP_ADD;
	rs[D3DRS_SRCBLENDALPHA] = D3DBLEND_ONE;
	rs[D3DRS_DESTBLENDALPHA] = D3DBLEND_ZERO;
	rs[D3DRS_BLENDOPALPHA] = D3DBLENDOP_ADD;
	rs[D3DRS_BLENDFACTOR] = 0xFFFFFFFF;
	rs[D3DRS_MULTISAMPLEANTIALIAS] = TRUE;
	rs[D3DRS_MULTISAMPLEMASK] = 0xFFFFFFFF;
	rs[D3DRS_LIGHTING] = TRUE;
	rs[D3DRS_CLIPPING] = TRUE;
	float one = 1.0f;
	memcpy(&rs[D3DRS_POINTSIZE], &one, 4);
	memcpy(&rs[D3DRS_POINTSIZE_MAX], &one, 4);
	for (int i = 0; i < 20; i++) {
		ss[i][D3DSAMP_ADDRESSU] = ss[i][D3DSAMP_ADDRESSV] = ss[i][D3DSAMP_ADDRESSW] = D3DTADDRESS_WRAP;
		ss[i][D3DSAMP_MAGFILTER] = ss[i][D3DSAMP_MINFILTER] = D3DTEXF_POINT;
		ss[i][D3DSAMP_MIPFILTER] = D3DTEXF_NONE;
		ss[i][D3DSAMP_MAXANISOTROPY] = 1;
	}
}

bool Device::Init(HWND hWnd, D3DPRESENT_PARAMETERS *ppIn)
{
	mtl = MTLCreateSystemDefaultDevice();
	if (!mtl) return false;
	owner = pthread_self();
	queue = [mtl newCommandQueueWithMaxCommandBufferCount:16];
	SetDefaults();
	visBuffer = [mtl newBufferWithLength:8 * 4096 options:MTLResourceStorageModeShared];
	zeroBuffer = [mtl newBufferWithLength:256 options:MTLResourceStorageModeShared];
	memset(zeroBuffer.contents, 0, 256);

	NSError *err = nil;
	utilLib = [mtl newLibraryWithSource:[NSString stringWithUTF8String:kUtilSource] options:nil error:&err];
	if (!utilLib) {
		Log("utility shaders failed: %s", err.localizedDescription.UTF8String);
		return false;
	}
	MTLSamplerDescriptor *sd = [MTLSamplerDescriptor new];
	sd.minFilter = sd.magFilter = MTLSamplerMinMagFilterLinear;
	sd.sAddressMode = sd.tAddressMode = MTLSamplerAddressModeClampToEdge;
	blitLinear = [mtl newSamplerStateWithDescriptor:sd];
	sd.minFilter = sd.magFilter = MTLSamplerMinMagFilterNearest;
	blitPoint = [mtl newSamplerStateWithDescriptor:sd];

	pp = *ppIn;
	if (!pp.hDeviceWindow) pp.hDeviceWindow = hWnd;
	SwapChain *sc = new SwapChain();
	sc->dev = this;
	sc->hWnd = pp.hDeviceWindow;
	sc->pp = pp;
	if (!sc->Create()) {
		delete sc;
		return false;
	}
	*ppIn = sc->pp;
	pp = sc->pp;
	chains.push_back(sc);
	rt[0] = sc->backBuffer;
	rt[0]->AddRef();
	ds = sc->depthStencil;
	if (ds) ds->AddRef();
	D3DSURFACE_DESC d;
	rt[0]->GetDesc(&d);
	viewport = {0, 0, d.Width, d.Height, 0.0f, 1.0f};
	scissor = {0, 0, (LONG)d.Width, (LONG)d.Height};
	return true;
}

// ---------------------------------------------------------------------------
// Command submission

id<MTLCommandBuffer> Device::Cmd()
{
	if (!cmd) {
		dispatch_semaphore_wait(inflight, DISPATCH_TIME_FOREVER);
		cmd = [queue commandBuffer];
		uint64_t s = serial;
		__block Device *self = this;
		dispatch_semaphore_t sem = inflight;
		[cmd addCompletedHandler:^(id<MTLCommandBuffer> cb) {
			if (cb.status == MTLCommandBufferStatusError)
				Log("command buffer failed: %s", cb.error.localizedDescription.UTF8String);
			if (s > self->completed) self->completed = s;
			dispatch_semaphore_signal(sem);
		}];
	}
	return cmd;
}

void Device::EndEncoders()
{
	if (renc) {
		[renc endEncoding];
		renc = nil;
	}
	if (benc) {
		[benc endEncoding];
		benc = nil;
	}
	passDirty = true;
}

void Device::Submit(bool wait)
{
	if (!cmd) return;
	EndEncoders();
	id<MTLCommandBuffer> c = cmd;
	cmd = nil;
	[c commit];
	serial++;
	if (wait) [c waitUntilCompleted];
}

void Device::WaitIdle()
{
	Submit(true);
}

void Device::FrameEnd()
{
	Submit(false);
}

id<MTLBuffer> Device::Upload(const void *data, UINT size, UINT &offset, UINT align)
{
	const UINT chunkSize = 8 * 1024 * 1024;
	UploadChunk *chunk = nullptr;
	for (auto &c : chunks) {
		if (c.serial == serial) {
			UINT o = (c.used + align - 1) & ~(align - 1);
			if (o + size <= c.buf.length) {
				chunk = &c;
				break;
			}
		}
	}
	if (!chunk) {
		for (auto &c : chunks) {
			if (c.serial != serial && !InFlight(c.serial) && size <= c.buf.length) {
				c.serial = serial;
				c.used = 0;
				chunk = &c;
				break;
			}
		}
	}
	if (!chunk) {
		UploadChunk c;
		c.buf = [mtl newBufferWithLength:std::max(chunkSize, size + align) options:MTLResourceStorageModeShared];
		c.serial = serial;
		chunks.push_back(c);
		chunk = &chunks.back();
	}
	UINT o = (chunk->used + align - 1) & ~(align - 1);
	if (data) memcpy((uint8_t *)chunk->buf.contents + o, data, size);
	chunk->used = o + size;
	offset = o;
	return chunk->buf;
}

// ---------------------------------------------------------------------------
// Render passes

static void AttachmentFor(Surface *s, __strong id<MTLTexture> &tex, __strong id<MTLTexture> &resolve, UINT &slice, UINT &level)
{
	Texture *t = s->tex;
	slice = t->type == TT_CUBE ? s->face : 0;
	level = s->level;
	if (t->msaa) {
		tex = t->msaa;
		resolve = t->tex;
	} else {
		tex = t->tex;
		resolve = nil;
	}
}

static std::unordered_map<uint64_t, id<MTLTexture>> g_scratchDepth;

static id<MTLTexture> ScratchDepth(Device *d, MTLPixelFormat pf, NSUInteger w, NSUInteger h, NSUInteger samples)
{
	uint64_t key = ((uint64_t)pf << 48) ^ ((uint64_t)w << 24) ^ (uint64_t)h ^ ((uint64_t)samples << 60);
	auto it = g_scratchDepth.find(key);
	if (it != g_scratchDepth.end()) return it->second;
	MTLTextureDescriptor *td = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:pf width:w height:h mipmapped:NO];
	td.storageMode = MTLStorageModePrivate;
	td.usage = MTLTextureUsageRenderTarget;
	if (samples > 1) {
		td.textureType = MTLTextureType2DMultisample;
		td.sampleCount = samples;
	}
	id<MTLTexture> t = [d->mtl newTextureWithDescriptor:td];
	g_scratchDepth[key] = t;
	return t;
}

MTLRenderPassDescriptor *Device::PassDescriptor()
{
	MTLRenderPassDescriptor *rp = [MTLRenderPassDescriptor renderPassDescriptor];
	NSUInteger w = 0, h = 0, samples = 1;
	for (int i = 0; i < 4; i++) {
		Surface *s = SurfImpl(rt[i]);
		if (!s) continue;
		id<MTLTexture> tex, resolve;
		UINT slice, level;
		AttachmentFor(s, tex, resolve, slice, level);
		MTLRenderPassColorAttachmentDescriptor *c = rp.colorAttachments[i];
		c.texture = tex;
		c.slice = slice;
		c.level = level;
		if (resolve) {
			c.resolveTexture = resolve;
			c.resolveSlice = slice;
			c.resolveLevel = level;
			c.storeAction = MTLStoreActionStoreAndMultisampleResolve;
		} else {
			c.storeAction = MTLStoreActionStore;
		}
		if (pendingClear && (clearFlags & D3DCLEAR_TARGET)) {
			c.loadAction = MTLLoadActionClear;
			D3DCOLOR k = clearColor;
			c.clearColor = MTLClearColorMake(((k >> 16) & 255) / 255.0, ((k >> 8) & 255) / 255.0, (k & 255) / 255.0, ((k >> 24) & 255) / 255.0);
		} else {
			c.loadAction = MTLLoadActionLoad;
		}
		if (!w) {
			w = s->tex->LevelWidth(level);
			h = s->tex->LevelHeight(level);
			samples = s->tex->samples;
		}
	}
	Surface *d = SurfImpl(ds);
	if (d && rs[D3DRS_ZENABLE] != D3DZB_FALSE + 100) {
		id<MTLTexture> dt = d->tex->msaa ? d->tex->msaa : d->tex->tex;
		if (w && (dt.width != w || dt.height != h || dt.sampleCount != samples))
			dt = ScratchDepth(this, dt.pixelFormat, w, h, samples);
		rp.depthAttachment.texture = dt;
		rp.depthAttachment.storeAction = MTLStoreActionStore;
		if (pendingClear && (clearFlags & D3DCLEAR_ZBUFFER)) {
			rp.depthAttachment.loadAction = MTLLoadActionClear;
			rp.depthAttachment.clearDepth = clearZ;
		} else {
			rp.depthAttachment.loadAction = MTLLoadActionLoad;
		}
		if (d->tex->fi->hasStencil) {
			rp.stencilAttachment.texture = dt;
			rp.stencilAttachment.storeAction = MTLStoreActionStore;
			if (pendingClear && (clearFlags & D3DCLEAR_STENCIL)) {
				rp.stencilAttachment.loadAction = MTLLoadActionClear;
				rp.stencilAttachment.clearStencil = clearStencil;
			} else {
				rp.stencilAttachment.loadAction = MTLLoadActionLoad;
			}
		}
		if (!w) {
			w = dt.width;
			h = dt.height;
		}
	}
	rp.visibilityResultBuffer = visBuffer;
	if (!w) {
		rp.renderTargetWidth = 1;
		rp.renderTargetHeight = 1;
		rp.defaultRasterSampleCount = 1;
	}
	pendingClear = false;
	return rp;
}

id<MTLRenderCommandEncoder> Device::RenderEncoder()
{
	if (renc && !passDirty) return renc;
	if (benc) {
		[benc endEncoding];
		benc = nil;
	}
	if (renc) {
		[renc endEncoding];
		renc = nil;
	}
	for (int i = 0; i < 4; i++) {
		Surface *s = SurfImpl(rt[i]);
		if (!s) continue;
		if (pendingClear && (clearFlags & D3DCLEAR_TARGET)) s->tex->msaaDirty = false;
		RefreshMSAA(s->tex);
	}
	renc = [Cmd() renderCommandEncoderWithDescriptor:PassDescriptor()];
	passDirty = false;
	return renc;
}

void Device::EncodeBlit(void (^fn)(id<MTLBlitCommandEncoder>), bool wait)
{
	if (OnDeviceThread()) {
		fn(BlitEncoder());
		if (wait) Submit(true);
		return;
	}
	@autoreleasepool {
		id<MTLCommandBuffer> c = [queue commandBuffer];
		id<MTLBlitCommandEncoder> b = [c blitCommandEncoder];
		fn(b);
		[b endEncoding];
		[c commit];
		if (wait) [c waitUntilCompleted];
	}
}

id<MTLBlitCommandEncoder> Device::BlitEncoder()
{
	if (benc) return benc;
	if (renc) {
		[renc endEncoding];
		renc = nil;
		passDirty = true;
	}
	benc = [Cmd() blitCommandEncoder];
	return benc;
}

// ---------------------------------------------------------------------------
// State translation

static MTLBlendFactor BlendFactor(DWORD b, bool alpha)
{
	switch (b) {
	case D3DBLEND_ZERO: return MTLBlendFactorZero;
	case D3DBLEND_ONE: return MTLBlendFactorOne;
	case D3DBLEND_SRCCOLOR: return alpha ? MTLBlendFactorSourceAlpha : MTLBlendFactorSourceColor;
	case D3DBLEND_INVSRCCOLOR: return alpha ? MTLBlendFactorOneMinusSourceAlpha : MTLBlendFactorOneMinusSourceColor;
	case D3DBLEND_SRCALPHA: return MTLBlendFactorSourceAlpha;
	case D3DBLEND_INVSRCALPHA: return MTLBlendFactorOneMinusSourceAlpha;
	case D3DBLEND_DESTALPHA: return MTLBlendFactorDestinationAlpha;
	case D3DBLEND_INVDESTALPHA: return MTLBlendFactorOneMinusDestinationAlpha;
	case D3DBLEND_DESTCOLOR: return alpha ? MTLBlendFactorDestinationAlpha : MTLBlendFactorDestinationColor;
	case D3DBLEND_INVDESTCOLOR: return alpha ? MTLBlendFactorOneMinusDestinationAlpha : MTLBlendFactorOneMinusDestinationColor;
	case D3DBLEND_SRCALPHASAT: return MTLBlendFactorSourceAlphaSaturated;
	case D3DBLEND_BOTHSRCALPHA: return MTLBlendFactorSourceAlpha;
	case D3DBLEND_BOTHINVSRCALPHA: return MTLBlendFactorOneMinusSourceAlpha;
	case D3DBLEND_BLENDFACTOR: return alpha ? MTLBlendFactorBlendAlpha : MTLBlendFactorBlendColor;
	case D3DBLEND_INVBLENDFACTOR: return alpha ? MTLBlendFactorOneMinusBlendAlpha : MTLBlendFactorOneMinusBlendColor;
	default: return MTLBlendFactorOne;
	}
}

static MTLBlendOperation BlendOp(DWORD op)
{
	switch (op) {
	case D3DBLENDOP_SUBTRACT: return MTLBlendOperationSubtract;
	case D3DBLENDOP_REVSUBTRACT: return MTLBlendOperationReverseSubtract;
	case D3DBLENDOP_MIN: return MTLBlendOperationMin;
	case D3DBLENDOP_MAX: return MTLBlendOperationMax;
	default: return MTLBlendOperationAdd;
	}
}

static MTLCompareFunction CompareFunc(DWORD f)
{
	switch (f) {
	case D3DCMP_NEVER: return MTLCompareFunctionNever;
	case D3DCMP_LESS: return MTLCompareFunctionLess;
	case D3DCMP_EQUAL: return MTLCompareFunctionEqual;
	case D3DCMP_LESSEQUAL: return MTLCompareFunctionLessEqual;
	case D3DCMP_GREATER: return MTLCompareFunctionGreater;
	case D3DCMP_NOTEQUAL: return MTLCompareFunctionNotEqual;
	case D3DCMP_GREATEREQUAL: return MTLCompareFunctionGreaterEqual;
	default: return MTLCompareFunctionAlways;
	}
}

static MTLStencilOperation StencilOp(DWORD op)
{
	switch (op) {
	case D3DSTENCILOP_ZERO: return MTLStencilOperationZero;
	case D3DSTENCILOP_REPLACE: return MTLStencilOperationReplace;
	case D3DSTENCILOP_INCRSAT: return MTLStencilOperationIncrementClamp;
	case D3DSTENCILOP_DECRSAT: return MTLStencilOperationDecrementClamp;
	case D3DSTENCILOP_INVERT: return MTLStencilOperationInvert;
	case D3DSTENCILOP_INCR: return MTLStencilOperationIncrementWrap;
	case D3DSTENCILOP_DECR: return MTLStencilOperationDecrementWrap;
	default: return MTLStencilOperationKeep;
	}
}

static MTLVertexFormat VertexFormat(BYTE type)
{
	switch (type) {
	case D3DDECLTYPE_FLOAT1: return MTLVertexFormatFloat;
	case D3DDECLTYPE_FLOAT2: return MTLVertexFormatFloat2;
	case D3DDECLTYPE_FLOAT3: return MTLVertexFormatFloat3;
	case D3DDECLTYPE_FLOAT4: return MTLVertexFormatFloat4;
	case D3DDECLTYPE_D3DCOLOR: return MTLVertexFormatUChar4Normalized_BGRA;
	case D3DDECLTYPE_UBYTE4: return MTLVertexFormatUChar4;
	case D3DDECLTYPE_SHORT2: return MTLVertexFormatShort2;
	case D3DDECLTYPE_SHORT4: return MTLVertexFormatShort4;
	case D3DDECLTYPE_UBYTE4N: return MTLVertexFormatUChar4Normalized;
	case D3DDECLTYPE_SHORT2N: return MTLVertexFormatShort2Normalized;
	case D3DDECLTYPE_SHORT4N: return MTLVertexFormatShort4Normalized;
	case D3DDECLTYPE_USHORT2N: return MTLVertexFormatUShort2Normalized;
	case D3DDECLTYPE_USHORT4N: return MTLVertexFormatUShort4Normalized;
	case D3DDECLTYPE_UDEC3: return MTLVertexFormatUInt1010102Normalized;
	case D3DDECLTYPE_DEC3N: return MTLVertexFormatInt1010102Normalized;
	case D3DDECLTYPE_FLOAT16_2: return MTLVertexFormatHalf2;
	case D3DDECLTYPE_FLOAT16_4: return MTLVertexFormatHalf4;
	default: return MTLVertexFormatInvalid;
	}
}

static inline uint64_t Mix(uint64_t h, uint64_t v)
{
	h ^= v + 0x9e3779b97f4a7c15ull + (h << 6) + (h >> 2);
	return h;
}

static void PassFormats(Device *d, MTLPixelFormat color[4], MTLPixelFormat &depth, MTLPixelFormat &stencil, NSUInteger &samples)
{
	samples = 1;
	for (int i = 0; i < 4; i++) {
		Surface *s = SurfImpl(d->rt[i]);
		color[i] = s ? s->tex->fi->pf : MTLPixelFormatInvalid;
		if (s && i == 0) samples = s->tex->samples;
		if (s && !samples) samples = s->tex->samples;
	}
	Surface *z = SurfImpl(d->ds);
	depth = z ? z->tex->fi->pf : MTLPixelFormatInvalid;
	stencil = (z && z->tex->fi->hasStencil) ? z->tex->fi->pf : MTLPixelFormatInvalid;
	if (!SurfImpl(d->rt[0]) && z) samples = z->tex->samples;
}

const Device::PipelineEntry *Device::Pipeline(id<MTLFunction> vfn, id<MTLFunction> ffn, const Decl *dcl, const UINT *strides)
{
	MTLPixelFormat color[4], depth, stencil;
	NSUInteger samples;
	PassFormats(this, color, depth, stencil, samples);

	bool blend = rs[D3DRS_ALPHABLENDENABLE] != 0;
	bool sepAlpha = rs[D3DRS_SEPARATEALPHABLENDENABLE] != 0;
	DWORD src = rs[D3DRS_SRCBLEND], dst = rs[D3DRS_DESTBLEND];
	if (src == D3DBLEND_BOTHSRCALPHA) { src = D3DBLEND_SRCALPHA; dst = D3DBLEND_INVSRCALPHA; }
	if (src == D3DBLEND_BOTHINVSRCALPHA) { src = D3DBLEND_INVSRCALPHA; dst = D3DBLEND_SRCALPHA; }
	DWORD srcA = sepAlpha ? rs[D3DRS_SRCBLENDALPHA] : src, dstA = sepAlpha ? rs[D3DRS_DESTBLENDALPHA] : dst;
	DWORD op = rs[D3DRS_BLENDOP], opA = sepAlpha ? rs[D3DRS_BLENDOPALPHA] : op;
	DWORD masks[4] = {rs[D3DRS_COLORWRITEENABLE], rs[D3DRS_COLORWRITEENABLE1], rs[D3DRS_COLORWRITEENABLE2], rs[D3DRS_COLORWRITEENABLE3]};

	uint64_t h = (uint64_t)(uintptr_t)(__bridge void *)vfn;
	h = Mix(h, (uint64_t)(uintptr_t)(__bridge void *)ffn);
	h = Mix(h, dcl ? dcl->hash : 0);
	for (int i = 0; i < 8; i++) h = Mix(h, strides[i]);
	for (int i = 0; i < 4; i++) h = Mix(h, color[i] | ((uint64_t)masks[i] << 32));
	h = Mix(h, depth);
	h = Mix(h, stencil);
	h = Mix(h, samples);
	h = Mix(h, blend ? (src | (dst << 8) | (srcA << 16) | (dstA << 24) | ((uint64_t)op << 32) | ((uint64_t)opA << 40)) : 0);
	h = Mix(h, rs[D3DRS_FILLMODE] == D3DFILL_POINT);

	auto it = pipelines.find(h);
	if (it != pipelines.end()) return it->second.pso ? &it->second : nullptr;

	MTLRenderPipelineDescriptor *pd = [MTLRenderPipelineDescriptor new];
	pd.vertexFunction = vfn;
	pd.fragmentFunction = ffn;
	pd.rasterSampleCount = samples;
	for (int i = 0; i < 4; i++) {
		pd.colorAttachments[i].pixelFormat = color[i];
		if (color[i] == MTLPixelFormatInvalid) continue;
		MTLColorWriteMask m = MTLColorWriteMaskNone;
		if (masks[i] & D3DCOLORWRITEENABLE_RED) m |= MTLColorWriteMaskRed;
		if (masks[i] & D3DCOLORWRITEENABLE_GREEN) m |= MTLColorWriteMaskGreen;
		if (masks[i] & D3DCOLORWRITEENABLE_BLUE) m |= MTLColorWriteMaskBlue;
		if (masks[i] & D3DCOLORWRITEENABLE_ALPHA) m |= MTLColorWriteMaskAlpha;
		pd.colorAttachments[i].writeMask = m;
		if (blend) {
			pd.colorAttachments[i].blendingEnabled = YES;
			pd.colorAttachments[i].sourceRGBBlendFactor = BlendFactor(src, false);
			pd.colorAttachments[i].destinationRGBBlendFactor = BlendFactor(dst, false);
			pd.colorAttachments[i].rgbBlendOperation = BlendOp(op);
			pd.colorAttachments[i].sourceAlphaBlendFactor = BlendFactor(srcA, true);
			pd.colorAttachments[i].destinationAlphaBlendFactor = BlendFactor(dstA, true);
			pd.colorAttachments[i].alphaBlendOperation = BlendOp(opA);
		}
	}
	pd.depthAttachmentPixelFormat = depth;
	pd.stencilAttachmentPixelFormat = stencil;

	if (vfn.vertexAttributes.count) {
		MTLVertexDescriptor *vd = [MTLVertexDescriptor vertexDescriptor];
		bool usedStream[8] = {};
		bool zeroUsed = false;
		for (MTLVertexAttribute *a in vfn.vertexAttributes) {
			if (!a.active) continue;
			NSUInteger idx = a.attributeIndex;
			const D3DVERTEXELEMENT9 *found = nullptr;
			if (dcl)
				for (const auto &e : dcl->elems)
					if ((NSUInteger)AttributeIndex(e.Usage, e.UsageIndex) == idx) { found = &e; break; }
			if (found && found->Stream < 8) {
				vd.attributes[idx].format = VertexFormat(found->Type);
				vd.attributes[idx].offset = found->Offset;
				vd.attributes[idx].bufferIndex = 20 + found->Stream;
				usedStream[found->Stream] = true;
			} else {
				vd.attributes[idx].format = MTLVertexFormatFloat4;
				vd.attributes[idx].offset = 0;
				vd.attributes[idx].bufferIndex = 30;
				zeroUsed = true;
			}
		}
		for (int s = 0; s < 8; s++) {
			if (!usedStream[s]) continue;
			vd.layouts[20 + s].stride = strides[s] ? strides[s] : 4;
			vd.layouts[20 + s].stepFunction = MTLVertexStepFunctionPerVertex;
		}
		if (zeroUsed) {
			vd.layouts[30].stride = 16;
			vd.layouts[30].stepFunction = MTLVertexStepFunctionConstant;
			vd.layouts[30].stepRate = 0;
		}
		pd.vertexDescriptor = vd;
	}
	if (rs[D3DRS_FILLMODE] == D3DFILL_POINT) pd.inputPrimitiveTopology = MTLPrimitiveTopologyClassPoint;

	NSError *err = nil;
	MTLAutoreleasedRenderPipelineReflection refl = nil;
	id<MTLRenderPipelineState> ps = [mtl newRenderPipelineStateWithDescriptor:pd options:MTLPipelineOptionBindingInfo reflection:&refl error:&err];
	if (!ps) Log("pipeline (%s, %s) failed: %s", vfn.name.UTF8String, ffn.name.UTF8String, err.localizedDescription.UTF8String);
	PipelineEntry &e = pipelines[h];
	e.pso = ps;
	auto collect = [](NSArray<id<MTLBinding>> *bindings, std::vector<PipelineBinding> &tex, std::vector<PipelineBinding> &samp, std::vector<PipelineBinding> &buf) {
		for (id<MTLBinding> b in bindings) {
			if (!b.used) continue;
			if (b.type == MTLBindingTypeTexture) {
				id<MTLTextureBinding> tb = (id<MTLTextureBinding>)b;
				tex.push_back({(uint32_t)b.index, tb.textureType, tb.isDepthTexture != NO, 0});
			} else if (b.type == MTLBindingTypeSampler) {
				samp.push_back({(uint32_t)b.index, MTLTextureType2D, false, 0});
			} else if (b.type == MTLBindingTypeBuffer) {
				id<MTLBufferBinding> bb = (id<MTLBufferBinding>)b;
				buf.push_back({(uint32_t)b.index, MTLTextureType2D, false, (uint32_t)bb.bufferDataSize});
			}
		}
	};
	if (refl) {
		collect(refl.vertexBindings, e.vtex, e.vsamp, e.vbuf);
		collect(refl.fragmentBindings, e.ftex, e.fsamp, e.fbuf);
	}
	return ps ? &e : nullptr;
}

id<MTLTexture> Device::Dummy(MTLTextureType type, bool depth)
{
	auto make = [&](MTLTextureType t, MTLPixelFormat pf) {
		MTLTextureDescriptor *d = [MTLTextureDescriptor new];
		d.textureType = t;
		d.pixelFormat = pf;
		d.width = d.height = 1;
		d.depth = 1;
		d.storageMode = MTLStorageModePrivate;
		d.usage = MTLTextureUsageShaderRead | MTLTextureUsageRenderTarget;
		id<MTLTexture> tex = [mtl newTextureWithDescriptor:d];
		if (pf == MTLPixelFormatDepth32Float) {
			MTLRenderPassDescriptor *rp = [MTLRenderPassDescriptor renderPassDescriptor];
			rp.depthAttachment.texture = tex;
			rp.depthAttachment.loadAction = MTLLoadActionClear;
			rp.depthAttachment.clearDepth = 1.0;
			rp.depthAttachment.storeAction = MTLStoreActionStore;
			id<MTLCommandBuffer> c = [queue commandBuffer];
			[[c renderCommandEncoderWithDescriptor:rp] endEncoding];
			[c commit];
		} else {
			d.usage = MTLTextureUsageShaderRead;
			d.storageMode = MTLStorageModeShared;
			tex = [mtl newTextureWithDescriptor:d];
			uint32_t zero[6] = {};
			for (NSUInteger s = 0; s < (t == MTLTextureTypeCube ? 6 : 1); s++)
				[tex replaceRegion:MTLRegionMake3D(0, 0, 0, 1, 1, 1) mipmapLevel:0 slice:s withBytes:zero bytesPerRow:4 bytesPerImage:4];
		}
		return tex;
	};
	if (depth) return dummyDepth ? dummyDepth : (dummyDepth = make(MTLTextureType2D, MTLPixelFormatDepth32Float));
	if (type == MTLTextureTypeCube) return dummyCube ? dummyCube : (dummyCube = make(MTLTextureTypeCube, MTLPixelFormatRGBA8Unorm));
	if (type == MTLTextureType3D) return dummy3D ? dummy3D : (dummy3D = make(MTLTextureType3D, MTLPixelFormatRGBA8Unorm));
	return dummy2D ? dummy2D : (dummy2D = make(MTLTextureType2D, MTLPixelFormatRGBA8Unorm));
}

id<MTLDepthStencilState> Device::DepthStencil()
{
	Surface *z = SurfImpl(ds);
	bool zen = z && rs[D3DRS_ZENABLE] != D3DZB_FALSE;
	bool sten = z && z->tex->fi->hasStencil && rs[D3DRS_STENCILENABLE];
	bool twoSided = rs[D3DRS_TWOSIDEDSTENCILMODE] != 0;
	uint64_t h = zen ? (1 | ((uint64_t)rs[D3DRS_ZWRITEENABLE] << 1) | ((uint64_t)rs[D3DRS_ZFUNC] << 2)) : 0;
	if (sten) {
		h = Mix(h, rs[D3DRS_STENCILFAIL] | (rs[D3DRS_STENCILZFAIL] << 4) | (rs[D3DRS_STENCILPASS] << 8) | (rs[D3DRS_STENCILFUNC] << 12));
		h = Mix(h, ((uint64_t)rs[D3DRS_STENCILMASK] << 32) | rs[D3DRS_STENCILWRITEMASK]);
		if (twoSided) h = Mix(h, rs[D3DRS_CCW_STENCILFAIL] | (rs[D3DRS_CCW_STENCILZFAIL] << 4) | (rs[D3DRS_CCW_STENCILPASS] << 8) | (rs[D3DRS_CCW_STENCILFUNC] << 12) | (1ull << 40));
	}
	auto it = dsStates.find(h);
	if (it != dsStates.end()) return it->second;
	MTLDepthStencilDescriptor *dd = [MTLDepthStencilDescriptor new];
	if (zen) {
		dd.depthCompareFunction = CompareFunc(rs[D3DRS_ZFUNC]);
		dd.depthWriteEnabled = rs[D3DRS_ZWRITEENABLE] != 0;
	} else {
		dd.depthCompareFunction = MTLCompareFunctionAlways;
		dd.depthWriteEnabled = NO;
	}
	if (sten) {
		MTLStencilDescriptor *f = [MTLStencilDescriptor new];
		f.stencilFailureOperation = StencilOp(rs[D3DRS_STENCILFAIL]);
		f.depthFailureOperation = StencilOp(rs[D3DRS_STENCILZFAIL]);
		f.depthStencilPassOperation = StencilOp(rs[D3DRS_STENCILPASS]);
		f.stencilCompareFunction = CompareFunc(rs[D3DRS_STENCILFUNC]);
		f.readMask = rs[D3DRS_STENCILMASK] & 0xFF;
		f.writeMask = rs[D3DRS_STENCILWRITEMASK] & 0xFF;
		MTLStencilDescriptor *b = f;
		if (twoSided) {
			b = [MTLStencilDescriptor new];
			b.stencilFailureOperation = StencilOp(rs[D3DRS_CCW_STENCILFAIL]);
			b.depthFailureOperation = StencilOp(rs[D3DRS_CCW_STENCILZFAIL]);
			b.depthStencilPassOperation = StencilOp(rs[D3DRS_CCW_STENCILPASS]);
			b.stencilCompareFunction = CompareFunc(rs[D3DRS_CCW_STENCILFUNC]);
			b.readMask = f.readMask;
			b.writeMask = f.writeMask;
		}
		dd.frontFaceStencil = f;
		dd.backFaceStencil = b;
	}
	id<MTLDepthStencilState> s = [mtl newDepthStencilStateWithDescriptor:dd];
	dsStates[h] = s;
	return s;
}

static MTLSamplerAddressMode AddressMode(DWORD a)
{
	switch (a) {
	case D3DTADDRESS_MIRROR: return MTLSamplerAddressModeMirrorRepeat;
	case D3DTADDRESS_CLAMP: return MTLSamplerAddressModeClampToEdge;
	case D3DTADDRESS_BORDER: return MTLSamplerAddressModeClampToBorderColor;
	case D3DTADDRESS_MIRRORONCE: return MTLSamplerAddressModeMirrorClampToEdge;
	default: return MTLSamplerAddressModeRepeat;
	}
}

id<MTLSamplerState> Device::Sampler(UINT idx)
{
	const DWORD *s = ss[idx];
	uint64_t h = s[D3DSAMP_ADDRESSU] | (s[D3DSAMP_ADDRESSV] << 4) | (s[D3DSAMP_ADDRESSW] << 8) | (s[D3DSAMP_MAGFILTER] << 12) |
		(s[D3DSAMP_MINFILTER] << 16) | (s[D3DSAMP_MIPFILTER] << 20) | ((uint64_t)(s[D3DSAMP_MAXANISOTROPY] & 0xFF) << 24) |
		((uint64_t)s[D3DSAMP_MAXMIPLEVEL] << 32) | ((uint64_t)(s[D3DSAMP_BORDERCOLOR] != 0) << 40);
	auto it = samplers.find(h);
	if (it != samplers.end()) return it->second;
	MTLSamplerDescriptor *d = [MTLSamplerDescriptor new];
	d.sAddressMode = AddressMode(s[D3DSAMP_ADDRESSU]);
	d.tAddressMode = AddressMode(s[D3DSAMP_ADDRESSV]);
	d.rAddressMode = AddressMode(s[D3DSAMP_ADDRESSW]);
	d.magFilter = s[D3DSAMP_MAGFILTER] == D3DTEXF_POINT ? MTLSamplerMinMagFilterNearest : MTLSamplerMinMagFilterLinear;
	d.minFilter = s[D3DSAMP_MINFILTER] == D3DTEXF_POINT ? MTLSamplerMinMagFilterNearest : MTLSamplerMinMagFilterLinear;
	d.mipFilter = s[D3DSAMP_MIPFILTER] == D3DTEXF_NONE ? MTLSamplerMipFilterNotMipmapped :
		s[D3DSAMP_MIPFILTER] == D3DTEXF_POINT ? MTLSamplerMipFilterNearest : MTLSamplerMipFilterLinear;
	if (s[D3DSAMP_MINFILTER] == D3DTEXF_ANISOTROPIC || s[D3DSAMP_MAGFILTER] == D3DTEXF_ANISOTROPIC)
		d.maxAnisotropy = std::max<DWORD>(1, std::min<DWORD>(16, s[D3DSAMP_MAXANISOTROPY]));
	d.lodMinClamp = (float)s[D3DSAMP_MAXMIPLEVEL];
	if (s[D3DSAMP_BORDERCOLOR]) d.borderColor = (s[D3DSAMP_BORDERCOLOR] >> 24) ? MTLSamplerBorderColorOpaqueWhite : MTLSamplerBorderColorTransparentBlack;
	id<MTLSamplerState> st = [mtl newSamplerStateWithDescriptor:d];
	samplers[h] = st;
	return st;
}

// ---------------------------------------------------------------------------
// Draw

struct M9FragState {
	int alphaTest;
	float alphaRef;
	int alphaFunc;
	int pad;
};

bool Device::BindState(id<MTLRenderCommandEncoder> enc, const void *upVerts, UINT upStride, UINT upCount)
{
	Program *vp = nullptr, *fp = nullptr;
	id<MTLFunction> vfn = nil, ffn = nil;
	Effect *fx = effect;
	if (fx && effectPass >= 0 && fx->current && effectPass < (int)fx->current->passes.size()) {
		Pass &p = fx->current->passes[effectPass];
		vfn = p.vs;
		ffn = p.ps;
	} else {
		fx = nullptr;
		if (vs && vs->Impl()) { vp = vs->Impl()->prog.get(); vfn = vp->fn; }
		if (ps && ps->Impl()) { fp = ps->Impl()->prog.get(); ffn = fp->fn; }
	}
	if (!vfn) return false;

	Decl *dcl = decl ? decl->Impl() : nullptr;
	UINT strides[8] = {};
	for (int s = 0; s < 8; s++) strides[s] = streams[s].stride;
	if (upVerts) strides[0] = upStride;

	if (const char *skip = getenv("M9_SKIP")) if (ffn && strstr(skip, ffn.name.UTF8String)) return false;
	if (getenv("M9_DRAWLOG") && drawLogActive) drawLog[std::string(vfn.name.UTF8String) + "/" + (ffn ? ffn.name.UTF8String : "-")]++;
	const PipelineEntry *pe = Pipeline(vfn, ffn, dcl, strides);
	if (!pe) return false;
	[enc setRenderPipelineState:pe->pso];
	[enc setDepthStencilState:DepthStencil()];
	[enc setStencilReferenceValue:rs[D3DRS_STENCILREF] & 0xFF];

	switch (rs[D3DRS_CULLMODE]) {
	case D3DCULL_CW: [enc setCullMode:MTLCullModeFront]; break;
	case D3DCULL_CCW: [enc setCullMode:MTLCullModeBack]; break;
	default: [enc setCullMode:MTLCullModeNone]; break;
	}
	[enc setFrontFacingWinding:MTLWindingClockwise];
	[enc setTriangleFillMode:rs[D3DRS_FILLMODE] == D3DFILL_WIREFRAME ? MTLTriangleFillModeLines : MTLTriangleFillModeFill];
	float bias, slope;
	memcpy(&bias, &rs[D3DRS_DEPTHBIAS], 4);
	memcpy(&slope, &rs[D3DRS_SLOPESCALEDEPTHBIAS], 4);
	[enc setDepthBias:bias * 16777215.0f slopeScale:slope clamp:0];
	D3DCOLOR bf = rs[D3DRS_BLENDFACTOR];
	[enc setBlendColorRed:((bf >> 16) & 255) / 255.0f green:((bf >> 8) & 255) / 255.0f blue:(bf & 255) / 255.0f alpha:((bf >> 24) & 255) / 255.0f];

	NSUInteger tw = 1, th = 1;
	if (Surface *s0 = SurfImpl(rt[0])) { tw = s0->tex->LevelWidth(s0->level); th = s0->tex->LevelHeight(s0->level); }
	else if (Surface *z = SurfImpl(ds)) { tw = z->tex->width; th = z->tex->height; }
	MTLViewport v;
	v.originX = viewport.X;
	v.originY = viewport.Y;
	v.width = viewport.Width;
	v.height = viewport.Height;
	v.znear = viewport.MinZ;
	v.zfar = viewport.MaxZ;
	[enc setViewport:v];
	MTLScissorRect sr = {0, 0, tw, th};
	if (rs[D3DRS_SCISSORTESTENABLE]) {
		LONG l = std::max<LONG>(0, scissor.left), t = std::max<LONG>(0, scissor.top);
		LONG r = std::min<LONG>((LONG)tw, scissor.right), b = std::min<LONG>((LONG)th, scissor.bottom);
		sr = {(NSUInteger)l, (NSUInteger)t, (NSUInteger)std::max<LONG>(0, r - l), (NSUInteger)std::max<LONG>(0, b - t)};
	}
	[enc setScissorRect:sr];

	// vertex streams
	if (upVerts) {
		UINT off;
		id<MTLBuffer> b = Upload(upVerts, upStride * upCount, off, 16);
		[enc setVertexBuffer:b offset:off atIndex:20];
	} else {
		for (int s = 0; s < 8; s++) {
			if (!streams[s].vb) continue;
			Buffer *b = streams[s].vb->Impl();
			b->lastUse = serial;
			[enc setVertexBuffer:b->buf offset:streams[s].offset atIndex:20 + s];
		}
	}
	[enc setVertexBuffer:zeroBuffer offset:0 atIndex:30];

	// constants
	id<MTLTexture> ftex[32] = {}, vtex[32] = {};
	bool vconst = false, fconst = false;
	if (fx) {
		if (!fx->data.empty()) {
			UINT off;
			id<MTLBuffer> b = Upload(fx->data.data(), (UINT)fx->data.size(), off);
			[enc setVertexBuffer:b offset:off atIndex:0];
			[enc setFragmentBuffer:b offset:off atIndex:0];
			vconst = fconst = true;
		}
		for (auto &p : fx->params) {
			if (p->textureSlot < 0 || p->textureSlot >= 32) continue;
			id<MTLTexture> t = p->texture && p->texture->Impl() ? p->texture->Impl()->SampleTexture() : nil;
			ftex[p->textureSlot] = vtex[p->textureSlot] = t;
		}
	} else {
		if (vp && !vp->data.empty()) {
			UINT off;
			id<MTLBuffer> b = Upload(vp->data.data(), (UINT)vp->data.size(), off);
			[enc setVertexBuffer:b offset:off atIndex:0];
			vconst = true;
		}
		if (fp && !fp->data.empty()) {
			UINT off;
			id<MTLBuffer> b = Upload(fp->data.data(), (UINT)fp->data.size(), off);
			[enc setFragmentBuffer:b offset:off atIndex:0];
			fconst = true;
		}
		for (int i = 0; i < 16; i++) {
			Texture *t = textures[i] ? textures[i]->Impl() : nullptr;
			ftex[i] = t ? t->SampleTexture() : nil;
		}
		for (int i = 0; i < 4; i++) {
			Texture *t = textures[16 + i] ? textures[16 + i]->Impl() : nullptr;
			vtex[i] = t ? t->SampleTexture() : nil;
		}
	}
	auto pick = [&](id<MTLTexture> t, const PipelineBinding &b) -> id<MTLTexture> {
		if (t && t.textureType == b.texType && ((t.pixelFormat == MTLPixelFormatDepth32Float || t.pixelFormat == MTLPixelFormatDepth32Float_Stencil8 || t.pixelFormat == MTLPixelFormatDepth16Unorm) == b.depth)) return t;
		return Dummy(b.texType, b.depth);
	};
	for (const auto &b : pe->ftex) [enc setFragmentTexture:pick(b.index < 32 ? ftex[b.index] : nil, b) atIndex:b.index];
	for (const auto &b : pe->vtex) [enc setVertexTexture:pick(b.index < 32 ? vtex[b.index] : nil, b) atIndex:b.index];
	for (const auto &b : pe->fsamp) [enc setFragmentSamplerState:Sampler(b.index < 16 ? b.index : 0) atIndex:b.index];
	for (const auto &b : pe->vsamp) [enc setVertexSamplerState:Sampler(b.index < 4 ? 16 + b.index : 16) atIndex:b.index];
	auto fillBuffers = [&](const std::vector<PipelineBinding> &bufs, bool vertex, bool haveConst) {
		for (const auto &b : bufs) {
			if (b.index >= 20 || b.index == 29 || (b.index == 0 && haveConst)) continue;
			UINT off;
			id<MTLBuffer> z = Upload(nullptr, std::max(16u, b.size), off);
			memset((uint8_t *)z.contents + off, 0, std::max(16u, b.size));
			if (vertex) [enc setVertexBuffer:z offset:off atIndex:b.index];
			else [enc setFragmentBuffer:z offset:off atIndex:b.index];
		}
	};
	fillBuffers(pe->vbuf, true, vconst);
	fillBuffers(pe->fbuf, false, fconst);
	M9FragState fs = {(int)rs[D3DRS_ALPHATESTENABLE], (float)(rs[D3DRS_ALPHAREF] & 0xFF) / 255.0f, (int)rs[D3DRS_ALPHAFUNC], 0};
	[enc setFragmentBytes:&fs length:sizeof(fs) atIndex:29];
	return true;
}

static MTLPrimitiveType PrimType(D3DPRIMITIVETYPE t)
{
	switch (t) {
	case D3DPT_POINTLIST: return MTLPrimitiveTypePoint;
	case D3DPT_LINELIST: return MTLPrimitiveTypeLine;
	case D3DPT_LINESTRIP: return MTLPrimitiveTypeLineStrip;
	case D3DPT_TRIANGLESTRIP: return MTLPrimitiveTypeTriangleStrip;
	default: return MTLPrimitiveTypeTriangle;
	}
}

HRESULT Device::Draw(D3DPRIMITIVETYPE type, UINT vertexStart, UINT vertexCount, const void *upVerts, UINT upStride,
	IDirect3DIndexBuffer9 *ib, const void *upIndices, D3DFORMAT upIndexFmt, UINT indexStart, UINT indexCount, INT baseVertex)
{
	bool indexed = ib || upIndices;
	if (indexed ? indexCount == 0 : vertexCount == 0) return S_OK;
	id<MTLRenderCommandEncoder> enc = RenderEncoder();
	UINT upCount = vertexCount;
	if (!BindState(enc, upVerts, upStride, upCount)) return S_OK;

	MTLPrimitiveType pt = PrimType(type);
	if (type == D3DPT_TRIANGLEFAN) {
		UINT n = indexed ? indexCount : vertexCount;
		if (n < 3) return S_OK;
		std::vector<uint32_t> idx;
		idx.reserve((n - 2) * 3);
		auto src = [&](UINT i) -> uint32_t {
			if (!indexed) return vertexStart + i;
			if (upIndices) return upIndexFmt == D3DFMT_INDEX32 ? ((const uint32_t *)upIndices)[indexStart + i] : ((const uint16_t *)upIndices)[indexStart + i];
			Buffer *b = ib->Impl();
			if (b->fmt == D3DFMT_INDEX32) return ((const uint32_t *)b->buf.contents)[indexStart + i];
			return ((const uint16_t *)b->buf.contents)[indexStart + i];
		};
		for (UINT i = 1; i + 1 < n; i++) {
			idx.push_back(src(0));
			idx.push_back(src(i));
			idx.push_back(src(i + 1));
		}
		UINT off;
		id<MTLBuffer> b = Upload(idx.data(), (UINT)(idx.size() * 4), off, 4);
		[enc drawIndexedPrimitives:MTLPrimitiveTypeTriangle indexCount:idx.size() indexType:MTLIndexTypeUInt32
			indexBuffer:b indexBufferOffset:off instanceCount:1 baseVertex:(indexed ? baseVertex : 0) baseInstance:0];
		return S_OK;
	}
	if (!indexed) {
		if (upVerts) [enc drawPrimitives:pt vertexStart:0 vertexCount:vertexCount];
		else [enc drawPrimitives:pt vertexStart:vertexStart vertexCount:vertexCount];
		return S_OK;
	}
	if (upIndices) {
		UINT isz = upIndexFmt == D3DFMT_INDEX32 ? 4 : 2;
		UINT off;
		id<MTLBuffer> b = Upload((const uint8_t *)upIndices + (size_t)indexStart * isz, indexCount * isz, off, 4);
		[enc drawIndexedPrimitives:pt indexCount:indexCount indexType:(isz == 4 ? MTLIndexTypeUInt32 : MTLIndexTypeUInt16)
			indexBuffer:b indexBufferOffset:off instanceCount:1 baseVertex:baseVertex baseInstance:0];
		return S_OK;
	}
	Buffer *b = ib->Impl();
	b->lastUse = serial;
	UINT isz = b->fmt == D3DFMT_INDEX32 ? 4 : 2;
	[enc drawIndexedPrimitives:pt indexCount:indexCount indexType:(isz == 4 ? MTLIndexTypeUInt32 : MTLIndexTypeUInt16)
		indexBuffer:b->buf indexBufferOffset:(NSUInteger)indexStart * isz instanceCount:1 baseVertex:baseVertex baseInstance:0];
	return S_OK;
}

// ---------------------------------------------------------------------------
// Clears and blits

void Device::ClearInPass(DWORD flags, D3DCOLOR color, float z, DWORD stencil, const D3DRECT *rect)
{
	id<MTLRenderCommandEncoder> enc = RenderEncoder();
	MTLPixelFormat cf[4], df, sf;
	NSUInteger samples;
	PassFormats(this, cf, df, sf, samples);
	int ncolor = 0;
	for (int i = 0; i < 4; i++) if (cf[i] != MTLPixelFormatInvalid) ncolor = i + 1;
	bool doColor = (flags & D3DCLEAR_TARGET) && ncolor;
	bool doDepth = (flags & D3DCLEAR_ZBUFFER) && df != MTLPixelFormatInvalid;
	bool doStencil = (flags & D3DCLEAR_STENCIL) && sf != MTLPixelFormatInvalid;

	uint64_t h = Mix(Mix(0xC1EA4ull, ncolor | (doColor << 4) | (doDepth << 5) | (doStencil << 6)), samples);
	for (int i = 0; i < 4; i++) h = Mix(h, cf[i]);
	h = Mix(Mix(h, df), sf);
	id<MTLRenderPipelineState> pso;
	auto it = blitPipelines.find(h);
	if (it != blitPipelines.end()) pso = it->second;
	else {
		MTLRenderPipelineDescriptor *pd = [MTLRenderPipelineDescriptor new];
		pd.vertexFunction = [utilLib newFunctionWithName:@"BlitVSQuad"];
		NSString *fn = doColor ? [NSString stringWithFormat:@"ClearPS%d", ncolor] : @"ClearPS0";
		pd.fragmentFunction = [utilLib newFunctionWithName:fn];
		pd.rasterSampleCount = samples;
		for (int i = 0; i < 4; i++) {
			pd.colorAttachments[i].pixelFormat = cf[i];
			pd.colorAttachments[i].writeMask = doColor ? MTLColorWriteMaskAll : MTLColorWriteMaskNone;
		}
		pd.depthAttachmentPixelFormat = df;
		pd.stencilAttachmentPixelFormat = sf;
		NSError *err = nil;
		pso = [mtl newRenderPipelineStateWithDescriptor:pd error:&err];
		if (!pso) Log("clear pipeline failed: %s", err.localizedDescription.UTF8String);
		blitPipelines[h] = pso;
	}
	if (!pso) return;
	[enc setRenderPipelineState:pso];
	MTLDepthStencilDescriptor *dd = [MTLDepthStencilDescriptor new];
	dd.depthCompareFunction = MTLCompareFunctionAlways;
	dd.depthWriteEnabled = doDepth;
	if (doStencil) {
		MTLStencilDescriptor *s = [MTLStencilDescriptor new];
		s.stencilCompareFunction = MTLCompareFunctionAlways;
		s.depthStencilPassOperation = MTLStencilOperationReplace;
		s.writeMask = 0xFF;
		dd.frontFaceStencil = dd.backFaceStencil = s;
	}
	[enc setDepthStencilState:[mtl newDepthStencilStateWithDescriptor:dd]];
	[enc setStencilReferenceValue:stencil & 0xFF];
	[enc setCullMode:MTLCullModeNone];
	[enc setTriangleFillMode:MTLTriangleFillModeFill];
	[enc setDepthBias:0 slopeScale:0 clamp:0];

	NSUInteger tw = 1, th = 1;
	if (Surface *s0 = SurfImpl(rt[0])) { tw = s0->tex->LevelWidth(s0->level); th = s0->tex->LevelHeight(s0->level); }
	else if (Surface *zz = SurfImpl(ds)) { tw = zz->tex->width; th = zz->tex->height; }
	MTLViewport v = {0, 0, (double)tw, (double)th, 0, 1};
	[enc setViewport:v];
	LONG l = viewport.X, t = viewport.Y, r = viewport.X + viewport.Width, b = viewport.Y + viewport.Height;
	if (rect) {
		l = std::max<LONG>(l, rect->x1); t = std::max<LONG>(t, rect->y1);
		r = std::min<LONG>(r, rect->x2); b = std::min<LONG>(b, rect->y2);
	}
	l = std::max<LONG>(0, l); t = std::max<LONG>(0, t);
	r = std::min<LONG>((LONG)tw, r); b = std::min<LONG>((LONG)th, b);
	if (r <= l || b <= t) return;
	MTLScissorRect sr = {(NSUInteger)l, (NSUInteger)t, (NSUInteger)(r - l), (NSUInteger)(b - t)};
	[enc setScissorRect:sr];
	struct { float src[4]; float dst[4]; float depth; float pad[3]; } bp = {{0, 0, 1, 1}, {0, 0, 1, 1}, z, {0, 0, 0}};
	[enc setVertexBytes:&bp length:sizeof(bp) atIndex:0];
	float c[4] = {((color >> 16) & 255) / 255.0f, ((color >> 8) & 255) / 255.0f, (color & 255) / 255.0f, ((color >> 24) & 255) / 255.0f};
	[enc setFragmentBytes:c length:sizeof(c) atIndex:0];
	[enc drawPrimitives:MTLPrimitiveTypeTriangleStrip vertexStart:0 vertexCount:4];
	passDirty = false;
}

id<MTLRenderPipelineState> Device::OverlayPipeline(NSString *fragment)
{
	MTLPixelFormat cf[4], df, sf;
	NSUInteger samples;
	PassFormats(this, cf, df, sf, samples);
	uint64_t h = Mix(Mix(0x0EE1ull, (uint64_t)fragment.hash), samples);
	for (int i = 0; i < 4; i++) h = Mix(h, cf[i]);
	h = Mix(Mix(h, df), sf);
	auto it = blitPipelines.find(h);
	if (it != blitPipelines.end()) return it->second;
	MTLRenderPipelineDescriptor *pd = [MTLRenderPipelineDescriptor new];
	pd.vertexFunction = [utilLib newFunctionWithName:@"BlitVSQuad"];
	pd.fragmentFunction = [utilLib newFunctionWithName:fragment];
	pd.rasterSampleCount = samples;
	for (int i = 0; i < 4; i++) {
		pd.colorAttachments[i].pixelFormat = cf[i];
		if (cf[i] == MTLPixelFormatInvalid) continue;
		pd.colorAttachments[i].writeMask = i == 0 ? MTLColorWriteMaskAll : MTLColorWriteMaskNone;
		pd.colorAttachments[i].blendingEnabled = YES;
		pd.colorAttachments[i].sourceRGBBlendFactor = MTLBlendFactorOne;
		pd.colorAttachments[i].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
		pd.colorAttachments[i].sourceAlphaBlendFactor = MTLBlendFactorOne;
		pd.colorAttachments[i].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
	}
	pd.depthAttachmentPixelFormat = df;
	pd.stencilAttachmentPixelFormat = sf;
	NSError *err = nil;
	id<MTLRenderPipelineState> pso = [mtl newRenderPipelineStateWithDescriptor:pd error:&err];
	if (!pso) Log("overlay pipeline failed: %s", err.localizedDescription.UTF8String);
	blitPipelines[h] = pso;
	return pso;
}

id<MTLDepthStencilState> Device::NoDepth()
{
	if (!noDepth) {
		MTLDepthStencilDescriptor *dd = [MTLDepthStencilDescriptor new];
		dd.depthCompareFunction = MTLCompareFunctionAlways;
		dd.depthWriteEnabled = NO;
		noDepth = [mtl newDepthStencilStateWithDescriptor:dd];
	}
	return noDepth;
}

void Device::RefreshMSAA(Texture *t)
{
	if (!t->msaa || !t->msaaDirty) return;
	t->msaaDirty = false;
	MTLRenderPassDescriptor *rp = [MTLRenderPassDescriptor renderPassDescriptor];
	rp.colorAttachments[0].texture = t->msaa;
	rp.colorAttachments[0].loadAction = MTLLoadActionDontCare;
	rp.colorAttachments[0].storeAction = MTLStoreActionStore;
	id<MTLRenderCommandEncoder> enc = [Cmd() renderCommandEncoderWithDescriptor:rp];
	id<MTLRenderPipelineState> pso = BlitPipeline(t->fi->pf, false, t->samples);
	if (pso) {
		[enc setRenderPipelineState:pso];
		struct { float src[4]; float dst[4]; float depth; float pad[3]; } bp = {{0, 0, 1, 1}, {0, 0, 1, 1}, 0, {0, 0, 0}};
		[enc setVertexBytes:&bp length:sizeof(bp) atIndex:0];
		[enc setFragmentTexture:t->tex atIndex:0];
		[enc setFragmentSamplerState:blitPoint atIndex:0];
		[enc drawPrimitives:MTLPrimitiveTypeTriangleStrip vertexStart:0 vertexCount:4];
	}
	[enc endEncoding];
}

id<MTLRenderPipelineState> Device::BlitPipeline(MTLPixelFormat fmt, bool depth, NSUInteger samples)
{
	uint64_t h = Mix(Mix(0xB117ull, fmt | ((uint64_t)depth << 32)), samples);
	auto it = blitPipelines.find(h);
	if (it != blitPipelines.end()) return it->second;
	MTLRenderPipelineDescriptor *pd = [MTLRenderPipelineDescriptor new];
	pd.vertexFunction = [utilLib newFunctionWithName:@"BlitVSQuad"];
	pd.fragmentFunction = [utilLib newFunctionWithName:depth ? @"BlitDepthPS" : @"BlitPS"];
	pd.rasterSampleCount = samples;
	if (depth) {
		pd.depthAttachmentPixelFormat = fmt;
		if (fmt == MTLPixelFormatDepth32Float_Stencil8) pd.stencilAttachmentPixelFormat = fmt;
	} else {
		pd.colorAttachments[0].pixelFormat = fmt;
	}
	NSError *err = nil;
	id<MTLRenderPipelineState> pso = [mtl newRenderPipelineStateWithDescriptor:pd error:&err];
	if (!pso) Log("blit pipeline failed: %s", err.localizedDescription.UTF8String);
	blitPipelines[h] = pso;
	return pso;
}

void Device::Blit(Surface *src, const RECT *srcRect, Surface *dst, const RECT *dstRect, bool linear)
{
	Texture *st = src->tex, *dt = dst->tex;
	UINT sw = st->LevelWidth(src->level), sh = st->LevelHeight(src->level);
	UINT dw = dt->LevelWidth(dst->level), dh = dt->LevelHeight(dst->level);
	RECT sr = srcRect ? *srcRect : RECT{0, 0, (LONG)sw, (LONG)sh};
	RECT dr = dstRect ? *dstRect : RECT{0, 0, (LONG)dw, (LONG)dh};
	if (sr.right <= sr.left || sr.bottom <= sr.top || dr.right <= dr.left || dr.bottom <= dr.top) return;

	if (st->IsSystem() || dt->IsSystem()) {
		// CPU side copy between system memory and/or GPU surfaces of equal size
		UINT w = (UINT)(sr.right - sr.left), h = (UINT)(sr.bottom - sr.top);
		UINT pitch = RowPitch(st->fi, w);
		std::vector<uint8_t> tmp((size_t)pitch * RowCount(st->fi, h));
		if (st->IsSystem()) {
			UINT spitch = RowPitch(st->fi, sw);
			UINT rows = RowCount(st->fi, h);
			UINT sy = st->fi->kind == FK_COMPRESSED ? (UINT)sr.top / 4 : (UINT)sr.top;
			UINT sx = st->fi->kind == FK_COMPRESSED ? (UINT)sr.left / 4 : (UINT)sr.left;
			const uint8_t *base = st->sysmem[st->Sub(src->face, src->level)].data();
			for (UINT y = 0; y < rows; y++)
				memcpy(tmp.data() + (size_t)y * pitch, base + (size_t)(sy + y) * spitch + (size_t)sx * st->fi->bytes, pitch);
		} else {
			st->Download(src->face, src->level, tmp.data(), pitch, &sr);
		}
		RECT target = {dr.left, dr.top, dr.left + (LONG)w, dr.top + (LONG)h};
		if (dt->IsSystem()) {
			UINT dpitch = RowPitch(dt->fi, dw);
			UINT rows = RowCount(dt->fi, h);
			UINT dy = dt->fi->kind == FK_COMPRESSED ? (UINT)target.top / 4 : (UINT)target.top;
			UINT dx = dt->fi->kind == FK_COMPRESSED ? (UINT)target.left / 4 : (UINT)target.left;
			uint8_t *base = dt->sysmem[dt->Sub(dst->face, dst->level)].data();
			for (UINT y = 0; y < rows; y++)
				memcpy(base + (size_t)(dy + y) * dpitch + (size_t)dx * dt->fi->bytes, tmp.data() + (size_t)y * pitch, std::min(pitch, dpitch));
		} else {
			dt->Upload(dst->face, dst->level, tmp.data(), pitch, &target);
		}
		return;
	}

	bool sameSize = (sr.right - sr.left) == (dr.right - dr.left) && (sr.bottom - sr.top) == (dr.bottom - dr.top);
	if (sameSize && st->fi->pf == dt->fi->pf) {
		if (dt->msaa) dt->msaaDirty = true;
		id<MTLTexture> stex = st->tex, dtex = dt->tex;
		NSUInteger ss = st->type == TT_CUBE ? src->face : 0, sl = src->level, ds = dt->type == TT_CUBE ? dst->face : 0, dl = dst->level;
		EncodeBlit(^(id<MTLBlitCommandEncoder> b) {
			[b copyFromTexture:stex sourceSlice:ss sourceLevel:sl
				sourceOrigin:MTLOriginMake(sr.left, sr.top, 0) sourceSize:MTLSizeMake(sr.right - sr.left, sr.bottom - sr.top, 1)
				toTexture:dtex destinationSlice:ds destinationLevel:dl destinationOrigin:MTLOriginMake(dr.left, dr.top, 0)];
		});
		return;
	}
	if (st->fi->kind == FK_COMPRESSED || dt->fi->kind == FK_COMPRESSED) return;

	bool depth = dt->IsDepth();
	EndEncoders();
	MTLRenderPassDescriptor *rp = [MTLRenderPassDescriptor renderPassDescriptor];
	id<MTLTexture> target = dt->tex;
	if (dt->msaa && !depth) dt->msaaDirty = true;
	if (depth) {
		rp.depthAttachment.texture = target;
		rp.depthAttachment.loadAction = MTLLoadActionLoad;
		rp.depthAttachment.storeAction = MTLStoreActionStore;
	} else {
		rp.colorAttachments[0].texture = target;
		rp.colorAttachments[0].slice = dt->type == TT_CUBE ? dst->face : 0;
		rp.colorAttachments[0].level = dst->level;
		rp.colorAttachments[0].loadAction = MTLLoadActionLoad;
		rp.colorAttachments[0].storeAction = MTLStoreActionStore;
	}
	id<MTLRenderCommandEncoder> enc = [Cmd() renderCommandEncoderWithDescriptor:rp];
	id<MTLRenderPipelineState> pso = BlitPipeline(dt->fi->pf, depth);
	if (pso && !(depth && dt->msaa)) {
		[enc setRenderPipelineState:pso];
		if (depth) {
			MTLDepthStencilDescriptor *dd = [MTLDepthStencilDescriptor new];
			dd.depthCompareFunction = MTLCompareFunctionAlways;
			dd.depthWriteEnabled = YES;
			[enc setDepthStencilState:[mtl newDepthStencilStateWithDescriptor:dd]];
		}
		float fsw = (float)sw, fsh = (float)sh, fdw = (float)dw, fdh = (float)dh;
		struct { float src[4]; float dst[4]; float depth; float pad[3]; } bp = {
			{sr.left / fsw, sr.top / fsh, sr.right / fsw, sr.bottom / fsh},
			{dr.left / fdw, dr.top / fdh, dr.right / fdw, dr.bottom / fdh}, 0, {0, 0, 0}};
		[enc setVertexBytes:&bp length:sizeof(bp) atIndex:0];
		id<MTLTexture> s = st->tex;
		if (st->type == TT_CUBE || src->level)
			s = [st->tex newTextureViewWithPixelFormat:st->fi->pf textureType:MTLTextureType2D levels:NSMakeRange(src->level, 1) slices:NSMakeRange(st->type == TT_CUBE ? src->face : 0, 1)];
		[enc setFragmentTexture:s atIndex:0];
		[enc setFragmentSamplerState:linear ? blitLinear : blitPoint atIndex:0];
		[enc drawPrimitives:MTLPrimitiveTypeTriangleStrip vertexStart:0 vertexCount:4];
	}
	[enc endEncoding];
}

// ---------------------------------------------------------------------------
// Swap chains

bool SwapChain::Create()
{
	layer = (__bridge CAMetalLayer *)oplat::GetMetalLayer(hWnd);
	if (!layer) {
		Log("window has no Metal layer");
		return false;
	}
	layer.device = dev->mtl;
	layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
	layer.framebufferOnly = NO;
	layer.displaySyncEnabled = !(pp.PresentationInterval & D3DPRESENT_INTERVAL_IMMEDIATE);
	layer.maximumDrawableCount = 3;
	int w, h;
	oplat::GetDrawableSize(hWnd, &w, &h);
	if (pp.BackBufferWidth == 0 || !pp.Windowed) pp.BackBufferWidth = (UINT)w;
	if (pp.BackBufferHeight == 0 || !pp.Windowed) pp.BackBufferHeight = (UINT)h;
	layer.drawableSize = CGSizeMake(pp.BackBufferWidth, pp.BackBufferHeight);
	if (pp.BackBufferFormat == D3DFMT_UNKNOWN) pp.BackBufferFormat = D3DFMT_X8R8G8B8;
	pp.BackBufferCount = 1;

	UINT samples = pp.MultiSampleType >= D3DMULTISAMPLE_2_SAMPLES ? (UINT)pp.MultiSampleType : 1;
	if (samples > 1 && ![dev->mtl supportsTextureSampleCount:samples]) samples = 4;
	Texture *bb = CreateTexture(dev, TT_2D, pp.BackBufferWidth, pp.BackBufferHeight, 1, 1, D3DUSAGE_RENDERTARGET, pp.BackBufferFormat, D3DPOOL_DEFAULT, samples);
	if (!bb) return false;
	bb->isBackBuffer = true;
	IDirect3DSurface9 *s = new IDirect3DSurface9();
	Surface *si = new Surface();
	si->tex = bb;
	si->owner = true;
	si->iface = s;
	s->M9Attach(si);
	bb->iface = nullptr;
	backBuffer = s;
	if (pp.EnableAutoDepthStencil) {
		D3DFORMAT dsf = pp.AutoDepthStencilFormat == D3DFMT_UNKNOWN ? D3DFMT_D24S8 : pp.AutoDepthStencilFormat;
		Texture *dt = CreateTexture(dev, TT_2D, pp.BackBufferWidth, pp.BackBufferHeight, 1, 1, D3DUSAGE_DEPTHSTENCIL, dsf, D3DPOOL_DEFAULT, samples);
		if (dt) {
			IDirect3DSurface9 *z = new IDirect3DSurface9();
			Surface *zi = new Surface();
			zi->tex = dt;
			zi->owner = true;
			zi->iface = z;
			z->M9Attach(zi);
			depthStencil = z;
		}
	}
	return true;
}

void SwapChain::Destroy()
{
	if (backBuffer) {
		backBuffer->Release();
		backBuffer = nullptr;
	}
	if (depthStencil) {
		depthStencil->Release();
		depthStencil = nullptr;
	}
	layer = nil;
}

HRESULT SwapChain::Present()
{
	if (getenv("M9_DRAWLOG")) {
		static int frame = 0;
		frame++;
		if (dev->drawLogActive) {
			for (auto &c : dev->drawLog) Log("DRAW %s %d", c.first.c_str(), c.second);
			dev->drawLogActive = false;
		}
		if (frame == 250) { dev->drawLog.clear(); dev->drawLogActive = true; }
	}
	@autoreleasepool {
		Surface *bs = SurfImpl(backBuffer);
		if (!bs) return D3DERR_INVALIDCALL;
		int w, h;
		oplat::GetDrawableSize(hWnd, &w, &h);
		CGSize want = CGSizeMake(pp.BackBufferWidth, pp.BackBufferHeight);
		if (!CGSizeEqualToSize(layer.drawableSize, want)) layer.drawableSize = want;
		id<CAMetalDrawable> drawable = IsWindowVisible(hWnd) ? [layer nextDrawable] : nil;
		if (!drawable) {
			dev->Submit(false);
			return S_OK;
		}
		dev->EndEncoders();
		id<MTLTexture> src = bs->tex->tex;
		if (src.width == drawable.texture.width && src.height == drawable.texture.height && src.pixelFormat == drawable.texture.pixelFormat) {
			id<MTLBlitCommandEncoder> b = dev->BlitEncoder();
			[b copyFromTexture:src toTexture:drawable.texture];
			dev->EndEncoders();
		} else {
			MTLRenderPassDescriptor *rp = [MTLRenderPassDescriptor renderPassDescriptor];
			rp.colorAttachments[0].texture = drawable.texture;
			rp.colorAttachments[0].loadAction = MTLLoadActionDontCare;
			rp.colorAttachments[0].storeAction = MTLStoreActionStore;
			id<MTLRenderCommandEncoder> enc = [dev->Cmd() renderCommandEncoderWithDescriptor:rp];
			[enc setRenderPipelineState:dev->BlitPipeline(drawable.texture.pixelFormat, false)];
			struct { float src[4]; float dst[4]; float depth; float pad[3]; } bp = {{0, 0, 1, 1}, {0, 0, 1, 1}, 0, {0, 0, 0}};
			[enc setVertexBytes:&bp length:sizeof(bp) atIndex:0];
			[enc setFragmentTexture:src atIndex:0];
			[enc setFragmentSamplerState:dev->blitLinear atIndex:0];
			[enc drawPrimitives:MTLPrimitiveTypeTriangleStrip vertexStart:0 vertexCount:4];
			[enc endEncoding];
		}
		[dev->Cmd() presentDrawable:drawable];
		dev->Submit(false);
	}
	return S_OK;
}

}
