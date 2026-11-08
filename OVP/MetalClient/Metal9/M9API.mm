#include "M9Internal.h"
#include "OrbiterPlatform.h"
#import <CoreGraphics/CoreGraphics.h>
#include <algorithm>

using namespace m9;


static void GetModes(std::vector<D3DDISPLAYMODE> &out)
{
	out.clear();
	CFArrayRef modes = CGDisplayCopyAllDisplayModes(CGMainDisplayID(), nullptr);
	if (!modes) return;
	for (CFIndex i = 0; i < CFArrayGetCount(modes); i++) {
		CGDisplayModeRef m = (CGDisplayModeRef)CFArrayGetValueAtIndex(modes, i);
		D3DDISPLAYMODE dm = {(UINT)CGDisplayModeGetPixelWidth(m), (UINT)CGDisplayModeGetPixelHeight(m), (UINT)CGDisplayModeGetRefreshRate(m), D3DFMT_X8R8G8B8};
		if (!dm.RefreshRate) dm.RefreshRate = 60;
		bool dup = false;
		for (auto &o : out) dup |= (o.Width == dm.Width && o.Height == dm.Height && o.RefreshRate == dm.RefreshRate);
		if (!dup) out.push_back(dm);
	}
	CFRelease(modes);
	std::sort(out.begin(), out.end(), [](const D3DDISPLAYMODE &a, const D3DDISPLAYMODE &b) {
		return a.Width != b.Width ? a.Width < b.Width : a.Height != b.Height ? a.Height < b.Height : a.RefreshRate < b.RefreshRate;
	});
}

IDirect3D9 *Direct3DCreate9(UINT)
{
	if (!MTLCreateSystemDefaultDevice()) return nullptr;
	return new IDirect3D9();
}

UINT IDirect3D9::GetAdapterCount() { return 1; }

HRESULT IDirect3D9::GetAdapterIdentifier(UINT adapter, DWORD, D3DADAPTER_IDENTIFIER9 *ai)
{
	if (adapter || !ai) return D3DERR_INVALIDCALL;
	memset(ai, 0, sizeof(*ai));
	id<MTLDevice> d = MTLCreateSystemDefaultDevice();
	strncpy(ai->Description, d.name.UTF8String, sizeof(ai->Description) - 1);
	strncpy(ai->Driver, "Metal", sizeof(ai->Driver) - 1);
	strncpy(ai->DeviceName, "Display", sizeof(ai->DeviceName) - 1);
	ai->VendorId = 0x106B;
	ai->DeviceId = (DWORD)d.registryID;
	return S_OK;
}

UINT IDirect3D9::GetAdapterModeCount(UINT adapter, D3DFORMAT)
{
	std::vector<D3DDISPLAYMODE> m;
	GetModes(m);
	return adapter ? 0 : (UINT)m.size();
}

HRESULT IDirect3D9::EnumAdapterModes(UINT adapter, D3DFORMAT, UINT mode, D3DDISPLAYMODE *dm)
{
	std::vector<D3DDISPLAYMODE> m;
	GetModes(m);
	if (adapter || mode >= m.size() || !dm) return D3DERR_INVALIDCALL;
	*dm = m[mode];
	return S_OK;
}

HRESULT IDirect3D9::GetAdapterDisplayMode(UINT adapter, D3DDISPLAYMODE *dm)
{
	if (adapter || !dm) return D3DERR_INVALIDCALL;
	CGDisplayModeRef m = CGDisplayCopyDisplayMode(CGMainDisplayID());
	dm->Width = (UINT)CGDisplayModeGetPixelWidth(m);
	dm->Height = (UINT)CGDisplayModeGetPixelHeight(m);
	dm->RefreshRate = (UINT)CGDisplayModeGetRefreshRate(m);
	if (!dm->RefreshRate) dm->RefreshRate = 60;
	dm->Format = D3DFMT_X8R8G8B8;
	CGDisplayModeRelease(m);
	return S_OK;
}

HRESULT IDirect3D9::CheckDeviceType(UINT, D3DDEVTYPE, D3DFORMAT, D3DFORMAT, WINBOOL) { return S_OK; }

HRESULT IDirect3D9::CheckDeviceFormat(UINT, D3DDEVTYPE, D3DFORMAT, DWORD usage, D3DRESOURCETYPE, D3DFORMAT fmt)
{
	const FormatInfo *fi = GetFormat(fmt);
	if (!fi) return D3DERR_NOTAVAILABLE;
	if ((usage & D3DUSAGE_RENDERTARGET) && (fi->kind == FK_COMPRESSED || fi->kind == FK_CONVERTED)) return D3DERR_NOTAVAILABLE;
	if ((usage & D3DUSAGE_DEPTHSTENCIL) && fi->kind != FK_DEPTH) return D3DERR_NOTAVAILABLE;
	return S_OK;
}

HRESULT IDirect3D9::CheckDeviceMultiSampleType(UINT, D3DDEVTYPE, D3DFORMAT, WINBOOL, D3DMULTISAMPLE_TYPE ms, DWORD *quality)
{
	id<MTLDevice> d = MTLCreateSystemDefaultDevice();
	UINT n = ms <= D3DMULTISAMPLE_NONMASKABLE ? 1 : (UINT)ms;
	if (quality) *quality = 1;
	return [d supportsTextureSampleCount:n] ? S_OK : D3DERR_NOTAVAILABLE;
}

HRESULT IDirect3D9::CheckDepthStencilMatch(UINT, D3DDEVTYPE, D3DFORMAT, D3DFORMAT, D3DFORMAT) { return S_OK; }

HRESULT IDirect3D9::GetDeviceCaps(UINT, D3DDEVTYPE type, D3DCAPS9 *caps)
{
	if (!caps) return D3DERR_INVALIDCALL;
	memset(caps, 0, sizeof(*caps));
	caps->DeviceType = type;
	caps->Caps2 = D3DCAPS2_CANAUTOGENMIPMAP;
	caps->PresentationIntervals = D3DPRESENT_INTERVAL_ONE | D3DPRESENT_INTERVAL_IMMEDIATE;
	caps->PrimitiveMiscCaps = D3DPMISCCAPS_COLORWRITEENABLE | D3DPMISCCAPS_SEPARATEALPHABLEND | D3DPMISCCAPS_MRTINDEPENDENTBITDEPTHS;
	caps->TextureCaps = 0;
	caps->MaxTextureWidth = caps->MaxTextureHeight = 16384;
	caps->MaxVolumeExtent = 2048;
	caps->MaxTextureRepeat = 8192;
	caps->MaxTextureAspectRatio = 16384;
	caps->MaxAnisotropy = 16;
	caps->MaxSimultaneousTextures = 16;
	caps->MaxTextureBlendStages = 8;
	caps->MaxPointSize = 64.0f;
	caps->MaxPrimitiveCount = 0xFFFFF;
	caps->MaxVertexIndex = 0xFFFFFF;
	caps->MaxStreams = 8;
	caps->MaxStreamStride = 2048;
	caps->VertexShaderVersion = D3DVS_VERSION(3, 0);
	caps->PixelShaderVersion = D3DPS_VERSION(3, 0);
	caps->MaxVertexShaderConst = 256;
	caps->DeclTypes = D3DDTCAPS_UBYTE4 | D3DDTCAPS_UBYTE4N | D3DDTCAPS_SHORT2N | D3DDTCAPS_SHORT4N | D3DDTCAPS_USHORT2N |
		D3DDTCAPS_USHORT4N | D3DDTCAPS_UDEC3 | D3DDTCAPS_DEC3N | D3DDTCAPS_FLOAT16_2 | D3DDTCAPS_FLOAT16_4;
	caps->NumSimultaneousRTs = 4;
	caps->MaxVShaderInstructionsExecuted = caps->MaxPShaderInstructionsExecuted = 65535;
	caps->MaxVertexShader30InstructionSlots = caps->MaxPixelShader30InstructionSlots = 32768;
	caps->VertexTextureFilterCaps = 0xFFFFFFFF;
	caps->TextureFilterCaps = caps->CubeTextureFilterCaps = caps->VolumeTextureFilterCaps = 0xFFFFFFFF;
	caps->TextureAddressCaps = caps->VolumeTextureAddressCaps = 0x3F;
	caps->StencilCaps = 0xFF;
	caps->ZCmpCaps = caps->AlphaCmpCaps = 0xFF;
	caps->SrcBlendCaps = caps->DestBlendCaps = 0xFFFF;
	caps->RasterCaps = 0xFFFFFFFF;
	caps->DevCaps = 0xFFFFFFFF;
	return S_OK;
}

HRESULT IDirect3D9::CreateDevice(UINT, D3DDEVTYPE, HWND hFocus, DWORD flags, D3DPRESENT_PARAMETERS *pp, IDirect3DDevice9 **dev)
{
	if (!pp || !dev) return D3DERR_INVALIDCALL;
	Device *d = new Device();
	d->d3d = this;
	d->behaviour = flags;
	if (!d->Init(hFocus, pp)) {
		delete d;
		return D3DERR_NOTAVAILABLE;
	}
	IDirect3DDevice9 *iface = new IDirect3DDevice9();
	iface->m_dev = d;
	d->iface = iface;
	*dev = iface;
	return S_OK;
}

// ---------------------------------------------------------------------------
// Device

IDirect3DDevice9::~IDirect3DDevice9() { delete m_dev; }

HRESULT IDirect3DDevice9::TestCooperativeLevel() { return S_OK; }

UINT IDirect3DDevice9::GetAvailableTextureMem()
{
	uint64_t total = m_dev->mtl.recommendedMaxWorkingSetSize;
	uint64_t used = m_dev->mtl.currentAllocatedSize;
	uint64_t avail = total > used ? total - used : 0;
	return (UINT)std::min<uint64_t>(avail, 0xFFF00000ull);
}

HRESULT IDirect3DDevice9::GetDirect3D(IDirect3D9 **d3d)
{
	if (!d3d) return D3DERR_INVALIDCALL;
	*d3d = m_dev->d3d;
	m_dev->d3d->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::GetDeviceCaps(D3DCAPS9 *caps) { return m_dev->d3d->GetDeviceCaps(0, D3DDEVTYPE_HAL, caps); }

HRESULT IDirect3DDevice9::GetDisplayMode(UINT, D3DDISPLAYMODE *mode) { return m_dev->d3d->GetAdapterDisplayMode(0, mode); }

HRESULT IDirect3DDevice9::CreateAdditionalSwapChain(D3DPRESENT_PARAMETERS *pp, IDirect3DSwapChain9 **chain)
{
	if (!pp || !chain || !pp->hDeviceWindow) return D3DERR_INVALIDCALL;
	SwapChain *sc = new SwapChain();
	sc->dev = m_dev;
	sc->hWnd = pp->hDeviceWindow;
	sc->pp = *pp;
	sc->pp.Windowed = TRUE;
	if (!sc->Create()) {
		delete sc;
		return D3DERR_INVALIDCALL;
	}
	*pp = sc->pp;
	m_dev->chains.push_back(sc);
	IDirect3DSwapChain9 *iface = new IDirect3DSwapChain9();
	iface->m_chain = sc;
	*chain = iface;
	return S_OK;
}

HRESULT IDirect3DDevice9::GetSwapChain(UINT idx, IDirect3DSwapChain9 **chain)
{
	if (idx >= m_dev->chains.size() || !chain) return D3DERR_INVALIDCALL;
	IDirect3DSwapChain9 *iface = new IDirect3DSwapChain9();
	iface->m_chain = m_dev->chains[idx];
	iface->m_owned = false;
	*chain = iface;
	return S_OK;
}

IDirect3DSwapChain9::~IDirect3DSwapChain9()
{
	if (!m_chain || !m_owned) return;
	Device *d = m_chain->dev;
	auto &v = d->chains;
	v.erase(std::remove(v.begin(), v.end(), m_chain), v.end());
	d->WaitIdle();
	m_chain->Destroy();
	delete m_chain;
}

HRESULT IDirect3DSwapChain9::Present(const RECT *, const RECT *, HWND, const void *, DWORD)
{
	return m_chain ? m_chain->Present() : D3DERR_INVALIDCALL;
}

HRESULT IDirect3DSwapChain9::GetBackBuffer(UINT idx, D3DBACKBUFFER_TYPE, IDirect3DSurface9 **surf)
{
	if (!m_chain || idx || !surf) return D3DERR_INVALIDCALL;
	*surf = m_chain->backBuffer;
	(*surf)->AddRef();
	return S_OK;
}

HRESULT IDirect3DSwapChain9::GetPresentParameters(D3DPRESENT_PARAMETERS *pp)
{
	if (!m_chain || !pp) return D3DERR_INVALIDCALL;
	*pp = m_chain->pp;
	return S_OK;
}

HRESULT IDirect3DSwapChain9::GetDisplayMode(D3DDISPLAYMODE *mode)
{
	return m_chain ? m_chain->dev->d3d->GetAdapterDisplayMode(0, mode) : D3DERR_INVALIDCALL;
}

HRESULT IDirect3DDevice9::Reset(D3DPRESENT_PARAMETERS *pp)
{
	if (!pp || m_dev->chains.empty()) return D3DERR_INVALIDCALL;
	m_dev->WaitIdle();
	SwapChain *sc = m_dev->chains[0];
	for (int i = 0; i < 4; i++) {
		if (m_dev->rt[i]) m_dev->rt[i]->Release();
		m_dev->rt[i] = nullptr;
	}
	if (m_dev->ds) m_dev->ds->Release();
	m_dev->ds = nullptr;
	sc->Destroy();
	sc->pp = *pp;
	if (!sc->pp.hDeviceWindow) sc->pp.hDeviceWindow = sc->hWnd;
	if (!sc->Create()) return D3DERR_DEVICELOST;
	*pp = sc->pp;
	m_dev->pp = sc->pp;
	m_dev->SetDefaults();
	m_dev->rt[0] = sc->backBuffer;
	m_dev->rt[0]->AddRef();
	m_dev->ds = sc->depthStencil;
	if (m_dev->ds) m_dev->ds->AddRef();
	m_dev->viewport = {0, 0, sc->pp.BackBufferWidth, sc->pp.BackBufferHeight, 0.0f, 1.0f};
	m_dev->scissor = {0, 0, (LONG)sc->pp.BackBufferWidth, (LONG)sc->pp.BackBufferHeight};
	m_dev->passDirty = true;
	return S_OK;
}

HRESULT IDirect3DDevice9::Present(const RECT *, const RECT *, HWND, const void *)
{
	if (m_dev->chains.empty()) return D3DERR_INVALIDCALL;
	return m_dev->chains[0]->Present();
}

HRESULT IDirect3DDevice9::GetBackBuffer(UINT swap, UINT idx, D3DBACKBUFFER_TYPE, IDirect3DSurface9 **surf)
{
	if (swap >= m_dev->chains.size() || idx || !surf) return D3DERR_INVALIDCALL;
	*surf = m_dev->chains[swap]->backBuffer;
	(*surf)->AddRef();
	return S_OK;
}

template<typename T>
static HRESULT WrapTexture(Texture *t, T **out)
{
	if (!t) return D3DERR_OUTOFVIDEOMEMORY;
	T *iface = new T();
	t->iface = iface;
	iface->M9Attach(t);
	*out = iface;
	return S_OK;
}

HRESULT IDirect3DDevice9::CreateTexture(UINT w, UINT h, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DTexture9 **tex, HANDLE *)
{
	if (!tex) return D3DERR_INVALIDCALL;
	return WrapTexture(m9::CreateTexture(m_dev, TT_2D, w, h, 1, levels, usage, fmt, pool, 1), tex);
}

HRESULT IDirect3DDevice9::CreateVolumeTexture(UINT w, UINT h, UINT d, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DVolumeTexture9 **tex, HANDLE *)
{
	if (!tex) return D3DERR_INVALIDCALL;
	return WrapTexture(m9::CreateTexture(m_dev, TT_3D, w, h, d, levels, usage, fmt, pool, 1), tex);
}

HRESULT IDirect3DDevice9::CreateCubeTexture(UINT edge, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DCubeTexture9 **tex, HANDLE *)
{
	if (!tex) return D3DERR_INVALIDCALL;
	return WrapTexture(m9::CreateTexture(m_dev, TT_CUBE, edge, edge, 1, levels, usage, fmt, pool, 1), tex);
}

static Buffer *NewBuffer(Device *d, UINT length, DWORD usage, D3DPOOL pool)
{
	Buffer *b = new Buffer();
	b->dev = d;
	b->size = length;
	b->usage = usage;
	b->pool = pool;
	b->buf = [d->mtl newBufferWithLength:std::max(4u, length) options:MTLResourceStorageModeShared];
	if (!b->buf) {
		delete b;
		return nullptr;
	}
	return b;
}

HRESULT IDirect3DDevice9::CreateVertexBuffer(UINT length, DWORD usage, DWORD fvf, D3DPOOL pool, IDirect3DVertexBuffer9 **vb, HANDLE *)
{
	if (!vb || !length) return D3DERR_INVALIDCALL;
	Buffer *b = NewBuffer(m_dev, length, usage, pool);
	if (!b) return D3DERR_OUTOFVIDEOMEMORY;
	b->fvf = fvf;
	IDirect3DVertexBuffer9 *iface = new IDirect3DVertexBuffer9();
	iface->m_buf = b;
	*vb = iface;
	return S_OK;
}

HRESULT IDirect3DDevice9::CreateIndexBuffer(UINT length, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DIndexBuffer9 **ib, HANDLE *)
{
	if (!ib || !length) return D3DERR_INVALIDCALL;
	Buffer *b = NewBuffer(m_dev, length, usage, pool);
	if (!b) return D3DERR_OUTOFVIDEOMEMORY;
	b->fmt = fmt;
	IDirect3DIndexBuffer9 *iface = new IDirect3DIndexBuffer9();
	iface->m_buf = b;
	*ib = iface;
	return S_OK;
}

static HRESULT StandaloneSurface(Device *d, UINT w, UINT h, D3DFORMAT fmt, DWORD usage, D3DPOOL pool, UINT samples, IDirect3DSurface9 **surf)
{
	if (!surf) return D3DERR_INVALIDCALL;
	Texture *t = m9::CreateTexture(d, TT_2D, w, h, 1, 1, usage, fmt, pool, samples);
	if (!t) return D3DERR_OUTOFVIDEOMEMORY;
	IDirect3DSurface9 *s = new IDirect3DSurface9();
	Surface *si = new Surface();
	si->tex = t;
	si->owner = true;
	si->iface = s;
	s->M9Attach(si);
	*surf = s;
	return S_OK;
}

HRESULT IDirect3DDevice9::CreateRenderTarget(UINT w, UINT h, D3DFORMAT fmt, D3DMULTISAMPLE_TYPE ms, DWORD, WINBOOL, IDirect3DSurface9 **surf, HANDLE *)
{
	UINT samples = ms >= D3DMULTISAMPLE_2_SAMPLES ? (UINT)ms : 1;
	if (samples > 1 && ![m_dev->mtl supportsTextureSampleCount:samples]) samples = 4;
	return StandaloneSurface(m_dev, w, h, fmt, D3DUSAGE_RENDERTARGET, D3DPOOL_DEFAULT, samples, surf);
}

HRESULT IDirect3DDevice9::CreateDepthStencilSurface(UINT w, UINT h, D3DFORMAT fmt, D3DMULTISAMPLE_TYPE ms, DWORD, WINBOOL, IDirect3DSurface9 **surf, HANDLE *)
{
	UINT samples = ms >= D3DMULTISAMPLE_2_SAMPLES ? (UINT)ms : 1;
	if (samples > 1 && ![m_dev->mtl supportsTextureSampleCount:samples]) samples = 4;
	return StandaloneSurface(m_dev, w, h, fmt, D3DUSAGE_DEPTHSTENCIL, D3DPOOL_DEFAULT, samples, surf);
}

HRESULT IDirect3DDevice9::CreateOffscreenPlainSurface(UINT w, UINT h, D3DFORMAT fmt, D3DPOOL pool, IDirect3DSurface9 **surf, HANDLE *)
{
	return StandaloneSurface(m_dev, w, h, fmt, 0, pool, 1, surf);
}

HRESULT IDirect3DDevice9::UpdateSurface(IDirect3DSurface9 *src, const RECT *srcRect, IDirect3DSurface9 *dst, const POINT *dstPoint)
{
	Surface *s = SurfImpl(src), *d = SurfImpl(dst);
	if (!s || !d) return D3DERR_INVALIDCALL;
	RECT sr = srcRect ? *srcRect : RECT{0, 0, (LONG)s->tex->LevelWidth(s->level), (LONG)s->tex->LevelHeight(s->level)};
	RECT dr = {dstPoint ? dstPoint->x : 0, dstPoint ? dstPoint->y : 0, 0, 0};
	dr.right = dr.left + (sr.right - sr.left);
	dr.bottom = dr.top + (sr.bottom - sr.top);
	m_dev->Blit(s, &sr, d, &dr, false);
	return S_OK;
}

HRESULT IDirect3DDevice9::UpdateTexture(IDirect3DBaseTexture9 *src, IDirect3DBaseTexture9 *dst)
{
	if (!src || !dst || !src->Impl() || !dst->Impl()) return D3DERR_INVALIDCALL;
	Texture *s = src->Impl(), *d = dst->Impl();
	UINT faces = std::min(s->Faces(), d->Faces());
	UINT offset = 0;
	while (offset < s->levels && s->LevelWidth(offset) > d->width) offset++;
	for (UINT f = 0; f < faces; f++) {
		for (UINT l = 0; l < d->levels && l + offset < s->levels; l++) {
			Surface *ss = s->GetSurface(f, l + offset)->Impl();
			Surface *ds = d->GetSurface(f, l)->Impl();
			if (s->type == TT_3D) {
				if (s->IsSystem()) {
					UINT pitch = RowPitch(s->fi, s->LevelWidth(l + offset));
					d->Upload(0, l, s->sysmem[l + offset].data(), pitch, nullptr, 0, s->LevelDepth(l + offset));
				}
			} else {
				m_dev->Blit(ss, nullptr, ds, nullptr, false);
			}
		}
	}
	if (d->usage & D3DUSAGE_AUTOGENMIPMAP) dst->GenerateMipSubLevels();
	return S_OK;
}

HRESULT IDirect3DDevice9::GetRenderTargetData(IDirect3DSurface9 *rt, IDirect3DSurface9 *dst)
{
	Surface *s = SurfImpl(rt), *d = SurfImpl(dst);
	if (!s || !d) return D3DERR_INVALIDCALL;
	m_dev->Blit(s, nullptr, d, nullptr, false);
	return S_OK;
}

HRESULT IDirect3DDevice9::StretchRect(IDirect3DSurface9 *src, const RECT *srcRect, IDirect3DSurface9 *dst, const RECT *dstRect, D3DTEXTUREFILTERTYPE filter)
{
	Surface *s = SurfImpl(src), *d = SurfImpl(dst);
	if (!s || !d) return D3DERR_INVALIDCALL;
	m_dev->Blit(s, srcRect, d, dstRect, filter != D3DTEXF_POINT && filter != D3DTEXF_NONE);
	return S_OK;
}

HRESULT IDirect3DDevice9::ColorFill(IDirect3DSurface9 *surf, const RECT *rect, D3DCOLOR color)
{
	Surface *s = SurfImpl(surf);
	if (!s) return D3DERR_INVALIDCALL;
	Texture *t = s->tex;
	if (t->IsSystem()) {
		UINT w = t->LevelWidth(s->level), h = t->LevelHeight(s->level);
		RECT r = rect ? *rect : RECT{0, 0, (LONG)w, (LONG)h};
		UINT pitch = RowPitch(t->fi, w);
		uint8_t *base = t->sysmem[t->Sub(s->face, s->level)].data();
		for (LONG y = r.top; y < r.bottom; y++)
			for (LONG x = r.left; x < r.right; x++)
				if (t->fi->bytes == 4) ((uint32_t *)(base + (size_t)y * pitch))[x] = color;
		return S_OK;
	}
	IDirect3DSurface9 *saveRT[4], *saveDS = m_dev->ds;
	for (int i = 0; i < 4; i++) saveRT[i] = m_dev->rt[i];
	D3DVIEWPORT9 saveVP = m_dev->viewport;
	m_dev->rt[0] = surf;
	m_dev->rt[1] = m_dev->rt[2] = m_dev->rt[3] = nullptr;
	m_dev->ds = nullptr;
	m_dev->viewport = {0, 0, t->LevelWidth(s->level), t->LevelHeight(s->level), 0, 1};
	m_dev->passDirty = true;
	D3DRECT dr;
	if (rect) dr = {rect->left, rect->top, rect->right, rect->bottom};
	m_dev->ClearInPass(D3DCLEAR_TARGET, color, 1.0f, 0, rect ? &dr : nullptr);
	for (int i = 0; i < 4; i++) m_dev->rt[i] = saveRT[i];
	m_dev->ds = saveDS;
	m_dev->viewport = saveVP;
	m_dev->passDirty = true;
	return S_OK;
}

HRESULT IDirect3DDevice9::SetRenderTarget(DWORD idx, IDirect3DSurface9 *surf)
{
	if (idx >= 4 || (idx == 0 && !surf)) return D3DERR_INVALIDCALL;
	if (m_dev->rt[idx] == surf) return S_OK;
	if (surf) surf->AddRef();
	if (m_dev->rt[idx]) m_dev->rt[idx]->Release();
	m_dev->rt[idx] = surf;
	m_dev->passDirty = true;
	if (idx == 0) {
		Surface *s = surf->Impl();
		UINT w = s->tex->LevelWidth(s->level), h = s->tex->LevelHeight(s->level);
		m_dev->viewport = {0, 0, w, h, 0.0f, 1.0f};
		m_dev->scissor = {0, 0, (LONG)w, (LONG)h};
	}
	return S_OK;
}

HRESULT IDirect3DDevice9::GetRenderTarget(DWORD idx, IDirect3DSurface9 **surf)
{
	if (idx >= 4 || !surf) return D3DERR_INVALIDCALL;
	*surf = m_dev->rt[idx];
	if (!*surf) return D3DERR_NOTFOUND;
	(*surf)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::SetDepthStencilSurface(IDirect3DSurface9 *surf)
{
	if (m_dev->ds == surf) return S_OK;
	if (surf) surf->AddRef();
	if (m_dev->ds) m_dev->ds->Release();
	m_dev->ds = surf;
	m_dev->passDirty = true;
	return S_OK;
}

HRESULT IDirect3DDevice9::GetDepthStencilSurface(IDirect3DSurface9 **surf)
{
	if (!surf) return D3DERR_INVALIDCALL;
	*surf = m_dev->ds;
	if (!*surf) return D3DERR_NOTFOUND;
	(*surf)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::BeginScene()
{
	m_dev->Cmd();
	return S_OK;
}

HRESULT IDirect3DDevice9::EndScene() { return S_OK; }

HRESULT IDirect3DDevice9::Clear(DWORD count, const D3DRECT *rects, DWORD flags, D3DCOLOR color, float z, DWORD stencil)
{
	Device *d = m_dev;
	Surface *s0 = SurfImpl(d->rt[0]);
	UINT tw = s0 ? s0->tex->LevelWidth(s0->level) : 0, th = s0 ? s0->tex->LevelHeight(s0->level) : 0;
	bool full = (count == 0 || !rects) && d->viewport.X == 0 && d->viewport.Y == 0 &&
		(!s0 || (d->viewport.Width >= tw && d->viewport.Height >= th));
	if (full && (!d->renc || d->passDirty)) {
		if (d->renc) d->EndEncoders();
		d->pendingClear = true;
		d->clearFlags = flags;
		d->clearColor = color;
		d->clearZ = z;
		d->clearStencil = stencil;
		d->passDirty = true;
		d->RenderEncoder();
		return S_OK;
	}
	if (count && rects) {
		for (DWORD i = 0; i < count; i++) d->ClearInPass(flags, color, z, stencil, &rects[i]);
	} else {
		d->ClearInPass(flags, color, z, stencil, nullptr);
	}
	return S_OK;
}

HRESULT IDirect3DDevice9::SetViewport(const D3DVIEWPORT9 *vp)
{
	if (!vp) return D3DERR_INVALIDCALL;
	m_dev->viewport = *vp;
	return S_OK;
}

HRESULT IDirect3DDevice9::GetViewport(D3DVIEWPORT9 *vp)
{
	if (!vp) return D3DERR_INVALIDCALL;
	*vp = m_dev->viewport;
	return S_OK;
}

HRESULT IDirect3DDevice9::SetRenderState(D3DRENDERSTATETYPE state, DWORD value)
{
	if ((UINT)state >= D3DRS_COUNT) return D3DERR_INVALIDCALL;
	m_dev->rs[state] = value;
	return S_OK;
}

HRESULT IDirect3DDevice9::GetRenderState(D3DRENDERSTATETYPE state, DWORD *value)
{
	if ((UINT)state >= D3DRS_COUNT || !value) return D3DERR_INVALIDCALL;
	*value = m_dev->rs[state];
	return S_OK;
}

static int StageIndex(DWORD stage)
{
	if (stage < 16) return (int)stage;
	if (stage >= D3DVERTEXTEXTURESAMPLER0 && stage <= D3DVERTEXTEXTURESAMPLER3) return 16 + (int)(stage - D3DVERTEXTEXTURESAMPLER0);
	return -1;
}

HRESULT IDirect3DDevice9::GetTexture(DWORD stage, IDirect3DBaseTexture9 **tex)
{
	int i = StageIndex(stage);
	if (i < 0 || !tex) return D3DERR_INVALIDCALL;
	*tex = m_dev->textures[i];
	if (*tex) (*tex)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::SetTexture(DWORD stage, IDirect3DBaseTexture9 *tex)
{
	int i = StageIndex(stage);
	if (i < 0) return D3DERR_INVALIDCALL;
	Rebind(m_dev->textures[i], tex);
	return S_OK;
}

HRESULT IDirect3DDevice9::GetSamplerState(DWORD sampler, D3DSAMPLERSTATETYPE type, DWORD *value)
{
	int i = StageIndex(sampler);
	if (i < 0 || (UINT)type >= D3DSAMP_COUNT || !value) return D3DERR_INVALIDCALL;
	*value = m_dev->ss[i][type];
	return S_OK;
}

HRESULT IDirect3DDevice9::SetSamplerState(DWORD sampler, D3DSAMPLERSTATETYPE type, DWORD value)
{
	int i = StageIndex(sampler);
	if (i < 0 || (UINT)type >= D3DSAMP_COUNT) return D3DERR_INVALIDCALL;
	m_dev->ss[i][type] = value;
	return S_OK;
}

HRESULT IDirect3DDevice9::SetScissorRect(const RECT *rect)
{
	if (!rect) return D3DERR_INVALIDCALL;
	m_dev->scissor = *rect;
	return S_OK;
}

HRESULT IDirect3DDevice9::GetScissorRect(RECT *rect)
{
	if (!rect) return D3DERR_INVALIDCALL;
	*rect = m_dev->scissor;
	return S_OK;
}

static UINT VertexCount(D3DPRIMITIVETYPE type, UINT prims)
{
	switch (type) {
	case D3DPT_POINTLIST: return prims;
	case D3DPT_LINELIST: return prims * 2;
	case D3DPT_LINESTRIP: return prims + 1;
	case D3DPT_TRIANGLELIST: return prims * 3;
	case D3DPT_TRIANGLESTRIP:
	case D3DPT_TRIANGLEFAN: return prims + 2;
	default: return 0;
	}
}

HRESULT IDirect3DDevice9::DrawPrimitive(D3DPRIMITIVETYPE type, UINT start, UINT count)
{
	return m_dev->Draw(type, start, VertexCount(type, count), nullptr, 0, nullptr, nullptr, D3DFMT_UNKNOWN, 0, 0, 0);
}

HRESULT IDirect3DDevice9::DrawIndexedPrimitive(D3DPRIMITIVETYPE type, INT baseVertex, UINT, UINT, UINT startIndex, UINT primCount)
{
	if (!m_dev->indices) return D3DERR_INVALIDCALL;
	return m_dev->Draw(type, 0, 0, nullptr, 0, m_dev->indices, nullptr, D3DFMT_UNKNOWN, startIndex, VertexCount(type, primCount), baseVertex);
}

HRESULT IDirect3DDevice9::DrawPrimitiveUP(D3DPRIMITIVETYPE type, UINT primCount, const void *data, UINT stride)
{
	if (!data) return D3DERR_INVALIDCALL;
	HRESULT hr = m_dev->Draw(type, 0, VertexCount(type, primCount), data, stride, nullptr, nullptr, D3DFMT_UNKNOWN, 0, 0, 0);
	Rebind(m_dev->streams[0].vb, (IDirect3DVertexBuffer9 *)nullptr);
	m_dev->streams[0] = {};
	return hr;
}

HRESULT IDirect3DDevice9::DrawIndexedPrimitiveUP(D3DPRIMITIVETYPE type, UINT minIndex, UINT numVertices, UINT primCount,
	const void *indices, D3DFORMAT indexFmt, const void *data, UINT stride)
{
	if (!data || !indices) return D3DERR_INVALIDCALL;
	HRESULT hr = m_dev->Draw(type, 0, minIndex + numVertices, data, stride, nullptr, indices, indexFmt, 0, VertexCount(type, primCount), 0);
	Rebind(m_dev->streams[0].vb, (IDirect3DVertexBuffer9 *)nullptr);
	m_dev->streams[0] = {};
	Rebind(m_dev->indices, (IDirect3DIndexBuffer9 *)nullptr);
	return hr;
}

HRESULT IDirect3DDevice9::CreateVertexDeclaration(const D3DVERTEXELEMENT9 *elements, IDirect3DVertexDeclaration9 **decl)
{
	if (!elements || !decl) return D3DERR_INVALIDCALL;
	Decl *d = new Decl();
	uint64_t h = 1469598103934665603ull;
	for (const D3DVERTEXELEMENT9 *e = elements; e->Stream != 0xFF; e++) {
		d->elems.push_back(*e);
		const uint8_t *p = (const uint8_t *)e;
		for (size_t i = 0; i < sizeof(*e); i++) h = (h ^ p[i]) * 1099511628211ull;
	}
	d->hash = h;
	IDirect3DVertexDeclaration9 *iface = new IDirect3DVertexDeclaration9();
	iface->m_decl = d;
	*decl = iface;
	return S_OK;
}

HRESULT IDirect3DDevice9::SetVertexDeclaration(IDirect3DVertexDeclaration9 *decl)
{
	Rebind(m_dev->decl, decl);
	return S_OK;
}

HRESULT IDirect3DDevice9::GetVertexDeclaration(IDirect3DVertexDeclaration9 **decl)
{
	if (!decl) return D3DERR_INVALIDCALL;
	*decl = m_dev->decl;
	if (*decl) (*decl)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::SetFVF(DWORD) { return S_OK; }

HRESULT IDirect3DDevice9::SetVertexShader(IDirect3DVertexShader9 *shader)
{
	Rebind(m_dev->vs, shader);
	if (shader) {
		m_dev->effect = nullptr;
		m_dev->effectPass = -1;
	}
	return S_OK;
}

HRESULT IDirect3DDevice9::GetVertexShader(IDirect3DVertexShader9 **shader)
{
	if (!shader) return D3DERR_INVALIDCALL;
	*shader = m_dev->vs;
	if (*shader) (*shader)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::SetPixelShader(IDirect3DPixelShader9 *shader)
{
	Rebind(m_dev->ps, shader);
	return S_OK;
}

HRESULT IDirect3DDevice9::GetPixelShader(IDirect3DPixelShader9 **shader)
{
	if (!shader) return D3DERR_INVALIDCALL;
	*shader = m_dev->ps;
	if (*shader) (*shader)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::SetStreamSource(UINT stream, IDirect3DVertexBuffer9 *vb, UINT offset, UINT stride)
{
	if (stream >= 8) return D3DERR_INVALIDCALL;
	Rebind(m_dev->streams[stream].vb, vb);
	m_dev->streams[stream].offset = offset;
	m_dev->streams[stream].stride = stride;
	return S_OK;
}

HRESULT IDirect3DDevice9::GetStreamSource(UINT stream, IDirect3DVertexBuffer9 **vb, UINT *offset, UINT *stride)
{
	if (stream >= 8 || !vb) return D3DERR_INVALIDCALL;
	*vb = m_dev->streams[stream].vb;
	if (*vb) (*vb)->AddRef();
	if (offset) *offset = m_dev->streams[stream].offset;
	if (stride) *stride = m_dev->streams[stream].stride;
	return S_OK;
}

HRESULT IDirect3DDevice9::SetStreamSourceFreq(UINT, UINT) { return S_OK; }

HRESULT IDirect3DDevice9::SetIndices(IDirect3DIndexBuffer9 *ib)
{
	Rebind(m_dev->indices, ib);
	return S_OK;
}

HRESULT IDirect3DDevice9::GetIndices(IDirect3DIndexBuffer9 **ib)
{
	if (!ib) return D3DERR_INVALIDCALL;
	*ib = m_dev->indices;
	if (*ib) (*ib)->AddRef();
	return S_OK;
}

HRESULT IDirect3DDevice9::CreateQuery(D3DQUERYTYPE type, IDirect3DQuery9 **query)
{
	switch (type) {
	case D3DQUERYTYPE_EVENT:
	case D3DQUERYTYPE_OCCLUSION:
	case D3DQUERYTYPE_TIMESTAMP:
	case D3DQUERYTYPE_TIMESTAMPDISJOINT:
	case D3DQUERYTYPE_TIMESTAMPFREQ:
		break;
	default:
		return D3DERR_NOTAVAILABLE;
	}
	if (!query) return S_OK;
	IDirect3DQuery9 *q = new IDirect3DQuery9();
	q->m_type = type;
	q->m_query = new Query();
	q->m_query->type = type;
	*query = q;
	return S_OK;
}

HRESULT IDirect3DDevice9::SetVertexShaderConstantF(UINT, const float *, UINT) { return S_OK; }
HRESULT IDirect3DDevice9::SetPixelShaderConstantF(UINT, const float *, UINT) { return S_OK; }
HRESULT IDirect3DDevice9::SetVertexShaderConstantB(UINT, const WINBOOL *, UINT) { return S_OK; }
HRESULT IDirect3DDevice9::SetPixelShaderConstantB(UINT, const WINBOOL *, UINT) { return S_OK; }
HRESULT IDirect3DDevice9::SetVertexShaderConstantI(UINT, const int *, UINT) { return S_OK; }
HRESULT IDirect3DDevice9::SetPixelShaderConstantI(UINT, const int *, UINT) { return S_OK; }
