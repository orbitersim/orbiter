#include "M9Internal.h"
#include <stdarg.h>
#include <algorithm>

namespace m9 {

void Log(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "[Metal9] ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

// ---------------------------------------------------------------------------
// Formats

static const MTLTextureSwizzleChannels kIdentity = {MTLTextureSwizzleRed, MTLTextureSwizzleGreen, MTLTextureSwizzleBlue, MTLTextureSwizzleAlpha};
static const MTLTextureSwizzleChannels kRGB1 = {MTLTextureSwizzleRed, MTLTextureSwizzleGreen, MTLTextureSwizzleBlue, MTLTextureSwizzleOne};
static const MTLTextureSwizzleChannels kRRR1 = {MTLTextureSwizzleRed, MTLTextureSwizzleRed, MTLTextureSwizzleRed, MTLTextureSwizzleOne};
static const MTLTextureSwizzleChannels kRRRG = {MTLTextureSwizzleRed, MTLTextureSwizzleRed, MTLTextureSwizzleRed, MTLTextureSwizzleGreen};

static const FormatInfo kFormats[] = {
	{D3DFMT_A8R8G8B8, MTLPixelFormatBGRA8Unorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_X8R8G8B8, MTLPixelFormatBGRA8Unorm, FK_PLAIN, 4, 4, kRGB1, true, false},
	{D3DFMT_A8B8G8R8, MTLPixelFormatRGBA8Unorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_X8B8G8R8, MTLPixelFormatRGBA8Unorm, FK_PLAIN, 4, 4, kRGB1, true, false},
	{D3DFMT_R8G8B8, MTLPixelFormatBGRA8Unorm, FK_CONVERTED, 3, 4, kIdentity, false, false},
	{D3DFMT_R5G6B5, MTLPixelFormatBGRA8Unorm, FK_CONVERTED, 2, 4, kIdentity, false, false},
	{D3DFMT_X1R5G5B5, MTLPixelFormatBGRA8Unorm, FK_CONVERTED, 2, 4, kIdentity, false, false},
	{D3DFMT_A1R5G5B5, MTLPixelFormatBGRA8Unorm, FK_CONVERTED, 2, 4, kIdentity, false, false},
	{D3DFMT_A4R4G4B4, MTLPixelFormatBGRA8Unorm, FK_CONVERTED, 2, 4, kIdentity, false, false},
	{D3DFMT_X4R4G4B4, MTLPixelFormatBGRA8Unorm, FK_CONVERTED, 2, 4, kIdentity, false, false},
	{D3DFMT_A2R10G10B10, MTLPixelFormatBGR10A2Unorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_A2B10G10R10, MTLPixelFormatRGB10A2Unorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_G16R16, MTLPixelFormatRG16Unorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_A16B16G16R16, MTLPixelFormatRGBA16Unorm, FK_PLAIN, 8, 8, kIdentity, false, false},
	{D3DFMT_A8, MTLPixelFormatA8Unorm, FK_PLAIN, 1, 1, kIdentity, false, false},
	{D3DFMT_L8, MTLPixelFormatR8Unorm, FK_PLAIN, 1, 1, kRRR1, true, false},
	{D3DFMT_L16, MTLPixelFormatR16Unorm, FK_PLAIN, 2, 2, kRRR1, true, false},
	{D3DFMT_A8L8, MTLPixelFormatRG8Unorm, FK_PLAIN, 2, 2, kRRRG, true, false},
	{D3DFMT_V8U8, MTLPixelFormatRG8Snorm, FK_PLAIN, 2, 2, kIdentity, false, false},
	{D3DFMT_Q8W8V8U8, MTLPixelFormatRGBA8Snorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_V16U16, MTLPixelFormatRG16Snorm, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_Q16W16V16U16, MTLPixelFormatRGBA16Snorm, FK_PLAIN, 8, 8, kIdentity, false, false},
	{D3DFMT_R16F, MTLPixelFormatR16Float, FK_PLAIN, 2, 2, kIdentity, false, false},
	{D3DFMT_G16R16F, MTLPixelFormatRG16Float, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_A16B16G16R16F, MTLPixelFormatRGBA16Float, FK_PLAIN, 8, 8, kIdentity, false, false},
	{D3DFMT_R32F, MTLPixelFormatR32Float, FK_PLAIN, 4, 4, kIdentity, false, false},
	{D3DFMT_G32R32F, MTLPixelFormatRG32Float, FK_PLAIN, 8, 8, kIdentity, false, false},
	{D3DFMT_A32B32G32R32F, MTLPixelFormatRGBA32Float, FK_PLAIN, 16, 16, kIdentity, false, false},
	{D3DFMT_DXT1, MTLPixelFormatBC1_RGBA, FK_COMPRESSED, 8, 8, kIdentity, false, false},
	{D3DFMT_DXT2, MTLPixelFormatBC2_RGBA, FK_COMPRESSED, 16, 16, kIdentity, false, false},
	{D3DFMT_DXT3, MTLPixelFormatBC2_RGBA, FK_COMPRESSED, 16, 16, kIdentity, false, false},
	{D3DFMT_DXT4, MTLPixelFormatBC3_RGBA, FK_COMPRESSED, 16, 16, kIdentity, false, false},
	{D3DFMT_DXT5, MTLPixelFormatBC3_RGBA, FK_COMPRESSED, 16, 16, kIdentity, false, false},
	{D3DFMT_D24S8, MTLPixelFormatDepth32Float_Stencil8, FK_DEPTH, 4, 8, kIdentity, false, true},
	{D3DFMT_D24X8, MTLPixelFormatDepth32Float_Stencil8, FK_DEPTH, 4, 8, kIdentity, false, true},
	{D3DFMT_D24FS8, MTLPixelFormatDepth32Float_Stencil8, FK_DEPTH, 4, 8, kIdentity, false, true},
	{D3DFMT_D24X4S4, MTLPixelFormatDepth32Float_Stencil8, FK_DEPTH, 4, 8, kIdentity, false, true},
	{D3DFMT_D15S1, MTLPixelFormatDepth32Float_Stencil8, FK_DEPTH, 2, 8, kIdentity, false, true},
	{D3DFMT_D16, MTLPixelFormatDepth16Unorm, FK_DEPTH, 2, 2, kIdentity, false, false},
	{D3DFMT_D16_LOCKABLE, MTLPixelFormatDepth16Unorm, FK_DEPTH, 2, 2, kIdentity, false, false},
	{D3DFMT_D32, MTLPixelFormatDepth32Float, FK_DEPTH, 4, 4, kIdentity, false, false},
	{D3DFMT_D32F_LOCKABLE, MTLPixelFormatDepth32Float, FK_DEPTH, 4, 4, kIdentity, false, false},
};

const FormatInfo *GetFormat(D3DFORMAT fmt)
{
	for (const auto &f : kFormats)
		if (f.d3d == fmt) return &f;
	return nullptr;
}

UINT RowPitch(const FormatInfo *fi, UINT width)
{
	if (fi->kind == FK_COMPRESSED) return std::max(1u, (width + 3) / 4) * fi->bytes;
	return width * fi->bytes;
}

UINT RowCount(const FormatInfo *fi, UINT height)
{
	if (fi->kind == FK_COMPRESSED) return std::max(1u, (height + 3) / 4);
	return height;
}

static inline uint8_t Expand(uint32_t v, int bits)
{
	return (uint8_t)((v * 255 + ((1 << bits) - 1) / 2) / ((1 << bits) - 1));
}

void ConvertToGpu(const FormatInfo *fi, const uint8_t *src, UINT srcPitch, uint8_t *dst, UINT dstPitch, UINT w, UINT h)
{
	for (UINT y = 0; y < h; y++) {
		const uint8_t *s = src + (size_t)y * srcPitch;
		uint8_t *d = dst + (size_t)y * dstPitch;
		for (UINT x = 0; x < w; x++, d += 4) {
			uint32_t b = 0, g = 0, r = 0, a = 255;
			switch (fi->d3d) {
			case D3DFMT_R8G8B8: b = s[x * 3]; g = s[x * 3 + 1]; r = s[x * 3 + 2]; break;
			case D3DFMT_R5G6B5: { uint16_t p = ((const uint16_t *)s)[x]; r = Expand(p >> 11, 5); g = Expand((p >> 5) & 63, 6); b = Expand(p & 31, 5); break; }
			case D3DFMT_X1R5G5B5:
			case D3DFMT_A1R5G5B5: { uint16_t p = ((const uint16_t *)s)[x]; r = Expand((p >> 10) & 31, 5); g = Expand((p >> 5) & 31, 5); b = Expand(p & 31, 5); if (fi->d3d == D3DFMT_A1R5G5B5) a = (p & 0x8000) ? 255 : 0; break; }
			case D3DFMT_A4R4G4B4:
			case D3DFMT_X4R4G4B4: { uint16_t p = ((const uint16_t *)s)[x]; a = fi->d3d == D3DFMT_A4R4G4B4 ? Expand(p >> 12, 4) : 255; r = Expand((p >> 8) & 15, 4); g = Expand((p >> 4) & 15, 4); b = Expand(p & 15, 4); break; }
			default: break;
			}
			d[0] = (uint8_t)b; d[1] = (uint8_t)g; d[2] = (uint8_t)r; d[3] = (uint8_t)a;
		}
	}
}

void ConvertFromGpu(const FormatInfo *fi, const uint8_t *src, UINT srcPitch, uint8_t *dst, UINT dstPitch, UINT w, UINT h)
{
	for (UINT y = 0; y < h; y++) {
		const uint8_t *s = src + (size_t)y * srcPitch;
		uint8_t *d = dst + (size_t)y * dstPitch;
		for (UINT x = 0; x < w; x++, s += 4) {
			uint32_t b = s[0], g = s[1], r = s[2], a = s[3];
			switch (fi->d3d) {
			case D3DFMT_R8G8B8: d[x * 3] = (uint8_t)b; d[x * 3 + 1] = (uint8_t)g; d[x * 3 + 2] = (uint8_t)r; break;
			case D3DFMT_R5G6B5: ((uint16_t *)d)[x] = (uint16_t)(((r >> 3) << 11) | ((g >> 2) << 5) | (b >> 3)); break;
			case D3DFMT_X1R5G5B5:
			case D3DFMT_A1R5G5B5: ((uint16_t *)d)[x] = (uint16_t)(((a >= 128) << 15) | ((r >> 3) << 10) | ((g >> 3) << 5) | (b >> 3)); break;
			case D3DFMT_A4R4G4B4:
			case D3DFMT_X4R4G4B4: ((uint16_t *)d)[x] = (uint16_t)(((a >> 4) << 12) | ((r >> 4) << 8) | ((g >> 4) << 4) | (b >> 4)); break;
			default: break;
			}
		}
	}
}

int AttributeIndex(BYTE usage, BYTE idx)
{
	switch (usage) {
	case D3DDECLUSAGE_POSITION: return 0 + (idx & 1);
	case D3DDECLUSAGE_NORMAL: return 2 + (idx & 1);
	case D3DDECLUSAGE_TEXCOORD: return 4 + (idx & 7);
	case D3DDECLUSAGE_COLOR: return 12 + (idx & 1);
	case D3DDECLUSAGE_TANGENT: return 14;
	case D3DDECLUSAGE_BINORMAL: return 15;
	case D3DDECLUSAGE_PSIZE: return 16;
	case D3DDECLUSAGE_BLENDWEIGHT: return 17;
	case D3DDECLUSAGE_BLENDINDICES: return 18;
	case D3DDECLUSAGE_POSITIONT: return 19;
	case D3DDECLUSAGE_FOG: return 20;
	default: return -1;
	}
}

// ---------------------------------------------------------------------------
// Textures

static UINT SubCount(const Texture *t) { return t->Faces() * t->levels; }

IDirect3DSurface9 *Texture::GetSurface(UINT face, UINT level)
{
	UINT idx = Sub(face, level);
	if (idx >= surfaces.size()) return nullptr;
	if (!surfaces[idx]) {
		IDirect3DSurface9 *s = new IDirect3DSurface9();
		Surface *si = new Surface();
		si->tex = this;
		si->face = face;
		si->level = level;
		si->iface = s;
		s->m_surf = si;
		surfaces[idx] = s;
	}
	return surfaces[idx];
}

id<MTLTexture> Texture::SampleTexture()
{
	return view ? view : tex;
}

static MTLRegion RegionFor(const Texture *t, UINT level, const RECT *rect, UINT slice, UINT slices)
{
	UINT w = t->LevelWidth(level), h = t->LevelHeight(level);
	if (!rect) return MTLRegionMake3D(0, slice, 0, w, h, slices);
	UINT l = std::min<UINT>(rect->left, w), tp = std::min<UINT>(rect->top, h);
	UINT r = std::min<UINT>(rect->right, w), b = std::min<UINT>(rect->bottom, h);
	if (t->fi->kind == FK_COMPRESSED) {
		l &= ~3u; tp &= ~3u;
		r = std::min(w, (r + 3) & ~3u);
		b = std::min(h, (b + 3) & ~3u);
	}
	return MTLRegionMake3D(l, tp, slice, r > l ? r - l : 0, b > tp ? b - tp : 0, slices);
}

void Texture::Upload(UINT face, UINT level, const uint8_t *data, UINT pitch, const RECT *rect, UINT slice, UINT slices)
{
	if (IsSystem()) return;
	if (!tex) return;
	if (msaa) msaaDirty = true;
	MTLRegion rg = RegionFor(this, level, rect, slice, slices);
	if (type != TT_3D) rg.origin.y = rect ? rg.origin.y : 0;
	if (type == TT_3D) { rg.origin.z = slice; rg.size.depth = slices; rg.origin.y = rect ? rect->top : 0; }
	else { rg.origin.z = 0; rg.size.depth = 1; }
	if (rg.size.width == 0 || rg.size.height == 0) return;

	UINT rows = RowCount(fi, (UINT)rg.size.height);
	UINT gpuPitch;
	std::vector<uint8_t> conv;
	const uint8_t *src = data;
	UINT srcPitch = pitch;
	if (fi->kind == FK_CONVERTED) {
		gpuPitch = (UINT)rg.size.width * fi->gpuBytes;
		conv.resize((size_t)gpuPitch * rows * rg.size.depth);
		for (UINT z = 0; z < rg.size.depth; z++)
			ConvertToGpu(fi, data + (size_t)z * pitch * rows, pitch, conv.data() + (size_t)z * gpuPitch * rows, gpuPitch, (UINT)rg.size.width, rows);
		src = conv.data();
		srcPitch = gpuPitch;
	} else {
		gpuPitch = fi->kind == FK_COMPRESSED ? RowPitch(fi, (UINT)rg.size.width) : (UINT)rg.size.width * fi->bytes;
	}
	size_t bytes = (size_t)gpuPitch * rows * rg.size.depth;
	id<MTLBuffer> staging = [dev->mtl newBufferWithLength:bytes options:MTLResourceStorageModeShared];
	uint8_t *dst = (uint8_t *)staging.contents;
	for (UINT z = 0; z < rg.size.depth; z++)
		for (UINT y = 0; y < rows; y++)
			memcpy(dst + ((size_t)z * rows + y) * gpuPitch, src + ((size_t)z * rows + y) * srcPitch, gpuPitch);

	id<MTLTexture> t = tex;
	NSUInteger dslice = type == TT_CUBE ? face : 0, depthCount = type == TT_3D ? rg.size.depth : 1, oz = type == TT_3D ? rg.origin.z : 0;
	dev->EncodeBlit(^(id<MTLBlitCommandEncoder> b) {
		[b copyFromBuffer:staging sourceOffset:0 sourceBytesPerRow:gpuPitch sourceBytesPerImage:(NSUInteger)gpuPitch * rows
			sourceSize:MTLSizeMake(rg.size.width, rg.size.height, depthCount)
			toTexture:t destinationSlice:dslice destinationLevel:level
			destinationOrigin:MTLOriginMake(rg.origin.x, rg.origin.y, oz)];
	});
}

void Texture::Download(UINT face, UINT level, uint8_t *data, UINT pitch, const RECT *rect)
{
	if (!tex) return;
	MTLRegion rg = RegionFor(this, level, rect, 0, 1);
	if (rg.size.width == 0 || rg.size.height == 0) return;
	UINT rows = RowCount(fi, (UINT)rg.size.height);
	UINT gpuPitch = fi->kind == FK_COMPRESSED ? RowPitch(fi, (UINT)rg.size.width) : (UINT)rg.size.width * fi->gpuBytes;
	id<MTLBuffer> staging = [dev->mtl newBufferWithLength:(size_t)gpuPitch * rows options:MTLResourceStorageModeShared];
	id<MTLTexture> t = tex;
	NSUInteger slice = type == TT_CUBE ? face : 0;
	dev->EncodeBlit(^(id<MTLBlitCommandEncoder> b) {
		[b copyFromTexture:t sourceSlice:slice sourceLevel:level sourceOrigin:rg.origin
			sourceSize:MTLSizeMake(rg.size.width, rg.size.height, 1) toBuffer:staging destinationOffset:0
			destinationBytesPerRow:gpuPitch destinationBytesPerImage:(NSUInteger)gpuPitch * rows];
	}, true);
	const uint8_t *src = (const uint8_t *)staging.contents;
	if (fi->kind == FK_CONVERTED) {
		ConvertFromGpu(fi, src, gpuPitch, data, pitch, (UINT)rg.size.width, rows);
	} else {
		UINT rowBytes = std::min(gpuPitch, pitch);
		for (UINT y = 0; y < rows; y++)
			memcpy(data + (size_t)y * pitch, src + (size_t)y * gpuPitch, rowBytes);
	}
}

HRESULT Texture::Lock(UINT face, UINT level, D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags)
{
	if (level >= levels || face >= Faces() || !lr) return D3DERR_INVALIDCALL;
	UINT sub = Sub(face, level);
	if (IsSystem()) {
		UINT pitch = RowPitch(fi, LevelWidth(level));
		uint8_t *base = sysmem[sub].data();
		if (rect) {
			UINT bx = fi->kind == FK_COMPRESSED ? (UINT)rect->left / 4 : (UINT)rect->left;
			UINT by = fi->kind == FK_COMPRESSED ? (UINT)rect->top / 4 : (UINT)rect->top;
			base += (size_t)by * pitch + (size_t)bx * fi->bytes;
		}
		lr->pBits = base;
		lr->Pitch = (INT)pitch;
		locks[sub].active = true;
		return S_OK;
	}
	LockState &ls = locks[sub];
	if (ls.active) return D3DERR_INVALIDCALL;
	UINT w = LevelWidth(level), h = LevelHeight(level);
	RECT r = rect ? *rect : RECT{0, 0, (LONG)w, (LONG)h};
	UINT rw = (UINT)(r.right - r.left), rh = (UINT)(r.bottom - r.top);
	ls.pitch = RowPitch(fi, rw);
	ls.data.assign((size_t)ls.pitch * RowCount(fi, rh), 0);
	ls.rect = r;
	ls.flags = flags;
	ls.active = true;
	bool needsRead = !(flags & D3DLOCK_DISCARD) && !(usage & D3DUSAGE_WRITEONLY);
	if (needsRead && !(usage & D3DUSAGE_DYNAMIC && flags & D3DLOCK_NOOVERWRITE))
		Download(face, level, ls.data.data(), ls.pitch, &r);
	lr->pBits = ls.data.data();
	lr->Pitch = (INT)ls.pitch;
	return S_OK;
}

HRESULT Texture::Unlock(UINT face, UINT level)
{
	if (level >= levels || face >= Faces()) return D3DERR_INVALIDCALL;
	UINT sub = Sub(face, level);
	LockState &ls = locks[sub];
	if (IsSystem()) {
		ls.active = false;
		return S_OK;
	}
	if (!ls.active) return D3DERR_INVALIDCALL;
	ls.active = false;
	if (!(ls.flags & D3DLOCK_READONLY))
		Upload(face, level, ls.data.data(), ls.pitch, &ls.rect);
	std::vector<uint8_t>().swap(ls.data);
	return S_OK;
}

HRESULT Texture::LockBox(UINT level, D3DLOCKED_BOX *lb, const D3DBOX *box, DWORD flags)
{
	if (level >= levels || !lb) return D3DERR_INVALIDCALL;
	UINT w = LevelWidth(level), h = LevelHeight(level), d = LevelDepth(level);
	UINT pitch = RowPitch(fi, w), slice = pitch * RowCount(fi, h);
	if (IsSystem()) {
		uint8_t *base = sysmem[level].data();
		if (box) base += (size_t)box->Front * slice + (size_t)box->Top * pitch + (size_t)box->Left * fi->bytes;
		lb->pBits = base;
		lb->RowPitch = (INT)pitch;
		lb->SlicePitch = (INT)slice;
		return S_OK;
	}
	LockState &ls = locks[level];
	if (ls.active) return D3DERR_INVALIDCALL;
	ls.pitch = pitch;
	ls.slicePitch = slice;
	ls.data.assign((size_t)slice * d, 0);
	ls.flags = flags;
	ls.active = true;
	lb->pBits = ls.data.data();
	lb->RowPitch = (INT)pitch;
	lb->SlicePitch = (INT)slice;
	return S_OK;
}

HRESULT Texture::UnlockBox(UINT level)
{
	if (level >= levels) return D3DERR_INVALIDCALL;
	if (IsSystem()) return S_OK;
	LockState &ls = locks[level];
	if (!ls.active) return D3DERR_INVALIDCALL;
	ls.active = false;
	if (!(ls.flags & D3DLOCK_READONLY))
		Upload(0, level, ls.data.data(), ls.pitch, nullptr, 0, LevelDepth(level));
	std::vector<uint8_t>().swap(ls.data);
	return S_OK;
}

Texture *CreateTexture(Device *dev, TexType type, UINT w, UINT h, UINT d, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, UINT samples)
{
	const FormatInfo *fi = GetFormat(fmt);
	if (!fi) {
		Log("unsupported texture format %u", (unsigned)fmt);
		return nullptr;
	}
	if (w == 0 || h == 0) return nullptr;
	Texture *t = new Texture();
	t->dev = dev;
	t->type = type;
	t->fmt = fmt;
	t->fi = fi;
	t->width = w;
	t->height = type == TT_CUBE ? w : h;
	t->depth = type == TT_3D ? std::max(1u, d) : 1;
	UINT maxLevels = 1;
	for (UINT s = std::max(std::max(w, h), t->depth); s > 1; s >>= 1) maxLevels++;
	if (levels == 0 || levels > maxLevels) levels = (usage & D3DUSAGE_AUTOGENMIPMAP || levels == 0) ? maxLevels : maxLevels;
	if (usage & D3DUSAGE_RENDERTARGET && !(usage & D3DUSAGE_AUTOGENMIPMAP) && levels == 0) levels = 1;
	if (fi->kind == FK_DEPTH) levels = 1;
	t->levels = levels;
	t->usage = usage;
	t->pool = pool;
	t->samples = samples > 1 ? samples : 1;
	t->locks.resize(SubCount(t));
	t->surfaces.resize(SubCount(t), nullptr);

	if (t->IsSystem()) {
		t->sysmem.resize(SubCount(t));
		for (UINT f = 0; f < t->Faces(); f++)
			for (UINT l = 0; l < levels; l++)
				t->sysmem[t->Sub(f, l)].assign((size_t)RowPitch(fi, t->LevelWidth(l)) * RowCount(fi, t->LevelHeight(l)) * t->LevelDepth(l), 0);
		return t;
	}

	MTLTextureDescriptor *td = [MTLTextureDescriptor new];
	td.textureType = type == TT_CUBE ? MTLTextureTypeCube : type == TT_3D ? MTLTextureType3D : MTLTextureType2D;
	td.pixelFormat = fi->pf;
	td.width = t->width;
	td.height = t->height;
	td.depth = t->depth;
	td.mipmapLevelCount = levels;
	td.storageMode = MTLStorageModePrivate;
	td.usage = MTLTextureUsageShaderRead;
	if (usage & (D3DUSAGE_RENDERTARGET | D3DUSAGE_DEPTHSTENCIL) || fi->kind == FK_DEPTH)
		td.usage |= MTLTextureUsageRenderTarget;
	bool rtSwizzle = fi->hasSwizzle && (td.usage & MTLTextureUsageRenderTarget);
	if (fi->hasSwizzle && !rtSwizzle) td.swizzle = fi->swizzle;
	if (rtSwizzle) td.usage |= MTLTextureUsagePixelFormatView;
	t->tex = [dev->mtl newTextureWithDescriptor:td];
	if (!t->tex) {
		Log("texture creation failed (%ux%u, format %u)", w, h, (unsigned)fmt);
		delete t;
		return nullptr;
	}
	if (rtSwizzle)
		t->view = [t->tex newTextureViewWithPixelFormat:fi->pf textureType:td.textureType levels:NSMakeRange(0, levels)
			slices:NSMakeRange(0, type == TT_CUBE ? 6 : 1) swizzle:fi->swizzle];
	if (t->samples > 1) {
		MTLTextureDescriptor *md = [MTLTextureDescriptor new];
		md.textureType = MTLTextureType2DMultisample;
		md.pixelFormat = fi->pf;
		md.width = t->width;
		md.height = t->height;
		md.sampleCount = t->samples;
		md.storageMode = MTLStorageModePrivate;
		md.usage = MTLTextureUsageRenderTarget;
		t->msaa = [dev->mtl newTextureWithDescriptor:md];
	}
	return t;
}

void DestroyTexture(Texture *t)
{
	if (!t) return;
	for (auto *s : t->surfaces) {
		if (!s) continue;
		Surface *si = s->m_surf;
		s->m_surf = nullptr;
		delete si;
		delete s;
	}
	t->tex = nil;
	t->msaa = nil;
	delete t;
}

}

using namespace m9;

// ---------------------------------------------------------------------------
// IUnknown

ULONG IUnknownM9::AddRef() { return (ULONG)++m_refs; }

ULONG IUnknownM9::Release()
{
	long r = --m_refs;
	if (r == 0 && m_binds == 0) delete this;
	return (ULONG)(r < 0 ? 0 : r);
}

void IUnknownM9::BindRelease()
{
	if (--m_binds == 0 && m_refs <= 0) delete this;
}

HRESULT IDirect3DResource9::GetDevice(IDirect3DDevice9 **ppDevice)
{
	Device *d = CurrentDevice();
	if (!d || !ppDevice) return D3DERR_INVALIDCALL;
	*ppDevice = d->iface;
	d->iface->AddRef();
	return S_OK;
}

// ---------------------------------------------------------------------------
// Base texture

DWORD IDirect3DBaseTexture9::GetLevelCount() { return m_tex ? m_tex->levels : 0; }

void IDirect3DBaseTexture9::GenerateMipSubLevels()
{
	if (!m_tex || !m_tex->tex || m_tex->levels < 2) return;
	if (m_tex->fi->kind == FK_COMPRESSED || m_tex->fi->kind == FK_DEPTH) return;
	id<MTLTexture> t = m_tex->tex;
	m_tex->dev->EncodeBlit(^(id<MTLBlitCommandEncoder> b) { [b generateMipmapsForTexture:t]; });
}

// ---------------------------------------------------------------------------
// Texture

IDirect3DTexture9::~IDirect3DTexture9() { DestroyTexture(m_tex); }

HRESULT IDirect3DTexture9::GetLevelDesc(UINT level, D3DSURFACE_DESC *desc)
{
	if (!m_tex || level >= m_tex->levels || !desc) return D3DERR_INVALIDCALL;
	desc->Format = m_tex->fmt;
	desc->Type = D3DRTYPE_SURFACE;
	desc->Usage = m_tex->usage;
	desc->Pool = m_tex->pool;
	desc->MultiSampleType = (D3DMULTISAMPLE_TYPE)(m_tex->samples > 1 ? m_tex->samples : 0);
	desc->MultiSampleQuality = 0;
	desc->Width = m_tex->LevelWidth(level);
	desc->Height = m_tex->LevelHeight(level);
	return S_OK;
}

HRESULT IDirect3DTexture9::GetSurfaceLevel(UINT level, IDirect3DSurface9 **surf)
{
	if (!m_tex || level >= m_tex->levels || !surf) return D3DERR_INVALIDCALL;
	*surf = m_tex->GetSurface(0, level);
	AddRef();
	return S_OK;
}

HRESULT IDirect3DTexture9::LockRect(UINT level, D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags)
{
	return m_tex ? m_tex->Lock(0, level, lr, rect, flags) : D3DERR_INVALIDCALL;
}

HRESULT IDirect3DTexture9::UnlockRect(UINT level)
{
	return m_tex ? m_tex->Unlock(0, level) : D3DERR_INVALIDCALL;
}

IDirect3DCubeTexture9::~IDirect3DCubeTexture9() { DestroyTexture(m_tex); }

HRESULT IDirect3DCubeTexture9::GetLevelDesc(UINT level, D3DSURFACE_DESC *desc)
{
	if (!m_tex || level >= m_tex->levels || !desc) return D3DERR_INVALIDCALL;
	desc->Format = m_tex->fmt;
	desc->Type = D3DRTYPE_SURFACE;
	desc->Usage = m_tex->usage;
	desc->Pool = m_tex->pool;
	desc->MultiSampleType = D3DMULTISAMPLE_NONE;
	desc->MultiSampleQuality = 0;
	desc->Width = m_tex->LevelWidth(level);
	desc->Height = m_tex->LevelHeight(level);
	return S_OK;
}

HRESULT IDirect3DCubeTexture9::GetCubeMapSurface(D3DCUBEMAP_FACES face, UINT level, IDirect3DSurface9 **surf)
{
	if (!m_tex || level >= m_tex->levels || (UINT)face >= 6 || !surf) return D3DERR_INVALIDCALL;
	*surf = m_tex->GetSurface((UINT)face, level);
	AddRef();
	return S_OK;
}

HRESULT IDirect3DCubeTexture9::LockRect(D3DCUBEMAP_FACES face, UINT level, D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags)
{
	return m_tex ? m_tex->Lock((UINT)face, level, lr, rect, flags) : D3DERR_INVALIDCALL;
}

HRESULT IDirect3DCubeTexture9::UnlockRect(D3DCUBEMAP_FACES face, UINT level)
{
	return m_tex ? m_tex->Unlock((UINT)face, level) : D3DERR_INVALIDCALL;
}

IDirect3DVolumeTexture9::~IDirect3DVolumeTexture9() { DestroyTexture(m_tex); }

HRESULT IDirect3DVolumeTexture9::GetLevelDesc(UINT level, D3DVOLUME_DESC *desc)
{
	if (!m_tex || level >= m_tex->levels || !desc) return D3DERR_INVALIDCALL;
	desc->Format = m_tex->fmt;
	desc->Type = D3DRTYPE_VOLUME;
	desc->Usage = m_tex->usage;
	desc->Pool = m_tex->pool;
	desc->Width = m_tex->LevelWidth(level);
	desc->Height = m_tex->LevelHeight(level);
	desc->Depth = m_tex->LevelDepth(level);
	return S_OK;
}

HRESULT IDirect3DVolumeTexture9::LockBox(UINT level, D3DLOCKED_BOX *lb, const D3DBOX *box, DWORD flags)
{
	return m_tex ? m_tex->LockBox(level, lb, box, flags) : D3DERR_INVALIDCALL;
}

HRESULT IDirect3DVolumeTexture9::UnlockBox(UINT level)
{
	return m_tex ? m_tex->UnlockBox(level) : D3DERR_INVALIDCALL;
}

// ---------------------------------------------------------------------------
// Surface

IDirect3DSurface9::~IDirect3DSurface9()
{
	if (m_surf && m_surf->owner) {
		Texture *t = m_surf->tex;
		size_t n = t->surfaces.size();
		for (size_t i = 0; i < n; i++)
			if (t->surfaces[i] == this) t->surfaces[i] = nullptr;
		delete m_surf;
		m_surf = nullptr;
		DestroyTexture(t);
	}
}

ULONG IDirect3DSurface9::AddRef()
{
	if (m_surf && !m_surf->owner && m_surf->tex->iface)
		return m_surf->tex->iface->AddRef();
	return IUnknownM9::AddRef();
}

ULONG IDirect3DSurface9::Release()
{
	if (m_surf && !m_surf->owner && m_surf->tex->iface)
		return m_surf->tex->iface->Release();
	return IUnknownM9::Release();
}

HRESULT IDirect3DSurface9::GetDesc(D3DSURFACE_DESC *desc)
{
	if (!m_surf || !desc) return D3DERR_INVALIDCALL;
	Texture *t = m_surf->tex;
	desc->Format = t->fmt;
	desc->Type = D3DRTYPE_SURFACE;
	desc->Usage = t->usage;
	desc->Pool = t->pool;
	desc->MultiSampleType = (D3DMULTISAMPLE_TYPE)(t->samples > 1 ? t->samples : 0);
	desc->MultiSampleQuality = 0;
	desc->Width = t->LevelWidth(m_surf->level);
	desc->Height = t->LevelHeight(m_surf->level);
	return S_OK;
}

HRESULT IDirect3DSurface9::LockRect(D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags)
{
	if (!m_surf) return D3DERR_INVALIDCALL;
	return m_surf->tex->Lock(m_surf->face, m_surf->level, lr, rect, flags);
}

HRESULT IDirect3DSurface9::UnlockRect()
{
	if (!m_surf) return D3DERR_INVALIDCALL;
	return m_surf->tex->Unlock(m_surf->face, m_surf->level);
}

HRESULT IDirect3DSurface9::GetContainer(REFIID, void **container)
{
	if (!m_surf || !container || m_surf->owner || !m_surf->tex->iface) return E_NOTIMPL;
	*container = m_surf->tex->iface;
	m_surf->tex->iface->AddRef();
	return S_OK;
}

// ---------------------------------------------------------------------------
// Buffers

static HRESULT LockBuffer(Buffer *b, UINT offset, UINT size, void **data, DWORD flags)
{
	if (!b || !data) return D3DERR_INVALIDCALL;
	if (size == 0) size = b->size - offset;
	if (offset + size > b->size) return D3DERR_INVALIDCALL;
	Device *dev = b->dev;
	if (!(flags & D3DLOCK_READONLY) && !(flags & D3DLOCK_NOOVERWRITE) && dev->InFlight(b->lastUse)) {
		id<MTLBuffer> fresh = [dev->mtl newBufferWithLength:b->size options:MTLResourceStorageModeShared];
		if (!(flags & D3DLOCK_DISCARD))
			memcpy(fresh.contents, b->buf.contents, b->size);
		b->buf = fresh;
		b->lastUse = 0;
	} else if (flags & D3DLOCK_READONLY && dev->InFlight(b->lastUse)) {
		dev->Submit(true);
	}
	b->locked = true;
	*data = (uint8_t *)b->buf.contents + offset;
	return S_OK;
}

IDirect3DVertexBuffer9::~IDirect3DVertexBuffer9() { delete m_buf; }

HRESULT IDirect3DVertexBuffer9::Lock(UINT offset, UINT size, void **data, DWORD flags) { return LockBuffer(m_buf, offset, size, data, flags); }

HRESULT IDirect3DVertexBuffer9::Unlock()
{
	if (m_buf) m_buf->locked = false;
	return S_OK;
}

HRESULT IDirect3DVertexBuffer9::GetDesc(D3DVERTEXBUFFER_DESC *desc)
{
	if (!m_buf || !desc) return D3DERR_INVALIDCALL;
	desc->Format = D3DFMT_VERTEXDATA;
	desc->Type = D3DRTYPE_VERTEXBUFFER;
	desc->Usage = m_buf->usage;
	desc->Pool = m_buf->pool;
	desc->Size = m_buf->size;
	desc->FVF = m_buf->fvf;
	return S_OK;
}

IDirect3DIndexBuffer9::~IDirect3DIndexBuffer9() { delete m_buf; }

HRESULT IDirect3DIndexBuffer9::Lock(UINT offset, UINT size, void **data, DWORD flags) { return LockBuffer(m_buf, offset, size, data, flags); }

HRESULT IDirect3DIndexBuffer9::Unlock()
{
	if (m_buf) m_buf->locked = false;
	return S_OK;
}

HRESULT IDirect3DIndexBuffer9::GetDesc(D3DINDEXBUFFER_DESC *desc)
{
	if (!m_buf || !desc) return D3DERR_INVALIDCALL;
	desc->Format = m_buf->fmt;
	desc->Type = D3DRTYPE_INDEXBUFFER;
	desc->Usage = m_buf->usage;
	desc->Pool = m_buf->pool;
	desc->Size = m_buf->size;
	return S_OK;
}

// ---------------------------------------------------------------------------
// Declarations, shaders, queries

IDirect3DVertexDeclaration9::~IDirect3DVertexDeclaration9() { delete m_decl; }

HRESULT IDirect3DVertexDeclaration9::GetDeclaration(D3DVERTEXELEMENT9 *elements, UINT *count)
{
	if (!m_decl || !count) return D3DERR_INVALIDCALL;
	UINT n = (UINT)m_decl->elems.size() + 1;
	if (elements) {
		for (UINT i = 0; i + 1 < n; i++) elements[i] = m_decl->elems[i];
		elements[n - 1] = D3DVERTEXELEMENT9 D3DDECL_END();
	}
	*count = n;
	return S_OK;
}

IDirect3DVertexShader9::~IDirect3DVertexShader9() { delete m_shader; }
IDirect3DPixelShader9::~IDirect3DPixelShader9() { delete m_shader; }

IDirect3DQuery9::~IDirect3DQuery9() { delete m_query; }

DWORD IDirect3DQuery9::GetDataSize()
{
	switch (m_type) {
	case D3DQUERYTYPE_OCCLUSION: return sizeof(DWORD);
	case D3DQUERYTYPE_EVENT: return sizeof(WINBOOL);
	case D3DQUERYTYPE_TIMESTAMP: return sizeof(UINT64);
	case D3DQUERYTYPE_TIMESTAMPDISJOINT: return sizeof(WINBOOL);
	case D3DQUERYTYPE_TIMESTAMPFREQ: return sizeof(UINT64);
	default: return 0;
	}
}

HRESULT IDirect3DQuery9::Issue(DWORD flags)
{
	Device *d = CurrentDevice();
	if (!d || !m_query) return D3DERR_INVALIDCALL;
	if (m_type == D3DQUERYTYPE_OCCLUSION) {
		if (flags & D3DISSUE_BEGIN) {
			if (d->visNext + 8 > d->visBuffer.length) d->visNext = 0;
			m_query->visOffset = d->visNext;
			d->visNext += 8;
			*(uint64_t *)((uint8_t *)d->visBuffer.contents + m_query->visOffset) = 0;
			[d->RenderEncoder() setVisibilityResultMode:MTLVisibilityResultModeCounting offset:m_query->visOffset];
		}
		if (flags & D3DISSUE_END) {
			if (d->renc) [d->renc setVisibilityResultMode:MTLVisibilityResultModeDisabled offset:0];
			m_query->serial = d->serial;
			m_query->issued = true;
		}
		return S_OK;
	}
	if (flags & D3DISSUE_END) {
		m_query->serial = d->serial;
		m_query->issued = true;
	}
	return S_OK;
}

HRESULT IDirect3DQuery9::GetData(void *data, DWORD size, DWORD flags)
{
	Device *d = CurrentDevice();
	if (!d || !m_query) return D3DERR_INVALIDCALL;
	if (m_query->issued && m_query->serial == d->serial && (flags & D3DGETDATA_FLUSH))
		d->Submit(false);
	bool done = !m_query->issued || !d->InFlight(m_query->serial);
	if (!done) return S_FALSE;
	if (!data || !size) return S_OK;
	switch (m_type) {
	case D3DQUERYTYPE_OCCLUSION: {
		uint64_t v = *(uint64_t *)((uint8_t *)d->visBuffer.contents + m_query->visOffset);
		DWORD r = (DWORD)v;
		memcpy(data, &r, std::min<DWORD>(size, sizeof(r)));
		break;
	}
	case D3DQUERYTYPE_EVENT: {
		WINBOOL r = TRUE;
		memcpy(data, &r, std::min<DWORD>(size, sizeof(r)));
		break;
	}
	case D3DQUERYTYPE_TIMESTAMPDISJOINT: {
		WINBOOL r = FALSE;
		memcpy(data, &r, std::min<DWORD>(size, sizeof(r)));
		break;
	}
	case D3DQUERYTYPE_TIMESTAMP:
	case D3DQUERYTYPE_TIMESTAMPFREQ: {
		UINT64 r = m_type == D3DQUERYTYPE_TIMESTAMPFREQ ? 1000000000ull : (UINT64)(GetTickCount64() * 1000000ull);
		memcpy(data, &r, std::min<DWORD>(size, (DWORD)sizeof(r)));
		break;
	}
	default:
		memset(data, 0, size);
		break;
	}
	return S_OK;
}
