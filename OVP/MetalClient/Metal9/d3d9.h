#pragma once

// Direct3D 9 interface implemented on Metal. The graphics client renders
// through these types; Metal9 maps them onto Metal devices, textures,
// buffers and pipelines.

#include <windows.h>
#include <stdint.h>

#define D3D_SDK_VERSION 32

#ifndef MAKEFOURCC
#define MAKEFOURCC(c0, c1, c2, c3) ((DWORD)(BYTE)(c0) | ((DWORD)(BYTE)(c1) << 8) | ((DWORD)(BYTE)(c2) << 16) | ((DWORD)(BYTE)(c3) << 24))
#endif

typedef DWORD D3DCOLOR;
#define D3DCOLOR_ARGB(a, r, g, b) ((D3DCOLOR)((((a) & 0xff) << 24) | (((r) & 0xff) << 16) | (((g) & 0xff) << 8) | ((b) & 0xff)))
#define D3DCOLOR_RGBA(r, g, b, a) D3DCOLOR_ARGB(a, r, g, b)
#define D3DCOLOR_XRGB(r, g, b) D3DCOLOR_ARGB(0xff, r, g, b)
#define D3DCOLOR_COLORVALUE(r, g, b, a) D3DCOLOR_RGBA((DWORD)((r) * 255.f), (DWORD)((g) * 255.f), (DWORD)((b) * 255.f), (DWORD)((a) * 255.f))

#ifndef D3DVECTOR_DEFINED
#define D3DVECTOR_DEFINED
typedef struct _D3DVECTOR { float x, y, z; } D3DVECTOR;
#endif

#ifndef D3DCOLORVALUE_DEFINED
#define D3DCOLORVALUE_DEFINED
typedef struct _D3DCOLORVALUE { float r, g, b, a; } D3DCOLORVALUE;
#endif

#ifndef D3DMATRIX_DEFINED
#define D3DMATRIX_DEFINED
typedef struct _D3DMATRIX {
	union {
		struct {
			float _11, _12, _13, _14;
			float _21, _22, _23, _24;
			float _31, _32, _33, _34;
			float _41, _42, _43, _44;
		};
		float m[4][4];
	};
} D3DMATRIX;
#endif

typedef struct _D3DRECT { LONG x1, y1, x2, y2; } D3DRECT;

typedef struct _D3DVIEWPORT9 {
	DWORD X, Y, Width, Height;
	float MinZ, MaxZ;
} D3DVIEWPORT9;

typedef struct _D3DMATERIAL9 {
	D3DCOLORVALUE Diffuse, Ambient, Specular, Emissive;
	float Power;
} D3DMATERIAL9;

typedef enum _D3DFORMAT {
	D3DFMT_UNKNOWN = 0,
	D3DFMT_R8G8B8 = 20,
	D3DFMT_A8R8G8B8 = 21,
	D3DFMT_X8R8G8B8 = 22,
	D3DFMT_R5G6B5 = 23,
	D3DFMT_X1R5G5B5 = 24,
	D3DFMT_A1R5G5B5 = 25,
	D3DFMT_A4R4G4B4 = 26,
	D3DFMT_R3G3B2 = 27,
	D3DFMT_A8 = 28,
	D3DFMT_A8R3G3B2 = 29,
	D3DFMT_X4R4G4B4 = 30,
	D3DFMT_A2B10G10R10 = 31,
	D3DFMT_A8B8G8R8 = 32,
	D3DFMT_X8B8G8R8 = 33,
	D3DFMT_G16R16 = 34,
	D3DFMT_A2R10G10B10 = 35,
	D3DFMT_A16B16G16R16 = 36,
	D3DFMT_A8P8 = 40,
	D3DFMT_P8 = 41,
	D3DFMT_L8 = 50,
	D3DFMT_A8L8 = 51,
	D3DFMT_A4L4 = 52,
	D3DFMT_V8U8 = 60,
	D3DFMT_L6V5U5 = 61,
	D3DFMT_X8L8V8U8 = 62,
	D3DFMT_Q8W8V8U8 = 63,
	D3DFMT_V16U16 = 64,
	D3DFMT_A2W10V10U10 = 67,
	D3DFMT_DXT1 = MAKEFOURCC('D', 'X', 'T', '1'),
	D3DFMT_DXT2 = MAKEFOURCC('D', 'X', 'T', '2'),
	D3DFMT_DXT3 = MAKEFOURCC('D', 'X', 'T', '3'),
	D3DFMT_DXT4 = MAKEFOURCC('D', 'X', 'T', '4'),
	D3DFMT_DXT5 = MAKEFOURCC('D', 'X', 'T', '5'),
	D3DFMT_D16_LOCKABLE = 70,
	D3DFMT_D32 = 71,
	D3DFMT_D15S1 = 73,
	D3DFMT_D24S8 = 75,
	D3DFMT_D24X8 = 77,
	D3DFMT_D24X4S4 = 79,
	D3DFMT_D16 = 80,
	D3DFMT_D32F_LOCKABLE = 82,
	D3DFMT_D24FS8 = 83,
	D3DFMT_L16 = 81,
	D3DFMT_VERTEXDATA = 100,
	D3DFMT_INDEX16 = 101,
	D3DFMT_INDEX32 = 102,
	D3DFMT_Q16W16V16U16 = 110,
	D3DFMT_R16F = 111,
	D3DFMT_G16R16F = 112,
	D3DFMT_A16B16G16R16F = 113,
	D3DFMT_R32F = 114,
	D3DFMT_G32R32F = 115,
	D3DFMT_A32B32G32R32F = 116,
	D3DFMT_CxV8U8 = 117,
	D3DFMT_FORCE_DWORD = 0x7fffffff
} D3DFORMAT;

typedef enum _D3DPOOL {
	D3DPOOL_DEFAULT = 0,
	D3DPOOL_MANAGED = 1,
	D3DPOOL_SYSTEMMEM = 2,
	D3DPOOL_SCRATCH = 3,
	D3DPOOL_FORCE_DWORD = 0x7fffffff
} D3DPOOL;

#define D3DUSAGE_RENDERTARGET 0x00000001L
#define D3DUSAGE_DEPTHSTENCIL 0x00000002L
#define D3DUSAGE_DYNAMIC 0x00000200L
#define D3DUSAGE_AUTOGENMIPMAP 0x00000400L
#define D3DUSAGE_DMAP 0x00004000L
#define D3DUSAGE_WRITEONLY 0x00000008L
#define D3DUSAGE_SOFTWAREPROCESSING 0x00000010L
#define D3DUSAGE_DONOTCLIP 0x00000020L
#define D3DUSAGE_POINTS 0x00000040L
#define D3DUSAGE_RTPATCHES 0x00000080L
#define D3DUSAGE_NPATCHES 0x00000100L
#define D3DUSAGE_QUERY_VERTEXTEXTURE 0x00100000L
#define D3DUSAGE_QUERY_FILTER 0x00020000L
#define D3DUSAGE_QUERY_POSTPIXELSHADER_BLENDING 0x00080000L

#define D3DLOCK_READONLY 0x00000010L
#define D3DLOCK_DISCARD 0x00002000L
#define D3DLOCK_NOOVERWRITE 0x00001000L
#define D3DLOCK_NOSYSLOCK 0x00000800L
#define D3DLOCK_DONOTWAIT 0x00004000L
#define D3DLOCK_NO_DIRTY_UPDATE 0x00008000L

#ifndef D3DPT_DEFINED
#define D3DPT_DEFINED
typedef enum _D3DPRIMITIVETYPE {
	D3DPT_POINTLIST = 1,
	D3DPT_LINELIST = 2,
	D3DPT_LINESTRIP = 3,
	D3DPT_TRIANGLELIST = 4,
	D3DPT_TRIANGLESTRIP = 5,
	D3DPT_TRIANGLEFAN = 6,
	D3DPT_FORCE_DWORD = 0x7fffffff
} D3DPRIMITIVETYPE;
#endif

typedef enum _D3DRENDERSTATETYPE {
	D3DRS_ZENABLE = 7,
	D3DRS_FILLMODE = 8,
	D3DRS_SHADEMODE = 9,
	D3DRS_ZWRITEENABLE = 14,
	D3DRS_ALPHATESTENABLE = 15,
	D3DRS_LASTPIXEL = 16,
	D3DRS_SRCBLEND = 19,
	D3DRS_DESTBLEND = 20,
	D3DRS_CULLMODE = 22,
	D3DRS_ZFUNC = 23,
	D3DRS_ALPHAREF = 24,
	D3DRS_ALPHAFUNC = 25,
	D3DRS_DITHERENABLE = 26,
	D3DRS_ALPHABLENDENABLE = 27,
	D3DRS_FOGENABLE = 28,
	D3DRS_SPECULARENABLE = 29,
	D3DRS_FOGCOLOR = 34,
	D3DRS_FOGTABLEMODE = 35,
	D3DRS_FOGSTART = 36,
	D3DRS_FOGEND = 37,
	D3DRS_FOGDENSITY = 38,
	D3DRS_RANGEFOGENABLE = 48,
	D3DRS_STENCILENABLE = 52,
	D3DRS_STENCILFAIL = 53,
	D3DRS_STENCILZFAIL = 54,
	D3DRS_STENCILPASS = 55,
	D3DRS_STENCILFUNC = 56,
	D3DRS_STENCILREF = 57,
	D3DRS_STENCILMASK = 58,
	D3DRS_STENCILWRITEMASK = 59,
	D3DRS_TEXTUREFACTOR = 60,
	D3DRS_WRAP0 = 128,
	D3DRS_WRAP1 = 129,
	D3DRS_CLIPPING = 136,
	D3DRS_LIGHTING = 137,
	D3DRS_AMBIENT = 139,
	D3DRS_FOGVERTEXMODE = 140,
	D3DRS_COLORVERTEX = 141,
	D3DRS_LOCALVIEWER = 142,
	D3DRS_NORMALIZENORMALS = 143,
	D3DRS_DIFFUSEMATERIALSOURCE = 145,
	D3DRS_SPECULARMATERIALSOURCE = 146,
	D3DRS_AMBIENTMATERIALSOURCE = 147,
	D3DRS_EMISSIVEMATERIALSOURCE = 148,
	D3DRS_VERTEXBLEND = 151,
	D3DRS_CLIPPLANEENABLE = 152,
	D3DRS_POINTSIZE = 154,
	D3DRS_POINTSIZE_MIN = 155,
	D3DRS_POINTSPRITEENABLE = 156,
	D3DRS_POINTSCALEENABLE = 157,
	D3DRS_POINTSCALE_A = 158,
	D3DRS_POINTSCALE_B = 159,
	D3DRS_POINTSCALE_C = 160,
	D3DRS_MULTISAMPLEANTIALIAS = 161,
	D3DRS_MULTISAMPLEMASK = 162,
	D3DRS_PATCHEDGESTYLE = 163,
	D3DRS_DEBUGMONITORTOKEN = 165,
	D3DRS_POINTSIZE_MAX = 166,
	D3DRS_INDEXEDVERTEXBLENDENABLE = 167,
	D3DRS_COLORWRITEENABLE = 168,
	D3DRS_TWEENFACTOR = 170,
	D3DRS_BLENDOP = 171,
	D3DRS_POSITIONDEGREE = 172,
	D3DRS_NORMALDEGREE = 173,
	D3DRS_SCISSORTESTENABLE = 174,
	D3DRS_SLOPESCALEDEPTHBIAS = 175,
	D3DRS_ANTIALIASEDLINEENABLE = 176,
	D3DRS_MINTESSELLATIONLEVEL = 178,
	D3DRS_MAXTESSELLATIONLEVEL = 179,
	D3DRS_ADAPTIVETESS_X = 180,
	D3DRS_ADAPTIVETESS_Y = 181,
	D3DRS_ADAPTIVETESS_Z = 182,
	D3DRS_ADAPTIVETESS_W = 183,
	D3DRS_ENABLEADAPTIVETESSELLATION = 184,
	D3DRS_TWOSIDEDSTENCILMODE = 185,
	D3DRS_CCW_STENCILFAIL = 186,
	D3DRS_CCW_STENCILZFAIL = 187,
	D3DRS_CCW_STENCILPASS = 188,
	D3DRS_CCW_STENCILFUNC = 189,
	D3DRS_COLORWRITEENABLE1 = 190,
	D3DRS_COLORWRITEENABLE2 = 191,
	D3DRS_COLORWRITEENABLE3 = 192,
	D3DRS_BLENDFACTOR = 193,
	D3DRS_SRGBWRITEENABLE = 194,
	D3DRS_DEPTHBIAS = 195,
	D3DRS_WRAP8 = 198,
	D3DRS_SEPARATEALPHABLENDENABLE = 206,
	D3DRS_SRCBLENDALPHA = 207,
	D3DRS_DESTBLENDALPHA = 208,
	D3DRS_BLENDOPALPHA = 209,
	D3DRS_FORCE_DWORD = 0x7fffffff
} D3DRENDERSTATETYPE;

#define D3DRS_COUNT 210

typedef enum _D3DBLEND {
	D3DBLEND_ZERO = 1,
	D3DBLEND_ONE = 2,
	D3DBLEND_SRCCOLOR = 3,
	D3DBLEND_INVSRCCOLOR = 4,
	D3DBLEND_SRCALPHA = 5,
	D3DBLEND_INVSRCALPHA = 6,
	D3DBLEND_DESTALPHA = 7,
	D3DBLEND_INVDESTALPHA = 8,
	D3DBLEND_DESTCOLOR = 9,
	D3DBLEND_INVDESTCOLOR = 10,
	D3DBLEND_SRCALPHASAT = 11,
	D3DBLEND_BOTHSRCALPHA = 12,
	D3DBLEND_BOTHINVSRCALPHA = 13,
	D3DBLEND_BLENDFACTOR = 14,
	D3DBLEND_INVBLENDFACTOR = 15,
	D3DBLEND_FORCE_DWORD = 0x7fffffff
} D3DBLEND;

typedef enum _D3DBLENDOP {
	D3DBLENDOP_ADD = 1,
	D3DBLENDOP_SUBTRACT = 2,
	D3DBLENDOP_REVSUBTRACT = 3,
	D3DBLENDOP_MIN = 4,
	D3DBLENDOP_MAX = 5,
	D3DBLENDOP_FORCE_DWORD = 0x7fffffff
} D3DBLENDOP;

typedef enum _D3DCULL {
	D3DCULL_NONE = 1,
	D3DCULL_CW = 2,
	D3DCULL_CCW = 3,
	D3DCULL_FORCE_DWORD = 0x7fffffff
} D3DCULL;

typedef enum _D3DFILLMODE {
	D3DFILL_POINT = 1,
	D3DFILL_WIREFRAME = 2,
	D3DFILL_SOLID = 3,
	D3DFILL_FORCE_DWORD = 0x7fffffff
} D3DFILLMODE;

typedef enum _D3DCMPFUNC {
	D3DCMP_NEVER = 1,
	D3DCMP_LESS = 2,
	D3DCMP_EQUAL = 3,
	D3DCMP_LESSEQUAL = 4,
	D3DCMP_GREATER = 5,
	D3DCMP_NOTEQUAL = 6,
	D3DCMP_GREATEREQUAL = 7,
	D3DCMP_ALWAYS = 8,
	D3DCMP_FORCE_DWORD = 0x7fffffff
} D3DCMPFUNC;

typedef enum _D3DSTENCILOP {
	D3DSTENCILOP_KEEP = 1,
	D3DSTENCILOP_ZERO = 2,
	D3DSTENCILOP_REPLACE = 3,
	D3DSTENCILOP_INCRSAT = 4,
	D3DSTENCILOP_DECRSAT = 5,
	D3DSTENCILOP_INVERT = 6,
	D3DSTENCILOP_INCR = 7,
	D3DSTENCILOP_DECR = 8,
	D3DSTENCILOP_FORCE_DWORD = 0x7fffffff
} D3DSTENCILOP;

typedef enum _D3DZBUFFERTYPE {
	D3DZB_FALSE = 0,
	D3DZB_TRUE = 1,
	D3DZB_USEW = 2,
	D3DZB_FORCE_DWORD = 0x7fffffff
} D3DZBUFFERTYPE;

typedef enum _D3DSHADEMODE {
	D3DSHADE_FLAT = 1,
	D3DSHADE_GOURAUD = 2,
	D3DSHADE_PHONG = 3,
	D3DSHADE_FORCE_DWORD = 0x7fffffff
} D3DSHADEMODE;

#define D3DCOLORWRITEENABLE_RED (1L << 0)
#define D3DCOLORWRITEENABLE_GREEN (1L << 1)
#define D3DCOLORWRITEENABLE_BLUE (1L << 2)
#define D3DCOLORWRITEENABLE_ALPHA (1L << 3)

typedef enum _D3DSAMPLERSTATETYPE {
	D3DSAMP_ADDRESSU = 1,
	D3DSAMP_ADDRESSV = 2,
	D3DSAMP_ADDRESSW = 3,
	D3DSAMP_BORDERCOLOR = 4,
	D3DSAMP_MAGFILTER = 5,
	D3DSAMP_MINFILTER = 6,
	D3DSAMP_MIPFILTER = 7,
	D3DSAMP_MIPMAPLODBIAS = 8,
	D3DSAMP_MAXMIPLEVEL = 9,
	D3DSAMP_MAXANISOTROPY = 10,
	D3DSAMP_SRGBTEXTURE = 11,
	D3DSAMP_ELEMENTINDEX = 12,
	D3DSAMP_DMAPOFFSET = 13,
	D3DSAMP_FORCE_DWORD = 0x7fffffff
} D3DSAMPLERSTATETYPE;

#define D3DSAMP_COUNT 14
#define D3DDMAPSAMPLER 256
#define D3DVERTEXTEXTURESAMPLER0 (D3DDMAPSAMPLER + 1)
#define D3DVERTEXTEXTURESAMPLER1 (D3DDMAPSAMPLER + 2)
#define D3DVERTEXTEXTURESAMPLER2 (D3DDMAPSAMPLER + 3)
#define D3DVERTEXTEXTURESAMPLER3 (D3DDMAPSAMPLER + 4)

typedef enum _D3DTEXTUREADDRESS {
	D3DTADDRESS_WRAP = 1,
	D3DTADDRESS_MIRROR = 2,
	D3DTADDRESS_CLAMP = 3,
	D3DTADDRESS_BORDER = 4,
	D3DTADDRESS_MIRRORONCE = 5,
	D3DTADDRESS_FORCE_DWORD = 0x7fffffff
} D3DTEXTUREADDRESS;

typedef enum _D3DTEXTUREFILTERTYPE {
	D3DTEXF_NONE = 0,
	D3DTEXF_POINT = 1,
	D3DTEXF_LINEAR = 2,
	D3DTEXF_ANISOTROPIC = 3,
	D3DTEXF_PYRAMIDALQUAD = 6,
	D3DTEXF_GAUSSIANQUAD = 7,
	D3DTEXF_CONVOLUTIONMONO = 8,
	D3DTEXF_FORCE_DWORD = 0x7fffffff
} D3DTEXTUREFILTERTYPE;

typedef enum _D3DDECLTYPE {
	D3DDECLTYPE_FLOAT1 = 0,
	D3DDECLTYPE_FLOAT2 = 1,
	D3DDECLTYPE_FLOAT3 = 2,
	D3DDECLTYPE_FLOAT4 = 3,
	D3DDECLTYPE_D3DCOLOR = 4,
	D3DDECLTYPE_UBYTE4 = 5,
	D3DDECLTYPE_SHORT2 = 6,
	D3DDECLTYPE_SHORT4 = 7,
	D3DDECLTYPE_UBYTE4N = 8,
	D3DDECLTYPE_SHORT2N = 9,
	D3DDECLTYPE_SHORT4N = 10,
	D3DDECLTYPE_USHORT2N = 11,
	D3DDECLTYPE_USHORT4N = 12,
	D3DDECLTYPE_UDEC3 = 13,
	D3DDECLTYPE_DEC3N = 14,
	D3DDECLTYPE_FLOAT16_2 = 15,
	D3DDECLTYPE_FLOAT16_4 = 16,
	D3DDECLTYPE_UNUSED = 17
} D3DDECLTYPE;

typedef enum _D3DDECLMETHOD {
	D3DDECLMETHOD_DEFAULT = 0,
	D3DDECLMETHOD_PARTIALU,
	D3DDECLMETHOD_PARTIALV,
	D3DDECLMETHOD_CROSSUV,
	D3DDECLMETHOD_UV,
	D3DDECLMETHOD_LOOKUP,
	D3DDECLMETHOD_LOOKUPPRESAMPLED
} D3DDECLMETHOD;

typedef enum _D3DDECLUSAGE {
	D3DDECLUSAGE_POSITION = 0,
	D3DDECLUSAGE_BLENDWEIGHT,
	D3DDECLUSAGE_BLENDINDICES,
	D3DDECLUSAGE_NORMAL,
	D3DDECLUSAGE_PSIZE,
	D3DDECLUSAGE_TEXCOORD,
	D3DDECLUSAGE_TANGENT,
	D3DDECLUSAGE_BINORMAL,
	D3DDECLUSAGE_TESSFACTOR,
	D3DDECLUSAGE_POSITIONT,
	D3DDECLUSAGE_COLOR,
	D3DDECLUSAGE_FOG,
	D3DDECLUSAGE_DEPTH,
	D3DDECLUSAGE_SAMPLE
} D3DDECLUSAGE;

typedef struct _D3DVERTEXELEMENT9 {
	WORD Stream;
	WORD Offset;
	BYTE Type;
	BYTE Method;
	BYTE Usage;
	BYTE UsageIndex;
} D3DVERTEXELEMENT9, *LPD3DVERTEXELEMENT9;

#define D3DDECL_END() { 0xFF, 0, D3DDECLTYPE_UNUSED, 0, 0, 0 }
#define MAXD3DDECLLENGTH 64

#define D3DCLEAR_TARGET 0x00000001L
#define D3DCLEAR_ZBUFFER 0x00000002L
#define D3DCLEAR_STENCIL 0x00000004L

typedef enum _D3DRESOURCETYPE {
	D3DRTYPE_SURFACE = 1,
	D3DRTYPE_VOLUME = 2,
	D3DRTYPE_TEXTURE = 3,
	D3DRTYPE_VOLUMETEXTURE = 4,
	D3DRTYPE_CUBETEXTURE = 5,
	D3DRTYPE_VERTEXBUFFER = 6,
	D3DRTYPE_INDEXBUFFER = 7,
	D3DRTYPE_FORCE_DWORD = 0x7fffffff
} D3DRESOURCETYPE;

typedef enum _D3DCUBEMAP_FACES {
	D3DCUBEMAP_FACE_POSITIVE_X = 0,
	D3DCUBEMAP_FACE_NEGATIVE_X = 1,
	D3DCUBEMAP_FACE_POSITIVE_Y = 2,
	D3DCUBEMAP_FACE_NEGATIVE_Y = 3,
	D3DCUBEMAP_FACE_POSITIVE_Z = 4,
	D3DCUBEMAP_FACE_NEGATIVE_Z = 5,
	D3DCUBEMAP_FACE_FORCE_DWORD = 0x7fffffff
} D3DCUBEMAP_FACES;

typedef enum _D3DMULTISAMPLE_TYPE {
	D3DMULTISAMPLE_NONE = 0,
	D3DMULTISAMPLE_NONMASKABLE = 1,
	D3DMULTISAMPLE_2_SAMPLES = 2,
	D3DMULTISAMPLE_3_SAMPLES = 3,
	D3DMULTISAMPLE_4_SAMPLES = 4,
	D3DMULTISAMPLE_5_SAMPLES = 5,
	D3DMULTISAMPLE_6_SAMPLES = 6,
	D3DMULTISAMPLE_7_SAMPLES = 7,
	D3DMULTISAMPLE_8_SAMPLES = 8,
	D3DMULTISAMPLE_16_SAMPLES = 16,
	D3DMULTISAMPLE_FORCE_DWORD = 0x7fffffff
} D3DMULTISAMPLE_TYPE;

typedef enum _D3DQUERYTYPE {
	D3DQUERYTYPE_VCACHE = 4,
	D3DQUERYTYPE_RESOURCEMANAGER = 5,
	D3DQUERYTYPE_VERTEXSTATS = 6,
	D3DQUERYTYPE_EVENT = 8,
	D3DQUERYTYPE_OCCLUSION = 9,
	D3DQUERYTYPE_TIMESTAMP = 10,
	D3DQUERYTYPE_TIMESTAMPDISJOINT = 11,
	D3DQUERYTYPE_TIMESTAMPFREQ = 12,
	D3DQUERYTYPE_PIPELINETIMINGS = 13,
	D3DQUERYTYPE_INTERFACETIMINGS = 14,
	D3DQUERYTYPE_VERTEXTIMINGS = 15,
	D3DQUERYTYPE_PIXELTIMINGS = 16,
	D3DQUERYTYPE_BANDWIDTHTIMINGS = 17,
	D3DQUERYTYPE_CACHEUTILIZATION = 18
} D3DQUERYTYPE;

#define D3DISSUE_END (1 << 0)
#define D3DISSUE_BEGIN (1 << 1)
#define D3DGETDATA_FLUSH (1 << 0)

typedef enum _D3DDEVTYPE {
	D3DDEVTYPE_HAL = 1,
	D3DDEVTYPE_REF = 2,
	D3DDEVTYPE_SW = 3,
	D3DDEVTYPE_NULLREF = 4,
	D3DDEVTYPE_FORCE_DWORD = 0x7fffffff
} D3DDEVTYPE;

typedef enum _D3DSWAPEFFECT {
	D3DSWAPEFFECT_DISCARD = 1,
	D3DSWAPEFFECT_FLIP = 2,
	D3DSWAPEFFECT_COPY = 3,
	D3DSWAPEFFECT_FORCE_DWORD = 0x7fffffff
} D3DSWAPEFFECT;

typedef enum _D3DBACKBUFFER_TYPE {
	D3DBACKBUFFER_TYPE_MONO = 0,
	D3DBACKBUFFER_TYPE_LEFT = 1,
	D3DBACKBUFFER_TYPE_RIGHT = 2,
	D3DBACKBUFFER_TYPE_FORCE_DWORD = 0x7fffffff
} D3DBACKBUFFER_TYPE;

#define D3DADAPTER_DEFAULT 0
#define D3DCREATE_FPU_PRESERVE 0x00000002L
#define D3DCREATE_MULTITHREADED 0x00000004L
#define D3DCREATE_PUREDEVICE 0x00000010L
#define D3DCREATE_SOFTWARE_VERTEXPROCESSING 0x00000020L
#define D3DCREATE_HARDWARE_VERTEXPROCESSING 0x00000040L
#define D3DCREATE_MIXED_VERTEXPROCESSING 0x00000080L
#define D3DCREATE_DISABLE_DRIVER_MANAGEMENT 0x00000100L
#define D3DCREATE_NOWINDOWCHANGES 0x00000800L

#define D3DPRESENT_INTERVAL_DEFAULT 0x00000000L
#define D3DPRESENT_INTERVAL_ONE 0x00000001L
#define D3DPRESENT_INTERVAL_TWO 0x00000002L
#define D3DPRESENT_INTERVAL_IMMEDIATE 0x80000000L
#define D3DPRESENT_RATE_DEFAULT 0x00000000
#define D3DPRESENT_DONOTWAIT 0x00000001L
#define D3DPRESENTFLAG_LOCKABLE_BACKBUFFER 0x00000001
#define D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL 0x00000002

typedef struct _D3DPRESENT_PARAMETERS_ {
	UINT BackBufferWidth;
	UINT BackBufferHeight;
	D3DFORMAT BackBufferFormat;
	UINT BackBufferCount;
	D3DMULTISAMPLE_TYPE MultiSampleType;
	DWORD MultiSampleQuality;
	D3DSWAPEFFECT SwapEffect;
	HWND hDeviceWindow;
	WINBOOL Windowed;
	WINBOOL EnableAutoDepthStencil;
	D3DFORMAT AutoDepthStencilFormat;
	DWORD Flags;
	UINT FullScreen_RefreshRateInHz;
	UINT PresentationInterval;
} D3DPRESENT_PARAMETERS;

typedef struct _D3DDISPLAYMODE {
	UINT Width;
	UINT Height;
	UINT RefreshRate;
	D3DFORMAT Format;
} D3DDISPLAYMODE;

typedef struct _D3DADAPTER_IDENTIFIER9 {
	char Driver[512];
	char Description[512];
	char DeviceName[32];
	LARGE_INTEGER DriverVersion;
	DWORD VendorId;
	DWORD DeviceId;
	DWORD SubSysId;
	DWORD Revision;
	GUID DeviceIdentifier;
	DWORD WHQLLevel;
} D3DADAPTER_IDENTIFIER9;

typedef struct _D3DSURFACE_DESC {
	D3DFORMAT Format;
	D3DRESOURCETYPE Type;
	DWORD Usage;
	D3DPOOL Pool;
	D3DMULTISAMPLE_TYPE MultiSampleType;
	DWORD MultiSampleQuality;
	UINT Width;
	UINT Height;
} D3DSURFACE_DESC;

typedef struct _D3DVOLUME_DESC {
	D3DFORMAT Format;
	D3DRESOURCETYPE Type;
	DWORD Usage;
	D3DPOOL Pool;
	UINT Width;
	UINT Height;
	UINT Depth;
} D3DVOLUME_DESC;

typedef struct _D3DVERTEXBUFFER_DESC {
	D3DFORMAT Format;
	D3DRESOURCETYPE Type;
	DWORD Usage;
	D3DPOOL Pool;
	UINT Size;
	DWORD FVF;
} D3DVERTEXBUFFER_DESC;

typedef struct _D3DINDEXBUFFER_DESC {
	D3DFORMAT Format;
	D3DRESOURCETYPE Type;
	DWORD Usage;
	D3DPOOL Pool;
	UINT Size;
} D3DINDEXBUFFER_DESC;

typedef struct _D3DLOCKED_RECT {
	INT Pitch;
	void *pBits;
} D3DLOCKED_RECT;

typedef struct _D3DBOX {
	UINT Left, Top, Right, Bottom, Front, Back;
} D3DBOX;

typedef struct _D3DLOCKED_BOX {
	INT RowPitch;
	INT SlicePitch;
	void *pBits;
} D3DLOCKED_BOX;

typedef struct _D3DVSHADERCAPS2_0 { DWORD Caps; INT DynamicFlowControlDepth, NumTemps, StaticFlowControlDepth; } D3DVSHADERCAPS2_0;
typedef struct _D3DPSHADERCAPS2_0 { DWORD Caps; INT DynamicFlowControlDepth, NumTemps, StaticFlowControlDepth, NumInstructionSlots; } D3DPSHADERCAPS2_0;

typedef struct _D3DCAPS9 {
	D3DDEVTYPE DeviceType;
	UINT AdapterOrdinal;
	DWORD Caps, Caps2, Caps3, PresentationIntervals, CursorCaps, DevCaps;
	DWORD PrimitiveMiscCaps, RasterCaps, ZCmpCaps, SrcBlendCaps, DestBlendCaps, AlphaCmpCaps, ShadeCaps;
	DWORD TextureCaps, TextureFilterCaps, CubeTextureFilterCaps, VolumeTextureFilterCaps;
	DWORD TextureAddressCaps, VolumeTextureAddressCaps, LineCaps;
	DWORD MaxTextureWidth, MaxTextureHeight, MaxVolumeExtent, MaxTextureRepeat, MaxTextureAspectRatio, MaxAnisotropy;
	float MaxVertexW, GuardBandLeft, GuardBandTop, GuardBandRight, GuardBandBottom, ExtentsAdjust;
	DWORD StencilCaps, FVFCaps, TextureOpCaps, MaxTextureBlendStages, MaxSimultaneousTextures;
	DWORD VertexProcessingCaps, MaxActiveLights, MaxUserClipPlanes, MaxVertexBlendMatrices, MaxVertexBlendMatrixIndex;
	float MaxPointSize;
	DWORD MaxPrimitiveCount, MaxVertexIndex, MaxStreams, MaxStreamStride, VertexShaderVersion, MaxVertexShaderConst;
	DWORD PixelShaderVersion;
	float PixelShader1xMaxValue;
	DWORD DevCaps2;
	float MaxNpatchTessellationLevel;
	DWORD Reserved5;
	UINT MasterAdapterOrdinal, AdapterOrdinalInGroup, NumberOfAdaptersInGroup;
	DWORD DeclTypes, NumSimultaneousRTs, StretchRectFilterCaps;
	D3DVSHADERCAPS2_0 VS20Caps;
	D3DPSHADERCAPS2_0 PS20Caps;
	DWORD VertexTextureFilterCaps, MaxVShaderInstructionsExecuted, MaxPShaderInstructionsExecuted;
	DWORD MaxVertexShader30InstructionSlots, MaxPixelShader30InstructionSlots;
} D3DCAPS9;

#define D3DCAPS2_CANAUTOGENMIPMAP 0x40000000L
#define D3DPTEXTURECAPS_POW2 0x00000002L
#define D3DPTEXTURECAPS_NONPOW2CONDITIONAL 0x00000100L
#define D3DPMISCCAPS_COLORWRITEENABLE 0x00000080L
#define D3DPMISCCAPS_SEPARATEALPHABLEND 0x00020000L
#define D3DPMISCCAPS_MRTINDEPENDENTBITDEPTHS 0x00040000L
#define D3DDTCAPS_UBYTE4 0x00000001L
#define D3DDTCAPS_UBYTE4N 0x00000002L
#define D3DDTCAPS_SHORT2N 0x00000004L
#define D3DDTCAPS_SHORT4N 0x00000008L
#define D3DDTCAPS_USHORT2N 0x00000010L
#define D3DDTCAPS_USHORT4N 0x00000020L
#define D3DDTCAPS_UDEC3 0x00000040L
#define D3DDTCAPS_DEC3N 0x00000080L
#define D3DDTCAPS_FLOAT16_2 0x00000100L
#define D3DDTCAPS_FLOAT16_4 0x00000200L
#define D3DVS_VERSION(major, minor) (0xFFFE0000 | ((major) << 8) | (minor))
#define D3DPS_VERSION(major, minor) (0xFFFF0000 | ((major) << 8) | (minor))
#define D3DSHADER_VERSION_MAJOR(v) (((v) >> 8) & 0xFF)
#define D3DSHADER_VERSION_MINOR(v) (((v) >> 0) & 0xFF)

#define D3D_OK S_OK
#define D3DERR_DEVICELOST MAKE_HRESULT(1, 0x876, 2152)
#define D3DERR_DEVICENOTRESET MAKE_HRESULT(1, 0x876, 2153)
#define D3DERR_NOTAVAILABLE MAKE_HRESULT(1, 0x876, 2154)
#define D3DERR_INVALIDCALL MAKE_HRESULT(1, 0x876, 2156)
#define D3DERR_OUTOFVIDEOMEMORY MAKE_HRESULT(1, 0x876, 380)
#define D3DERR_WASSTILLDRAWING MAKE_HRESULT(1, 0x876, 540)
#define D3DERR_NOTFOUND MAKE_HRESULT(1, 0x876, 2150)
#define D3DERR_DRIVERINTERNALERROR MAKE_HRESULT(1, 0x876, 2087)
#define D3DOK_NOAUTOGEN MAKE_HRESULT(0, 0x876, 2159)
#define E_HANDLE ((HRESULT)0x80070006)
#define E_PENDING ((HRESULT)0x8000000A)

// ----------------------------------------------------------------------------
// Interfaces

namespace m9 { struct Device; struct Resource; struct Texture; struct Surface; struct Buffer; struct Shader; struct Query; struct SwapChain; struct Decl; }

struct IUnknownM9 {
	virtual ~IUnknownM9() {}
	ULONG AddRef();
	ULONG Release();
	void BindRef() { ++m_binds; }
	void BindRelease();
	HRESULT QueryInterface(REFIID, void **) { return E_NOTIMPL; }
public:
	long m_refs = 1;
	long m_binds = 0;
	friend struct m9::Device;
};

struct IDirect3DDevice9;

struct IDirect3DResource9 : IUnknownM9 {
	HRESULT GetDevice(IDirect3DDevice9 **ppDevice);
	virtual D3DRESOURCETYPE GetType() = 0;
	DWORD SetPriority(DWORD p) { DWORD o = m_priority; m_priority = p; return o; }
	DWORD GetPriority() { return m_priority; }
	void PreLoad() {}
	HRESULT SetPrivateData(REFGUID, const void *, DWORD, DWORD) { return S_OK; }
	HRESULT GetPrivateData(REFGUID, void *, DWORD *) { return E_FAIL; }
public:
	DWORD m_priority = 0;
};

struct IDirect3DSurface9;
struct IDirect3DVolume9;

struct IDirect3DBaseTexture9 : IDirect3DResource9 {
	DWORD GetLevelCount();
	DWORD SetLOD(DWORD l) { return 0; }
	DWORD GetLOD() { return 0; }
	void GenerateMipSubLevels();
	HRESULT SetAutoGenFilterType(D3DTEXTUREFILTERTYPE) { return S_OK; }
	D3DTEXTUREFILTERTYPE GetAutoGenFilterType() { return D3DTEXF_LINEAR; }
	m9::Texture *Impl() const { return m_tex; }
	void M9Attach(m9::Texture *t) { m_tex = t; }
public:
	m9::Texture *m_tex = nullptr;
	friend struct m9::Device;
};

struct IDirect3DTexture9 : IDirect3DBaseTexture9 {
	D3DRESOURCETYPE GetType() override { return D3DRTYPE_TEXTURE; }
	HRESULT GetLevelDesc(UINT level, D3DSURFACE_DESC *desc);
	HRESULT GetSurfaceLevel(UINT level, IDirect3DSurface9 **surf);
	HRESULT LockRect(UINT level, D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags);
	HRESULT UnlockRect(UINT level);
	HRESULT AddDirtyRect(const RECT *) { return S_OK; }
	~IDirect3DTexture9();
};

struct IDirect3DCubeTexture9 : IDirect3DBaseTexture9 {
	D3DRESOURCETYPE GetType() override { return D3DRTYPE_CUBETEXTURE; }
	HRESULT GetLevelDesc(UINT level, D3DSURFACE_DESC *desc);
	HRESULT GetCubeMapSurface(D3DCUBEMAP_FACES face, UINT level, IDirect3DSurface9 **surf);
	HRESULT LockRect(D3DCUBEMAP_FACES face, UINT level, D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags);
	HRESULT UnlockRect(D3DCUBEMAP_FACES face, UINT level);
	HRESULT AddDirtyRect(D3DCUBEMAP_FACES, const RECT *) { return S_OK; }
	~IDirect3DCubeTexture9();
};

struct IDirect3DVolumeTexture9 : IDirect3DBaseTexture9 {
	D3DRESOURCETYPE GetType() override { return D3DRTYPE_VOLUMETEXTURE; }
	HRESULT GetLevelDesc(UINT level, D3DVOLUME_DESC *desc);
	HRESULT LockBox(UINT level, D3DLOCKED_BOX *lb, const D3DBOX *box, DWORD flags);
	HRESULT UnlockBox(UINT level);
	HRESULT AddDirtyBox(const D3DBOX *) { return S_OK; }
	~IDirect3DVolumeTexture9();
};

struct IDirect3DSurface9 : IDirect3DResource9 {
	D3DRESOURCETYPE GetType() override { return D3DRTYPE_SURFACE; }
	HRESULT GetDesc(D3DSURFACE_DESC *desc);
	HRESULT LockRect(D3DLOCKED_RECT *lr, const RECT *rect, DWORD flags);
	HRESULT UnlockRect();
	HRESULT GetContainer(REFIID, void **container);
	HRESULT GetDC(HDC *) { return E_NOTIMPL; }
	HRESULT ReleaseDC(HDC) { return E_NOTIMPL; }
	m9::Surface *Impl() const { return m_surf; }
	void M9Attach(m9::Surface *s) { m_surf = s; }
	~IDirect3DSurface9();
	ULONG AddRef();
	ULONG Release();
public:
	m9::Surface *m_surf = nullptr;
	friend struct m9::Device;
	friend struct m9::Texture;
};

struct IDirect3DVertexBuffer9 : IDirect3DResource9 {
	D3DRESOURCETYPE GetType() override { return D3DRTYPE_VERTEXBUFFER; }
	HRESULT Lock(UINT offset, UINT size, void **data, DWORD flags);
	HRESULT Unlock();
	HRESULT GetDesc(D3DVERTEXBUFFER_DESC *desc);
	m9::Buffer *Impl() const { return m_buf; }
	~IDirect3DVertexBuffer9();
public:
	m9::Buffer *m_buf = nullptr;
	friend struct m9::Device;
};

struct IDirect3DIndexBuffer9 : IDirect3DResource9 {
	D3DRESOURCETYPE GetType() override { return D3DRTYPE_INDEXBUFFER; }
	HRESULT Lock(UINT offset, UINT size, void **data, DWORD flags);
	HRESULT Unlock();
	HRESULT GetDesc(D3DINDEXBUFFER_DESC *desc);
	m9::Buffer *Impl() const { return m_buf; }
	~IDirect3DIndexBuffer9();
public:
	m9::Buffer *m_buf = nullptr;
	friend struct m9::Device;
};

struct IDirect3DVertexDeclaration9 : IUnknownM9 {
	HRESULT GetDeclaration(D3DVERTEXELEMENT9 *elements, UINT *count);
	m9::Decl *Impl() const { return m_decl; }
	~IDirect3DVertexDeclaration9();
public:
	m9::Decl *m_decl = nullptr;
	friend struct m9::Device;
};

struct IDirect3DVertexShader9 : IUnknownM9 {
	m9::Shader *Impl() const { return m_shader; }
	void M9Attach(m9::Shader *s) { m_shader = s; }
	~IDirect3DVertexShader9();
public:
	m9::Shader *m_shader = nullptr;
	friend struct m9::Device;
};

struct IDirect3DPixelShader9 : IUnknownM9 {
	m9::Shader *Impl() const { return m_shader; }
	void M9Attach(m9::Shader *s) { m_shader = s; }
	~IDirect3DPixelShader9();
public:
	m9::Shader *m_shader = nullptr;
	friend struct m9::Device;
};

struct IDirect3DQuery9 : IUnknownM9 {
	D3DQUERYTYPE GetType() { return m_type; }
	DWORD GetDataSize();
	HRESULT Issue(DWORD flags);
	HRESULT GetData(void *data, DWORD size, DWORD flags);
	~IDirect3DQuery9();
public:
	D3DQUERYTYPE m_type = D3DQUERYTYPE_EVENT;
	m9::Query *m_query = nullptr;
	friend struct m9::Device;
};

struct IDirect3DSwapChain9 : IUnknownM9 {
	HRESULT Present(const RECT *src, const RECT *dst, HWND hWnd, const void *dirty, DWORD flags);
	HRESULT GetBackBuffer(UINT idx, D3DBACKBUFFER_TYPE type, IDirect3DSurface9 **surf);
	HRESULT GetPresentParameters(D3DPRESENT_PARAMETERS *pp);
	HRESULT GetDisplayMode(D3DDISPLAYMODE *mode);
	m9::SwapChain *Impl() const { return m_chain; }
	~IDirect3DSwapChain9();
public:
	m9::SwapChain *m_chain = nullptr;
	bool m_owned = true;
	friend struct m9::Device;
};

struct IDirect3D9;

struct IDirect3DDevice9 : IUnknownM9 {
	HRESULT TestCooperativeLevel();
	UINT GetAvailableTextureMem();
	HRESULT EvictManagedResources() { return S_OK; }
	HRESULT GetDirect3D(IDirect3D9 **d3d);
	HRESULT GetDeviceCaps(D3DCAPS9 *caps);
	HRESULT GetDisplayMode(UINT swap, D3DDISPLAYMODE *mode);
	HRESULT CreateAdditionalSwapChain(D3DPRESENT_PARAMETERS *pp, IDirect3DSwapChain9 **chain);
	HRESULT GetSwapChain(UINT idx, IDirect3DSwapChain9 **chain);
	HRESULT Reset(D3DPRESENT_PARAMETERS *pp);
	HRESULT Present(const RECT *src, const RECT *dst, HWND hWnd, const void *dirty);
	HRESULT GetBackBuffer(UINT swap, UINT idx, D3DBACKBUFFER_TYPE type, IDirect3DSurface9 **surf);
	HRESULT SetDialogBoxMode(WINBOOL) { return S_OK; }

	HRESULT CreateTexture(UINT w, UINT h, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DTexture9 **tex, HANDLE *shared);
	HRESULT CreateVolumeTexture(UINT w, UINT h, UINT d, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DVolumeTexture9 **tex, HANDLE *shared);
	HRESULT CreateCubeTexture(UINT edge, UINT levels, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DCubeTexture9 **tex, HANDLE *shared);
	HRESULT CreateVertexBuffer(UINT length, DWORD usage, DWORD fvf, D3DPOOL pool, IDirect3DVertexBuffer9 **vb, HANDLE *shared);
	HRESULT CreateIndexBuffer(UINT length, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DIndexBuffer9 **ib, HANDLE *shared);
	HRESULT CreateRenderTarget(UINT w, UINT h, D3DFORMAT fmt, D3DMULTISAMPLE_TYPE ms, DWORD quality, WINBOOL lockable, IDirect3DSurface9 **surf, HANDLE *shared);
	HRESULT CreateDepthStencilSurface(UINT w, UINT h, D3DFORMAT fmt, D3DMULTISAMPLE_TYPE ms, DWORD quality, WINBOOL discard, IDirect3DSurface9 **surf, HANDLE *shared);
	HRESULT CreateOffscreenPlainSurface(UINT w, UINT h, D3DFORMAT fmt, D3DPOOL pool, IDirect3DSurface9 **surf, HANDLE *shared);
	HRESULT UpdateSurface(IDirect3DSurface9 *src, const RECT *srcRect, IDirect3DSurface9 *dst, const POINT *dstPoint);
	HRESULT UpdateTexture(IDirect3DBaseTexture9 *src, IDirect3DBaseTexture9 *dst);
	HRESULT GetRenderTargetData(IDirect3DSurface9 *rt, IDirect3DSurface9 *dst);
	HRESULT StretchRect(IDirect3DSurface9 *src, const RECT *srcRect, IDirect3DSurface9 *dst, const RECT *dstRect, D3DTEXTUREFILTERTYPE filter);
	HRESULT ColorFill(IDirect3DSurface9 *surf, const RECT *rect, D3DCOLOR color);
	HRESULT SetRenderTarget(DWORD idx, IDirect3DSurface9 *surf);
	HRESULT GetRenderTarget(DWORD idx, IDirect3DSurface9 **surf);
	HRESULT SetDepthStencilSurface(IDirect3DSurface9 *surf);
	HRESULT GetDepthStencilSurface(IDirect3DSurface9 **surf);
	HRESULT BeginScene();
	HRESULT EndScene();
	HRESULT Clear(DWORD count, const D3DRECT *rects, DWORD flags, D3DCOLOR color, float z, DWORD stencil);
	HRESULT SetViewport(const D3DVIEWPORT9 *vp);
	HRESULT GetViewport(D3DVIEWPORT9 *vp);
	HRESULT SetRenderState(D3DRENDERSTATETYPE state, DWORD value);
	HRESULT GetRenderState(D3DRENDERSTATETYPE state, DWORD *value);
	HRESULT GetTexture(DWORD stage, IDirect3DBaseTexture9 **tex);
	HRESULT SetTexture(DWORD stage, IDirect3DBaseTexture9 *tex);
	HRESULT GetSamplerState(DWORD sampler, D3DSAMPLERSTATETYPE type, DWORD *value);
	HRESULT SetSamplerState(DWORD sampler, D3DSAMPLERSTATETYPE type, DWORD value);
	HRESULT SetScissorRect(const RECT *rect);
	HRESULT GetScissorRect(RECT *rect);
	HRESULT DrawPrimitive(D3DPRIMITIVETYPE type, UINT start, UINT count);
	HRESULT DrawIndexedPrimitive(D3DPRIMITIVETYPE type, INT baseVertex, UINT minIndex, UINT numVertices, UINT startIndex, UINT primCount);
	HRESULT DrawPrimitiveUP(D3DPRIMITIVETYPE type, UINT primCount, const void *data, UINT stride);
	HRESULT DrawIndexedPrimitiveUP(D3DPRIMITIVETYPE type, UINT minIndex, UINT numVertices, UINT primCount, const void *indices, D3DFORMAT indexFmt, const void *data, UINT stride);
	HRESULT CreateVertexDeclaration(const D3DVERTEXELEMENT9 *elements, IDirect3DVertexDeclaration9 **decl);
	HRESULT SetVertexDeclaration(IDirect3DVertexDeclaration9 *decl);
	HRESULT GetVertexDeclaration(IDirect3DVertexDeclaration9 **decl);
	HRESULT SetFVF(DWORD fvf);
	HRESULT CreateVertexShader(const DWORD *function, IDirect3DVertexShader9 **shader);
	HRESULT CreatePixelShader(const DWORD *function, IDirect3DPixelShader9 **shader);
	HRESULT SetVertexShader(IDirect3DVertexShader9 *shader);
	HRESULT GetVertexShader(IDirect3DVertexShader9 **shader);
	HRESULT SetPixelShader(IDirect3DPixelShader9 *shader);
	HRESULT GetPixelShader(IDirect3DPixelShader9 **shader);
	HRESULT SetStreamSource(UINT stream, IDirect3DVertexBuffer9 *vb, UINT offset, UINT stride);
	HRESULT GetStreamSource(UINT stream, IDirect3DVertexBuffer9 **vb, UINT *offset, UINT *stride);
	HRESULT SetStreamSourceFreq(UINT stream, UINT setting);
	HRESULT SetIndices(IDirect3DIndexBuffer9 *ib);
	HRESULT GetIndices(IDirect3DIndexBuffer9 **ib);
	HRESULT CreateQuery(D3DQUERYTYPE type, IDirect3DQuery9 **query);
	HRESULT SetVertexShaderConstantF(UINT reg, const float *data, UINT count);
	HRESULT SetPixelShaderConstantF(UINT reg, const float *data, UINT count);
	HRESULT SetVertexShaderConstantB(UINT reg, const WINBOOL *data, UINT count);
	HRESULT SetPixelShaderConstantB(UINT reg, const WINBOOL *data, UINT count);
	HRESULT SetVertexShaderConstantI(UINT reg, const int *data, UINT count);
	HRESULT SetPixelShaderConstantI(UINT reg, const int *data, UINT count);

	m9::Device *Impl() const { return m_dev; }
	~IDirect3DDevice9();
public:
	m9::Device *m_dev = nullptr;
	friend struct m9::Device;
	friend struct IDirect3D9;
};

struct IDirect3D9 : IUnknownM9 {
	UINT GetAdapterCount();
	HRESULT GetAdapterIdentifier(UINT adapter, DWORD flags, D3DADAPTER_IDENTIFIER9 *id);
	UINT GetAdapterModeCount(UINT adapter, D3DFORMAT fmt);
	HRESULT EnumAdapterModes(UINT adapter, D3DFORMAT fmt, UINT mode, D3DDISPLAYMODE *dm);
	HRESULT GetAdapterDisplayMode(UINT adapter, D3DDISPLAYMODE *dm);
	HRESULT CheckDeviceType(UINT adapter, D3DDEVTYPE type, D3DFORMAT adapterFmt, D3DFORMAT bbFmt, WINBOOL windowed);
	HRESULT CheckDeviceFormat(UINT adapter, D3DDEVTYPE type, D3DFORMAT adapterFmt, DWORD usage, D3DRESOURCETYPE rtype, D3DFORMAT checkFmt);
	HRESULT CheckDeviceMultiSampleType(UINT adapter, D3DDEVTYPE type, D3DFORMAT fmt, WINBOOL windowed, D3DMULTISAMPLE_TYPE ms, DWORD *quality);
	HRESULT CheckDepthStencilMatch(UINT adapter, D3DDEVTYPE type, D3DFORMAT adapterFmt, D3DFORMAT rtFmt, D3DFORMAT dsFmt);
	HRESULT GetDeviceCaps(UINT adapter, D3DDEVTYPE type, D3DCAPS9 *caps);
	HMONITOR GetAdapterMonitor(UINT adapter) { return nullptr; }
	HRESULT CreateDevice(UINT adapter, D3DDEVTYPE type, HWND hFocus, DWORD flags, D3DPRESENT_PARAMETERS *pp, IDirect3DDevice9 **dev);
};

IDirect3D9 *Direct3DCreate9(UINT sdk);

typedef IDirect3D9 *LPDIRECT3D9;
typedef IDirect3DDevice9 *LPDIRECT3DDEVICE9;
typedef IDirect3DResource9 *LPDIRECT3DRESOURCE9;
typedef IDirect3DBaseTexture9 *LPDIRECT3DBASETEXTURE9;
typedef IDirect3DTexture9 *LPDIRECT3DTEXTURE9;
typedef IDirect3DCubeTexture9 *LPDIRECT3DCUBETEXTURE9;
typedef IDirect3DVolumeTexture9 *LPDIRECT3DVOLUMETEXTURE9;
typedef IDirect3DSurface9 *LPDIRECT3DSURFACE9;
typedef IDirect3DVertexBuffer9 *LPDIRECT3DVERTEXBUFFER9;
typedef IDirect3DIndexBuffer9 *LPDIRECT3DINDEXBUFFER9;
typedef IDirect3DVertexDeclaration9 *LPDIRECT3DVERTEXDECLARATION9;
typedef IDirect3DVertexShader9 *LPDIRECT3DVERTEXSHADER9;
typedef IDirect3DPixelShader9 *LPDIRECT3DPIXELSHADER9;
typedef IDirect3DQuery9 *LPDIRECT3DQUERY9;
typedef IDirect3DSwapChain9 *LPDIRECT3DSWAPCHAIN9;
