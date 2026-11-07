#pragma once
#include "windows.h"

#ifndef DUMMYUNIONNAMEN
#define DUMMYUNIONNAMEN(n)
#endif

#define DDSD_CAPS 0x00000001
#define DDSD_HEIGHT 0x00000002
#define DDSD_WIDTH 0x00000004
#define DDSD_PITCH 0x00000008
#define DDSD_PIXELFORMAT 0x00001000
#define DDSD_MIPMAPCOUNT 0x00020000
#define DDSD_LINEARSIZE 0x00080000
#define DDSD_DEPTH 0x00800000

#define DDPF_ALPHAPIXELS 0x00000001
#define DDPF_ALPHA 0x00000002
#define DDPF_FOURCC 0x00000004
#define DDPF_RGB 0x00000040
#define DDPF_LUMINANCE 0x00020000

#pragma pack(push, 1)
typedef struct _DDCOLORKEY {
	DWORD dwColorSpaceLowValue;
	DWORD dwColorSpaceHighValue;
} DDCOLORKEY;

typedef struct _DDPIXELFORMAT {
	DWORD dwSize;
	DWORD dwFlags;
	DWORD dwFourCC;
	union { DWORD dwRGBBitCount; DWORD dwYUVBitCount; DWORD dwZBufferBitDepth; DWORD dwAlphaBitDepth; DWORD dwLuminanceBitCount; };
	union { DWORD dwRBitMask; DWORD dwYBitMask; DWORD dwLuminanceBitMask; };
	union { DWORD dwGBitMask; DWORD dwUBitMask; };
	union { DWORD dwBBitMask; DWORD dwVBitMask; };
	union { DWORD dwRGBAlphaBitMask; DWORD dwYUVAlphaBitMask; DWORD dwLuminanceAlphaBitMask; };
} DDPIXELFORMAT;

typedef struct _DDSCAPS2 {
	DWORD dwCaps;
	DWORD dwCaps2;
	DWORD dwCaps3;
	union { DWORD dwCaps4; DWORD dwVolumeDepth; };
} DDSCAPS2;

typedef struct _DDSURFACEDESC2 {
	DWORD dwSize;
	DWORD dwFlags;
	DWORD dwHeight;
	DWORD dwWidth;
	union { LONG lPitch; DWORD dwLinearSize; };
	union { DWORD dwBackBufferCount; DWORD dwDepth; };
	union { DWORD dwMipMapCount; DWORD dwRefreshRate; DWORD dwSrcVBHandle; };
	DWORD dwAlphaBitDepth;
	DWORD dwReserved;
	DWORD lpSurface;
	union { DDCOLORKEY ddckCKDestOverlay; DWORD dwEmptyFaceColor; };
	DDCOLORKEY ddckCKDestBlt;
	DDCOLORKEY ddckCKSrcOverlay;
	DDCOLORKEY ddckCKSrcBlt;
	union { DDPIXELFORMAT ddpfPixelFormat; DWORD dwFVF; };
	DDSCAPS2 ddsCaps;
	DWORD dwTextureStage;
} DDSURFACEDESC2;
#pragma pack(pop)

static_assert(sizeof(DDSURFACEDESC2) == 124, "DDS header size");
