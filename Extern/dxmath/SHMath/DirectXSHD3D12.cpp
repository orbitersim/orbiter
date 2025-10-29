//-------------------------------------------------------------------------------------
// DirectXSHD3D12.cpp -- C++ Spherical Harmonics Math Library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/p/?LinkId=262885
//-------------------------------------------------------------------------------------

#ifdef _MSC_VER
#pragma warning( disable : 4616 4619 4061 4265 4626 5039 )
// C4616/C4619 #pragma warning warnings
// C4061 numerator 'identifier' in switch of enum 'enumeration' is not explicitly handled by a case label
// C4265 class has virtual functions, but destructor is not virtual
// C4626 assignment operator was implicitly defined as deleted
// C5039 pointer or reference to potentially throwing function passed to extern C function under - EHc
#endif

#ifdef __MINGW32__
#include <unknwn.h>
#endif

#ifdef USING_DIRECTX_HEADERS
#include <directx/d3d12.h>
#else
#include <d3d12.h>
#endif

#include "DirectXSH.h"

#include <DirectXPackedVector.h>

#include <cassert>
#include <memory>
#include <malloc.h>

#include <wrl/client.h>

#ifdef __clang__
#pragma clang diagnostic ignored "-Wcovered-switch-default"
#pragma clang diagnostic ignored "-Wswitch-enum"
#pragma clang diagnostic ignored "-Wunknown-warning-option"
#pragma clang diagnostic ignored "-Wunsafe-buffer-usage"
#endif

using namespace DirectX;

using Microsoft::WRL::ComPtr;

namespace
{
    struct aligned_deleter { void operator()(void* p) { _aligned_free(p); } };

    using ScopedAlignedArrayXMVECTOR = std::unique_ptr<DirectX::XMVECTOR, aligned_deleter>;

    //-------------------------------------------------------------------------------------
    // This code is lifted from DirectXTex http://go.microsoft.com/fwlink/?LinkId=248926
    // If you need additional DXGI format support, see DirectXTexConvert.cpp
    //-------------------------------------------------------------------------------------
#define LOAD_SCANLINE( type, func )\
        if ( size >= sizeof(type) )\
        {\
            const type * __restrict sPtr = reinterpret_cast<const type*>(pSource);\
            for( size_t icount = 0; icount < ( size - sizeof(type) + 1 ); icount += sizeof(type) )\
            {\
                if ( dPtr >= ePtr ) break;\
                *(dPtr++) = func( sPtr++ );\
            }\
            return true;\
        }\
        return false;

#define LOAD_SCANLINE3( type, func, defvec )\
        if ( size >= sizeof(type) )\
        {\
            const type * __restrict sPtr = reinterpret_cast<const type*>(pSource);\
            for( size_t icount = 0; icount < ( size - sizeof(type) + 1 ); icount += sizeof(type) )\
            {\
                XMVECTOR v = func( sPtr++ );\
                if ( dPtr >= ePtr ) break;\
                *(dPtr++) = XMVectorSelect( defvec, v, g_XMSelect1110 );\
            }\
            return true;\
        }\
        return false;

#define LOAD_SCANLINE2( type, func, defvec )\
        if ( size >= sizeof(type) )\
        {\
            const type * __restrict sPtr = reinterpret_cast<const type*>(pSource);\
            for( size_t icount = 0; icount < ( size - sizeof(type) + 1 ); icount += sizeof(type) )\
            {\
                XMVECTOR v = func( sPtr++ );\
                if ( dPtr >= ePtr ) break;\
                *(dPtr++) = XMVectorSelect( defvec, v, g_XMSelect1100 );\
            }\
            return true;\
        }\
        return false;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 6101)
#endif
    _Success_(return)
        bool LoadScanline(
            _Out_writes_(count) DirectX::XMVECTOR* pDestination,
            size_t count,
            _In_reads_bytes_(size) LPCVOID pSource,
            size_t size,
            DXGI_FORMAT format)
    {
        assert(pDestination && count > 0 && ((reinterpret_cast<uintptr_t>(pDestination) & 0xF) == 0));
        assert(pSource && size > 0);

        using namespace DirectX::PackedVector;

        XMVECTOR* __restrict dPtr = pDestination;
        if (!dPtr)
            return false;

        const XMVECTOR* ePtr = pDestination + count;

        switch (format)
        {
        case DXGI_FORMAT_R32G32B32A32_FLOAT:
            {
                size_t msize = (size > (sizeof(XMVECTOR)*count)) ? (sizeof(XMVECTOR)*count) : size;
                memcpy_s(dPtr, sizeof(XMVECTOR)*count, pSource, msize);
            }
            return true;

        case DXGI_FORMAT_R32G32B32_FLOAT:
            LOAD_SCANLINE3(XMFLOAT3, XMLoadFloat3, g_XMIdentityR3)

        case DXGI_FORMAT_R16G16B16A16_FLOAT:
            LOAD_SCANLINE(XMHALF4, XMLoadHalf4)

        case DXGI_FORMAT_R32G32_FLOAT:
            LOAD_SCANLINE2(XMFLOAT2, XMLoadFloat2, g_XMIdentityR3)

        case DXGI_FORMAT_R11G11B10_FLOAT:
            LOAD_SCANLINE3(XMFLOAT3PK, XMLoadFloat3PK, g_XMIdentityR3)

        case DXGI_FORMAT_R16G16_FLOAT:
            LOAD_SCANLINE2(XMHALF2, XMLoadHalf2, g_XMIdentityR3)

        case DXGI_FORMAT_R32_FLOAT:
            if (size >= sizeof(float))
            {
                const float* __restrict sPtr = reinterpret_cast<const float*>(pSource);
                for (size_t icount = 0; icount < size; icount += sizeof(float))
                {
                    XMVECTOR v = XMLoadFloat(sPtr++);
                    if (dPtr >= ePtr) break;
                    *(dPtr++) = XMVectorSelect(g_XMIdentityR3, v, g_XMSelect1000);
                }
                return true;
            }
            return false;

        case DXGI_FORMAT_R16_FLOAT:
            if (size >= sizeof(HALF))
            {
                const HALF * __restrict sPtr = reinterpret_cast<const HALF*>(pSource);
                for (size_t icount = 0; icount < size; icount += sizeof(HALF))
                {
                    if (dPtr >= ePtr) break;
                    *(dPtr++) = XMVectorSet(XMConvertHalfToFloat(*sPtr++), 0.f, 0.f, 1.f);
                }
                return true;
            }
            return false;

        default:
            return false;
        }
    }
#ifdef _MSC_VER
#pragma warning(pop)
#endif
} // namespace anonymous

//-------------------------------------------------------------------------------------
// Projects a function represented in a cube map into spherical harmonics.
//
// http://msdn.microsoft.com/en-us/library/windows/desktop/ff476300.aspx
//-------------------------------------------------------------------------------------
_Use_decl_annotations_
HRESULT DirectX::SHProjectCubeMap(
    size_t order,
    const D3D12_RESOURCE_DESC& desc,
    const D3D12_SUBRESOURCE_DATA cubeMap[6],
    float *resultR,
    float *resultG,
    float *resultB) noexcept
{
    if (order < XM_SH_MINORDER || order > XM_SH_MAXORDER)
        return E_INVALIDARG;

    if (desc.Dimension != D3D12_RESOURCE_DIMENSION_TEXTURE2D
        || (desc.DepthOrArraySize != 6)
        || (desc.Width != desc.Height)
        || (desc.SampleDesc.Count > 1))
        return E_FAIL;

    switch (desc.Format)
    {
    case DXGI_FORMAT_R32G32B32A32_FLOAT:
    case DXGI_FORMAT_R32G32B32_FLOAT:
    case DXGI_FORMAT_R16G16B16A16_FLOAT:
    case DXGI_FORMAT_R32G32_FLOAT:
    case DXGI_FORMAT_R11G11B10_FLOAT:
    case DXGI_FORMAT_R16G16_FLOAT:
    case DXGI_FORMAT_R32_FLOAT:
    case DXGI_FORMAT_R16_FLOAT:
        // See LoadScanline to support more pixel formats
        break;

    default:
        return E_FAIL;
    }

    //--- Setup for SH projection
    ScopedAlignedArrayXMVECTOR scanline(reinterpret_cast<XMVECTOR*>(_aligned_malloc(static_cast<size_t>(sizeof(XMVECTOR)*desc.Width), 16)));
    if (!scanline)
        return E_OUTOFMEMORY;

    assert(desc.Width > 0);
    float fSize = static_cast<float>(desc.Width);
    float fPicSize = 1.0f / fSize;

    // index from [0,W-1], f(0) maps to -1 + 1/W, f(W-1) maps to 1 - 1/w
    // linear function x*S +B, 1st constraint means B is (-1+1/W), plug into
    // second and solve for S: S = 2*(1-1/W)/(W-1). The old code that did
    // this was incorrect - but only for computing the differential solid
    // angle, where the final value was 1.0 instead of 1-1/w...

    float fB = -1.0f + 1.0f / fSize;
    float fS = (desc.Width > 1) ? (2.0f*(1.0f - 1.0f / fSize) / (fSize - 1.0f)) : 0.f;

    // clear out accumulation variables
    float fWt = 0.0f;

    if (resultR)
        memset(resultR, 0, sizeof(float)*order*order);
    if (resultG)
        memset(resultG, 0, sizeof(float)*order*order);
    if (resultB)
        memset(resultB, 0, sizeof(float)*order*order);

    float shBuff[XM_SH_MAXORDER*XM_SH_MAXORDER] = {};
    float shBuffB[XM_SH_MAXORDER*XM_SH_MAXORDER] = {};

    //--- Process each face of the cubemap
    for (UINT face = 0; face < 6; ++face)
    {
        if (!cubeMap[face].pData)
            return E_POINTER;

        const uint8_t *pSrc = reinterpret_cast<const uint8_t*>(cubeMap[face].pData);
        for (UINT y = 0; y < desc.Height; ++y)
        {
            XMVECTOR* ptr = scanline.get();
            if (!LoadScanline(ptr, static_cast<size_t>(desc.Width), pSrc, static_cast<size_t>(cubeMap[face].RowPitch), desc.Format))
            {
                return E_FAIL;
            }

            const float v = float(y) * fS + fB;

            XMVECTOR* pixel = ptr;
            for (UINT x = 0; x < desc.Width; ++x, ++pixel)
            {
                const float u = float(x) * fS + fB;

                float ix, iy, iz;
                switch (face)
                {
                case 0: // Positive X
                    iz = 1.0f - (2.0f * float(x) + 1.0f) * fPicSize;
                    iy = 1.0f - (2.0f * float(y) + 1.0f) * fPicSize;
                    ix = 1.0f;
                    break;

                case 1: // Negative X
                    iz = -1.0f + (2.0f * float(x) + 1.0f) * fPicSize;
                    iy = 1.0f - (2.0f * float(y) + 1.0f) * fPicSize;
                    ix = -1;
                    break;

                case 2: // Positive Y
                    iz = -1.0f + (2.0f * float(y) + 1.0f) * fPicSize;
                    iy = 1.0f;
                    ix = -1.0f + (2.0f * float(x) + 1.0f) * fPicSize;
                    break;

                case 3: // Negative Y
                    iz = 1.0f - (2.0f * float(y) + 1.0f) * fPicSize;
                    iy = -1.0f;
                    ix = -1.0f + (2.0f * float(x) + 1.0f) * fPicSize;
                    break;

                case 4: // Positive Z
                    iz = 1.0f;
                    iy = 1.0f - (2.0f * float(y) + 1.0f) * fPicSize;
                    ix = -1.0f + (2.0f * float(x) + 1.0f) * fPicSize;
                    break;

                case 5: // Negative Z
                    iz = -1.0f;
                    iy = 1.0f - (2.0f * float(y) + 1.0f) * fPicSize;
                    ix = 1.0f - (2.0f * float(x) + 1.0f) * fPicSize;
                    break;

                default:
                    ix = iy = iz = 0.f;
                    assert(false);
                    break;
                }

                XMVECTOR dir = XMVectorSet(ix, iy, iz, 0);
                dir = XMVector3Normalize(dir);

                const float fDiffSolid = 4.0f / ((1.0f + u * u + v * v)*sqrtf(1.0f + u * u + v * v));
                fWt += fDiffSolid;

                XMSHEvalDirection(shBuff, order, dir);

                XMFLOAT3A clr;
                XMStoreFloat3A(&clr, *pixel);

                if (resultR) XMSHAdd(resultR, order, resultR, XMSHScale(shBuffB, order, shBuff, clr.x*fDiffSolid));
                if (resultG) XMSHAdd(resultG, order, resultG, XMSHScale(shBuffB, order, shBuff, clr.y*fDiffSolid));
                if (resultB) XMSHAdd(resultB, order, resultB, XMSHScale(shBuffB, order, shBuff, clr.z*fDiffSolid));
            }

            pSrc += cubeMap[face].RowPitch;
        }
    }

    const float fNormProj = (4.0f*XM_PI) / fWt;

    if (resultR) XMSHScale(resultR, order, resultR, fNormProj);
    if (resultG) XMSHScale(resultG, order, resultG, fNormProj);
    if (resultB) XMSHScale(resultB, order, resultB, fNormProj);

    return S_OK;
}
