//-------------------------------------------------------------------------------------
// DirectXSH.h -- C++ Spherical Harmonics Math Library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/p/?LinkId=262885
//-------------------------------------------------------------------------------------

#pragma once

#define DIRECTX_SHMATH_VERSION 106

#include "DirectXMath.h"

namespace DirectX
{
    constexpr size_t XM_SH_MINORDER = 2;
    constexpr size_t XM_SH_MAXORDER = 6;

    float* XM_CALLCONV XMSHEvalDirection(_Out_writes_(order*order) float *result, _In_ size_t order, _In_ FXMVECTOR dir) noexcept;

    float* XM_CALLCONV XMSHRotate(_Out_writes_(order*order) float *result, _In_ size_t order, _In_ FXMMATRIX rotMatrix, _In_reads_(order*order) const float *input) noexcept;

    float* XMSHRotateZ(_Out_writes_(order*order) float *result, _In_ size_t order, _In_ float angle, _In_reads_(order*order) const float *input) noexcept;

    float* XMSHAdd(_Out_writes_(order*order) float *result, _In_ size_t order, _In_reads_(order*order) const float *inputA, _In_reads_(order*order) const float *inputB) noexcept;

    float* XMSHScale(_Out_writes_(order*order) float *result, _In_ size_t order, _In_reads_(order*order) const float *input, _In_ float scale) noexcept;

    float XMSHDot(_In_ size_t order, _In_reads_(order*order) const float *inputA, _In_reads_(order*order) const float *inputB) noexcept;

    float* XMSHMultiply(_Out_writes_(order*order) float *result, _In_ size_t order, _In_reads_(order*order) const float *inputF, _In_reads_(order*order) const float *inputG) noexcept;

    float* XMSHMultiply2(_Out_writes_(4) float *result, _In_reads_(4) const float *inputF, _In_reads_(4) const float *inputG) noexcept;

    float* XMSHMultiply3(_Out_writes_(9) float *result, _In_reads_(9) const float *inputF, _In_reads_(9) const float *inputG) noexcept;

    float* XMSHMultiply4(_Out_writes_(16) float *result, _In_reads_(16) const float *inputF, _In_reads_(16) const float *inputG) noexcept;

    float* XMSHMultiply5(_Out_writes_(25) float *result, _In_reads_(25) const float *inputF, _In_reads_(25) const float *inputG) noexcept;

    float* XMSHMultiply6(_Out_writes_(36) float *result, _In_reads_(36) const float *inputF, _In_reads_(36) const float *inputG) noexcept;

    bool XM_CALLCONV XMSHEvalDirectionalLight(
        _In_ size_t order, _In_ FXMVECTOR dir, _In_ FXMVECTOR color,
        _Out_writes_(order*order) float *resultR, _Out_writes_opt_(order*order) float *resultG, _Out_writes_opt_(order*order) float *resultB) noexcept;

    bool XM_CALLCONV XMSHEvalSphericalLight(
        _In_ size_t order, _In_ FXMVECTOR pos, _In_ float radius, _In_ FXMVECTOR color,
        _Out_writes_(order*order) float *resultR, _Out_writes_opt_(order*order) float *resultG, _Out_writes_opt_(order*order) float *resultB) noexcept;

    bool XM_CALLCONV XMSHEvalConeLight(
        _In_ size_t order, _In_ FXMVECTOR dir, _In_ float radius, _In_ FXMVECTOR color,
        _Out_writes_(order*order) float *resultR, _Out_writes_opt_(order*order) float *resultG, _Out_writes_opt_(order*order) float *resultB) noexcept;

    bool XM_CALLCONV XMSHEvalHemisphereLight(
        _In_ size_t order, _In_ FXMVECTOR dir, _In_ FXMVECTOR topColor, _In_ FXMVECTOR bottomColor,
        _Out_writes_(order*order) float *resultR, _Out_writes_opt_(order*order) float *resultG, _Out_writes_opt_(order*order) float *resultB) noexcept;

#if defined(__d3d11_h__) || defined(__d3d11_x_h__)
    HRESULT SHProjectCubeMap(
        _In_ ID3D11DeviceContext *context, _In_ size_t order, _In_ ID3D11Texture2D *cubeMap,
        _Out_writes_opt_(order*order) float *resultR, _Out_writes_opt_(order*order) float *resultG, _Out_writes_opt_(order*order) float *resultB) noexcept;
#endif

#if defined(__d3d12_h__) || defined(__d3d12_x_h__) || defined(__XBOX_D3D12_X__)
    HRESULT SHProjectCubeMap(
        _In_ size_t order, _In_ const D3D12_RESOURCE_DESC& desc, _In_ const D3D12_SUBRESOURCE_DATA cubeMap[6],
        _Out_writes_opt_(order*order) float *resultR, _Out_writes_opt_(order*order) float *resultG, _Out_writes_opt_(order*order) float *resultB) noexcept;
#endif
} // namespace DirectX
