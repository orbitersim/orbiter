//-------------------------------------------------------------------------------------
// Stereo3DMatrixHelper.cpp -- SIMD C++ Math helper for Stereo 3D matricies
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//-------------------------------------------------------------------------------------

#include "Stereo3DMatrixHelper.h"

using namespace DirectX;

namespace
{
    inline bool StereoProjectionHelper
    (
        const STEREO_PARAMETERS& stereoParameters,
        _Out_ float* fVirtualProjection,
        _Out_ float* zNearWidth,
        _Out_ float* zNearHeight,
        float FovAngleY,
        float AspectRatio,
        float NearZ
    )
    {
        // note that most people have difficulty fusing images into 3D
        // if the separation equals even just the human average. by
        // reducing the separation (interocular distance) by 1/2, we
        // guarantee a larger subset of people will see full 3D

        // the conservative setting should always be used. the only problem
        // with the conservative setting is that the 3D effect will be less
        // impressive on smaller screens (which makes sense, since your eye
        // cannot be tricked as easily based on the smaller fov). to simulate
        // the effect of a larger screen, use the liberal settings (debug only)

        // Conservative Settings: * max acuity angle: 0.8f degrees * interoc distance: 1.25 inches

        // Liberal Settings: * max acuity angle: 1.6f degrees * interoc distance: 2.5f inches

        // maximum visual accuity angle allowed is 3.2 degrees for
        // a physical scene, and 1.6 degrees for a virtual one.
        // thus we cannot allow an object to appear any closer to
        // the viewer than 1.6 degrees (divided by two for most
        // half-angle calculations)

        static const float fMaxStereoDistance = 780; // inches (should be between 10 and 20m)
        static const float fMaxVisualAcuityAngle = 1.6f * (XM_PI / 180.0f);  // radians
        static const float fInterocularDistance = 1.25f; // inches

        float fDisplayHeight = stereoParameters.fDisplaySizeInches / sqrtf(AspectRatio * AspectRatio + 1.0f);
        float fDisplayWidth = fDisplayHeight * AspectRatio;
        float fHalfInterocular = 0.5f * fInterocularDistance * stereoParameters.fStereoExaggerationFactor;
        float fHalfPixelWidth = fDisplayWidth / stereoParameters.fPixelResolutionWidth * 0.5f;
        float fHalfMaximumAcuityAngle = fMaxVisualAcuityAngle * 0.5f * stereoParameters.fStereoExaggerationFactor;
        // float fHalfWidth = fDisplayWidth * 0.5f;

        float fMaxSeparationAcuityAngle = atanf(fHalfInterocular / fMaxStereoDistance);
        float fMaxSeparationDistance = fHalfPixelWidth / tanf(fMaxSeparationAcuityAngle);
        float fRefinedMaxStereoDistance = fMaxStereoDistance - fMaxSeparationDistance;
        float fFovHalfAngle = FovAngleY / 2.0f;

        bool ComfortableResult = true;
        if (fRefinedMaxStereoDistance < 0.0f || fMaxSeparationDistance > 0.1f * fMaxStereoDistance)
        {
            // Pixel resolution is too low to offer a comfortable stereo experience
            ComfortableResult = false;
        }

        float fRefinedMaxSeparationAcuityAngle = atanf(fHalfInterocular / (fRefinedMaxStereoDistance));
        float fPhysicalZNearDistance = fHalfInterocular / tanf(fHalfMaximumAcuityAngle);
        // float fScalingFactor = fHalfMaximumAcuityAngle / atanf(fHalfInterocular / stereoParameters.fViewerDistanceInches);

        float fNearZSeparation = tanf(fRefinedMaxSeparationAcuityAngle) * (fRefinedMaxStereoDistance - fPhysicalZNearDistance);
        // float fNearZSeparation2 = fHalfInterocular * (fRefinedMaxStereoDistance - fPhysicalZNearDistance) / fRefinedMaxStereoDistance;

        (*zNearHeight) = cosf(fFovHalfAngle) / sinf(fFovHalfAngle);
        (*zNearWidth) = (*zNearHeight) / AspectRatio;
        (*fVirtualProjection) = (fNearZSeparation * NearZ * (*zNearWidth * 4.0f)) / (2.0f * NearZ);

        return ComfortableResult;
    }
}

//------------------------------------------------------------------------------

void DirectX::StereoCreateDefaultParameters
(
    STEREO_PARAMETERS& stereoParameters
)
{
    // Default assumption is 1920x1200 resolution, a 22" LCD monitor, and a 2' viewing distance
    stereoParameters.fViewerDistanceInches = 24.0f;
    stereoParameters.fPixelResolutionWidth = 1920.0f;
    stereoParameters.fPixelResolutionHeight = 1200.0f;
    stereoParameters.fDisplaySizeInches = 22.0f;

    stereoParameters.fStereoSeparationFactor = 1.0f;
    stereoParameters.fStereoExaggerationFactor = 1.0f;
}

//------------------------------------------------------------------------------

XMMATRIX DirectX::StereoProjectionFovLH
(
    _In_opt_ const STEREO_PARAMETERS* pStereoParameters,
    STEREO_CHANNEL Channel,
    float FovAngleY,
    float AspectRatio,
    float NearZ,
    float FarZ,
    STEREO_MODE StereoMode
)
{
    assert(Channel == STEREO_CHANNEL_LEFT || Channel == STEREO_CHANNEL_RIGHT);
    assert(StereoMode == STEREO_MODE_NORMAL || StereoMode == STEREO_MODE_INVERTED);
    assert(!XMScalarNearEqual(FovAngleY, 0.0f, 0.00001f * 2.0f));
    assert(!XMScalarNearEqual(AspectRatio, 0.0f, 0.00001f));
    assert(!XMScalarNearEqual(FarZ, NearZ, 0.00001f));

    STEREO_PARAMETERS DefaultParameters = {};
    if (pStereoParameters == nullptr)
    {
        StereoCreateDefaultParameters(DefaultParameters);
        pStereoParameters = &DefaultParameters;
    }

    assert(pStereoParameters->fStereoSeparationFactor >= 0.0f && pStereoParameters->fStereoSeparationFactor <= 1.0f);
    assert(pStereoParameters->fStereoExaggerationFactor >= 1.0f && pStereoParameters->fStereoExaggerationFactor <= 2.0f);

    float fVirtualProjection = 0.0f;
    float zNearWidth = 0.0f;
    float zNearHeight = 0.0f;
    StereoProjectionHelper(*pStereoParameters, &fVirtualProjection, &zNearWidth, &zNearHeight, FovAngleY, AspectRatio, NearZ);

    fVirtualProjection *= pStereoParameters->fStereoSeparationFactor; // incorporate developer defined bias

    //
    // By applying a translation, we are forcing our cameras to be parallel
    //

    float fInvertedAngle = atanf(fVirtualProjection / (2.0f * NearZ));

    XMMATRIX proj = XMMatrixPerspectiveFovLH(FovAngleY, AspectRatio, NearZ, FarZ);

    XMMATRIX patchedProjection;
    if (Channel == STEREO_CHANNEL_LEFT)
    {
        if (StereoMode > STEREO_MODE_NORMAL)
        {
            XMMATRIX rots = XMMatrixRotationY(fInvertedAngle);
            XMMATRIX trans = XMMatrixTranslation(-fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(XMMatrixMultiply(rots, trans), proj);
        }
        else
        {
            XMMATRIX trans = XMMatrixTranslation(-fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(trans, proj);
        }
    }
    else
    {
        if (StereoMode > STEREO_MODE_NORMAL)
        {
            XMMATRIX rots = XMMatrixRotationY(-fInvertedAngle);
            XMMATRIX trans = XMMatrixTranslation(fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(XMMatrixMultiply(rots, trans), proj);
        }
        else
        {
            XMMATRIX trans = XMMatrixTranslation(fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(trans, proj);
        }
    }

    return patchedProjection;
}

//------------------------------------------------------------------------------

XMMATRIX DirectX::StereoProjectionFovRH
(
    _In_opt_ const STEREO_PARAMETERS* pStereoParameters,
    STEREO_CHANNEL Channel,
    float FovAngleY,
    float AspectRatio,
    float NearZ,
    float FarZ,
    STEREO_MODE StereoMode
)
{
    assert(Channel == STEREO_CHANNEL_LEFT || Channel == STEREO_CHANNEL_RIGHT);
    assert(StereoMode == STEREO_MODE_NORMAL || StereoMode == STEREO_MODE_INVERTED);
    assert(!XMScalarNearEqual(FovAngleY, 0.0f, 0.00001f * 2.0f));
    assert(!XMScalarNearEqual(AspectRatio, 0.0f, 0.00001f));
    assert(!XMScalarNearEqual(FarZ, NearZ, 0.00001f));

    STEREO_PARAMETERS DefaultParameters = {};
    if (pStereoParameters == nullptr)
    {
        StereoCreateDefaultParameters(DefaultParameters);
        pStereoParameters = &DefaultParameters;
    }

    assert(pStereoParameters->fStereoSeparationFactor >= 0.0f && pStereoParameters->fStereoSeparationFactor <= 1.0f);
    assert(pStereoParameters->fStereoExaggerationFactor >= 1.0f && pStereoParameters->fStereoExaggerationFactor <= 2.0f);

    float fVirtualProjection = 0.0f;
    float zNearWidth = 0.0f;
    float zNearHeight = 0.0f;
    StereoProjectionHelper(*pStereoParameters, &fVirtualProjection, &zNearWidth, &zNearHeight, FovAngleY, AspectRatio, NearZ);

    fVirtualProjection *= pStereoParameters->fStereoSeparationFactor; // incorporate developer defined bias

    //
    // By applying a translation, we are forcing our cameras to be parallel
    //

    float fInvertedAngle = atanf(fVirtualProjection / (2.0f * NearZ));

    XMMATRIX proj = XMMatrixPerspectiveFovRH(FovAngleY, AspectRatio, NearZ, FarZ);

    //
    // By applying a translation, we are forcing our cameras to be parallel
    //

    XMMATRIX patchedProjection;
    if (Channel == STEREO_CHANNEL_LEFT)
    {
        if (StereoMode > STEREO_MODE_NORMAL)
        {
            XMMATRIX rots = XMMatrixRotationY(fInvertedAngle);
            XMMATRIX trans = XMMatrixTranslation(-fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(XMMatrixMultiply(rots, trans), proj);
        }
        else
        {
            XMMATRIX trans = XMMatrixTranslation(-fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(trans, proj);
        }
    }
    else
    {
        if (StereoMode > STEREO_MODE_NORMAL)
        {
            XMMATRIX rots = XMMatrixRotationY(-fInvertedAngle);
            XMMATRIX trans = XMMatrixTranslation(fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(XMMatrixMultiply(rots, trans), proj);
        }
        else
        {
            XMMATRIX trans = XMMatrixTranslation(fVirtualProjection, 0, 0);
            patchedProjection = XMMatrixMultiply(trans, proj);
        }
    }

    return patchedProjection;
}
