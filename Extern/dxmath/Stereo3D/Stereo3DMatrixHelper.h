//-------------------------------------------------------------------------------------
// Stereo3DMatrixHelper.h -- SIMD C++ Math helper for Stereo 3D matrices
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//-------------------------------------------------------------------------------------

#pragma once

#include "DirectXMath.h"

namespace DirectX
{
    // Enumeration for stereo channels (left and right).
    enum STEREO_CHANNEL
    {
        STEREO_CHANNEL_LEFT = 0,
        STEREO_CHANNEL_RIGHT
    };

    // Enumeration for stereo mode (normal or inverted).
    enum STEREO_MODE
    {
        STEREO_MODE_NORMAL = 0,
        STEREO_MODE_INVERTED,
    };

    //------------------------------------------------------------------------------
    //
    // Stereo calibration settings
    //
    // * Viewer distance to the display
    // * Physical display size
    // * Render resolution
    //
    // The stereo separation factor indicates how much separation is between the left and right
    // eyes.  0 is no separation, 1 is full separation. It defaults to 1.0.
    //
    // The debug stereo exaggeration factor indicates how much to increase the interocular spacing and
    // maximum acuity angle from comfortable defaults.  For retail builds, this value should always
    // be 1.0, but during development, on small screens, this value can be raised to up to 2.0 in
    // order to exaggerate the 3D effect.  Values over 1.0 may cause discomfort on normal sized
    // displays. It defaults to 1.0.
    //
    struct STEREO_PARAMETERS
    {
        float fViewerDistanceInches;
        float fDisplaySizeInches;
        float fPixelResolutionWidth;
        float fPixelResolutionHeight;
        float fStereoSeparationFactor;
        float fStereoExaggerationFactor;
    };

    void StereoCreateDefaultParameters(STEREO_PARAMETERS& stereoParameters);

    XMMATRIX StereoProjectionFovLH(_In_opt_ const STEREO_PARAMETERS* pStereoParameters,
        STEREO_CHANNEL Channel, float FovAngleY, float AspectRatio, float NearZ, float FarZ,
        STEREO_MODE StereoMode = STEREO_MODE_NORMAL);

    XMMATRIX StereoProjectionFovRH(_In_opt_ const STEREO_PARAMETERS* pStereoParameters,
        STEREO_CHANNEL Channel, float FovAngleY, float AspectRatio, float NearZ, float FarZ,
        STEREO_MODE StereoMode = STEREO_MODE_NORMAL);
}
