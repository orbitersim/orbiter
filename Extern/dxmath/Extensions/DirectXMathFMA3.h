//-------------------------------------------------------------------------------------
// DirectXMathFMA3.h -- FMA3 extensions for SIMD C++ Math library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------

#pragma once

#if defined(_M_ARM) || defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64) || defined(_M_ARM64EC) || __arm__ || __aarch64__
#error FMA3 not supported on ARM platform
#endif

#include <DirectXMath.h>

namespace DirectX
{

    namespace FMA3
    {

        inline bool XMVerifyFMA3Support()
        {
            // Should return true for AMD "Pildriver" and Intel "Haswell" processors
            // with OS support for AVX (Windows 7 Service Pack 1, Windows Server 2008 R2 Service Pack 1, Windows 8, Windows Server 2012)

            // See http://msdn.microsoft.com/en-us/library/hskdteyh.aspx
            int CPUInfo[4] = { -1 };
        #if (defined(__clang__) || defined(__GNUC__)) && defined(__cpuid)
            __cpuid(0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
        #else
            __cpuid(CPUInfo, 0);
        #endif

            if (CPUInfo[0] < 1)
                return false;

        #if (defined(__clang__) || defined(__GNUC__)) && defined(__cpuid)
            __cpuid(1, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
        #else
            __cpuid(CPUInfo, 1);
        #endif

            // We check for FMA3, AVX, OSXSAVE
            return ((CPUInfo[2] & 0x18001000) == 0x18001000);
        }


        //-------------------------------------------------------------------------------------
        // Vector
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVectorMultiplyAdd
        (
            FXMVECTOR V1,
            FXMVECTOR V2,
            FXMVECTOR V3
        )
        {
            return _mm_fmadd_ps(V1, V2, V3);
        }

        inline XMVECTOR XM_CALLCONV XMVectorNegativeMultiplySubtract
        (
            FXMVECTOR V1,
            FXMVECTOR V2,
            FXMVECTOR V3
        )
        {
            return _mm_fnmadd_ps(V1, V2, V3);
        }


        //-------------------------------------------------------------------------------------
        // Vector2
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVector2Transform
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_fmadd_ps(vResult, M.r[1], M.r[3]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVector2TransformCoord
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_fmadd_ps(vResult, M.r[1], M.r[3]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            XMVECTOR W = _mm_permute_ps(vResult, _MM_SHUFFLE(3, 3, 3, 3));
            vResult = _mm_div_ps(vResult, W);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVector2TransformNormal
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_mul_ps(vResult, M.r[1]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            return vResult;
        }


        //-------------------------------------------------------------------------------------
        // Vector3
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVector3Transform
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(2, 2, 2, 2)); // Z
            vResult = _mm_fmadd_ps(vResult, M.r[2], M.r[3]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_fmadd_ps(vTemp, M.r[1], vResult);
            vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVector3TransformCoord
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(2, 2, 2, 2)); // Z
            vResult = _mm_fmadd_ps(vResult, M.r[2], M.r[3]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_fmadd_ps(vTemp, M.r[1], vResult);
            vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            XMVECTOR W = _mm_permute_ps(vResult, _MM_SHUFFLE(3, 3, 3, 3));
            vResult = _mm_div_ps(vResult, W);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVector3TransformNormal
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(2, 2, 2, 2)); // Z
            vResult = _mm_mul_ps(vResult, M.r[2]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_fmadd_ps(vTemp, M.r[1], vResult);
            vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            return vResult;
        }

        XMMATRIX XM_CALLCONV XMMatrixMultiply(CXMMATRIX M1, CXMMATRIX M2);

        inline XMVECTOR XM_CALLCONV XMVector3Project
        (
            FXMVECTOR V,
            float    ViewportX,
            float    ViewportY,
            float    ViewportWidth,
            float    ViewportHeight,
            float    ViewportMinZ,
            float    ViewportMaxZ,
            CXMMATRIX Projection,
            CXMMATRIX View,
            CXMMATRIX World
        )
        {
            const float HalfViewportWidth = ViewportWidth * 0.5f;
            const float HalfViewportHeight = ViewportHeight * 0.5f;

            XMVECTOR Scale = XMVectorSet(HalfViewportWidth, -HalfViewportHeight, ViewportMaxZ - ViewportMinZ, 0.0f);
            XMVECTOR Offset = XMVectorSet(ViewportX + HalfViewportWidth, ViewportY + HalfViewportHeight, ViewportMinZ, 0.0f);

            XMMATRIX Transform = FMA3::XMMatrixMultiply(World, View);
            Transform = FMA3::XMMatrixMultiply(Transform, Projection);

            XMVECTOR Result = FMA3::XMVector3TransformCoord(V, Transform);

            Result = FMA3::XMVectorMultiplyAdd(Result, Scale, Offset);

            return Result;
        }

        inline XMVECTOR XM_CALLCONV XMVector3Unproject
        (
            FXMVECTOR V,
            float     ViewportX,
            float     ViewportY,
            float     ViewportWidth,
            float     ViewportHeight,
            float     ViewportMinZ,
            float     ViewportMaxZ,
            CXMMATRIX Projection,
            CXMMATRIX View,
            CXMMATRIX World
        )
        {
            static const XMVECTORF32 D = { { { -1.0f, 1.0f, 0.0f, 0.0f } } };

            XMVECTOR Scale = XMVectorSet(ViewportWidth * 0.5f, -ViewportHeight * 0.5f, ViewportMaxZ - ViewportMinZ, 1.0f);
            Scale = XMVectorReciprocal(Scale);

            XMVECTOR Offset = XMVectorSet(-ViewportX, -ViewportY, -ViewportMinZ, 0.0f);
            Offset = FMA3::XMVectorMultiplyAdd(Scale, Offset, D.v);

            XMMATRIX Transform = FMA3::XMMatrixMultiply(World, View);
            Transform = FMA3::XMMatrixMultiply(Transform, Projection);
            Transform = XMMatrixInverse(nullptr, Transform);

            XMVECTOR Result = FMA3::XMVectorMultiplyAdd(V, Scale, Offset);

            return FMA3::XMVector3TransformCoord(Result, Transform);
        }


        //-------------------------------------------------------------------------------------
        // Vector4
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVector4Transform
        (
            FXMVECTOR V,
            CXMMATRIX M
        )
        {
            XMVECTOR vResult = _mm_permute_ps(V, _MM_SHUFFLE(3, 3, 3, 3)); // W
            vResult = _mm_mul_ps(vResult, M.r[3]);
            XMVECTOR vTemp = _mm_permute_ps(V, _MM_SHUFFLE(2, 2, 2, 2)); // Z
            vResult = _mm_fmadd_ps(vTemp, M.r[2], vResult);
            vTemp = _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1)); // Y
            vResult = _mm_fmadd_ps(vTemp, M.r[1], vResult);
            vTemp = _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0)); // X
            vResult = _mm_fmadd_ps(vTemp, M.r[0], vResult);
            return vResult;
        }


        //-------------------------------------------------------------------------------------
        // Matrix
        //-------------------------------------------------------------------------------------

        inline XMMATRIX XM_CALLCONV XMMatrixMultiply
        (
            CXMMATRIX M1,
            CXMMATRIX M2
        )
        {
            XMMATRIX mResult;
            // Use vW to hold the original row
            XMVECTOR vW = M1.r[0];
            // Splat the component X,Y,Z then W
            XMVECTOR vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            XMVECTOR vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            XMVECTOR vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            // Perform the operation on the first row
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            mResult.r[0] = vX;
            // Repeat for the other 3 rows
            vW = M1.r[1];
            vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            mResult.r[1] = vX;
            vW = M1.r[2];
            vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            mResult.r[2] = vX;
            vW = M1.r[3];
            vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            mResult.r[3] = vX;
            return mResult;
        }

        inline XMMATRIX XM_CALLCONV XMMatrixMultiplyTranspose
        (
            FXMMATRIX M1,
            CXMMATRIX M2
        )
        {
            // Use vW to hold the original row
            XMVECTOR vW = M1.r[0];
            // Splat the component X,Y,Z then W
            XMVECTOR vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            XMVECTOR vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            XMVECTOR vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            // Perform the operation on the first row
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            __m128 r0 = vX;
            // Repeat for the other 3 rows
            vW = M1.r[1];
            vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            __m128 r1 = vX;
            vW = M1.r[2];
            vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            __m128 r2 = vX;
            vW = M1.r[3];
            vX = _mm_permute_ps(vW, _MM_SHUFFLE(0, 0, 0, 0));
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            __m128 r3 = vX;

            // x.x,x.y,y.x,y.y
            XMVECTOR vTemp1 = _mm_shuffle_ps(r0, r1, _MM_SHUFFLE(1, 0, 1, 0));
            // x.z,x.w,y.z,y.w
            XMVECTOR vTemp3 = _mm_shuffle_ps(r0, r1, _MM_SHUFFLE(3, 2, 3, 2));
            // z.x,z.y,w.x,w.y
            XMVECTOR vTemp2 = _mm_shuffle_ps(r2, r3, _MM_SHUFFLE(1, 0, 1, 0));
            // z.z,z.w,w.z,w.w
            XMVECTOR vTemp4 = _mm_shuffle_ps(r2, r3, _MM_SHUFFLE(3, 2, 3, 2));

            XMMATRIX mResult;
            // x.x,y.x,z.x,w.x
            mResult.r[0] = _mm_shuffle_ps(vTemp1, vTemp2, _MM_SHUFFLE(2, 0, 2, 0));
            // x.y,y.y,z.y,w.y
            mResult.r[1] = _mm_shuffle_ps(vTemp1, vTemp2, _MM_SHUFFLE(3, 1, 3, 1));
            // x.z,y.z,z.z,w.z
            mResult.r[2] = _mm_shuffle_ps(vTemp3, vTemp4, _MM_SHUFFLE(2, 0, 2, 0));
            // x.w,y.w,z.w,w.w
            mResult.r[3] = _mm_shuffle_ps(vTemp3, vTemp4, _MM_SHUFFLE(3, 1, 3, 1));
            return mResult;
        }

    } // namespace FMA3

} // namespace DirectX;
