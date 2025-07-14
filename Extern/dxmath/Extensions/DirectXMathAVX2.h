//-------------------------------------------------------------------------------------
// DirectXMathAVX2.h -- AVX2 extensions for SIMD C++ Math library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------

#pragma once

#if defined(_M_ARM) || defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64) || defined(_M_ARM64EC) || __arm__ || __aarch64__
#error AVX2 not supported on ARM platform
#endif

#include <DirectXMath.h>
#include <DirectXPackedVector.h>

namespace DirectX
{

    namespace AVX2
    {

        inline bool XMVerifyAVX2Support()
        {
            // Should return true for AMD "Excavator", Intel "Haswell" or later processors
            // with OS support for AVX (Windows 7 Service Pack 1, Windows Server 2008 R2 Service Pack 1, Windows 8, Windows Server 2012)

            // See http://msdn.microsoft.com/en-us/library/hskdteyh.aspx
            int CPUInfo[4] = { -1 };
        #if (defined(__clang__) || defined(__GNUC__)) && defined(__cpuid)
            __cpuid(0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
        #else
            __cpuid(CPUInfo, 0);
        #endif

            if (CPUInfo[0] < 7)
                return false;

        #if (defined(__clang__) || defined(__GNUC__)) && defined(__cpuid)
            __cpuid(1, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
        #else
            __cpuid(CPUInfo, 1);
        #endif

            // We check for F16C, FMA3, AVX, OSXSAVE, SSSE4.1, and SSE3
            if ((CPUInfo[2] & 0x38081001) != 0x38081001)
                return false;

        #if defined(__clang__) || defined(__GNUC__)
            __cpuid_count(7, 0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
        #else
            __cpuidex(CPUInfo, 7, 0);
        #endif

            return ((CPUInfo[1] & 0x20) == 0x20);
        }


        //-------------------------------------------------------------------------------------
        // Vector
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVectorReplicatePtr(_In_  const float *pValue)
        {
            return _mm_broadcast_ss(pValue);
        }

        inline XMVECTOR XM_CALLCONV XMVectorSplatX(FXMVECTOR V)
        {
            return _mm_broadcastss_ps(V);
        }

        inline XMVECTOR XM_CALLCONV XMVectorSplatY(FXMVECTOR V)
        {
            return _mm_permute_ps(V, _MM_SHUFFLE(1, 1, 1, 1));
        }

        inline XMVECTOR XM_CALLCONV XMVectorSplatZ(FXMVECTOR V)
        {
            return _mm_permute_ps(V, _MM_SHUFFLE(2, 2, 2, 2));
        }

        inline XMVECTOR XM_CALLCONV XMVectorSplatW(FXMVECTOR V)
        {
            return _mm_permute_ps(V, _MM_SHUFFLE(3, 3, 3, 3));
        }

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

        inline XMVECTOR XM_CALLCONV XMVectorSwizzle(FXMVECTOR V, uint32_t E0, uint32_t E1, uint32_t E2, uint32_t E3)
        {
            assert((E0 < 4) && (E1 < 4) && (E2 < 4) && (E3 < 4));
            _Analysis_assume_((E0 < 4) && (E1 < 4) && (E2 < 4) && (E3 < 4));

            unsigned int elem[4] = { E0, E1, E2, E3 };
            __m128i vControl = _mm_loadu_si128(reinterpret_cast<const __m128i *>(&elem[0]));
            return _mm_permutevar_ps(V, vControl);
        }

        inline XMVECTOR XM_CALLCONV XMVectorPermute(FXMVECTOR V1, FXMVECTOR V2, uint32_t PermuteX, uint32_t PermuteY, uint32_t PermuteZ, uint32_t PermuteW)
        {
            assert(PermuteX <= 7 && PermuteY <= 7 && PermuteZ <= 7 && PermuteW <= 7);
            _Analysis_assume_(PermuteX <= 7 && PermuteY <= 7 && PermuteZ <= 7 && PermuteW <= 7);

            static const XMVECTORU32 three = { { { 3, 3, 3, 3 } } };

            XM_ALIGNED_DATA(16) unsigned int elem[4] = { PermuteX, PermuteY, PermuteZ, PermuteW };
            __m128i vControl = _mm_load_si128(reinterpret_cast<const __m128i *>(&elem[0]));

            __m128i vSelect = _mm_cmpgt_epi32(vControl, three);
            vControl = _mm_castps_si128(_mm_and_ps(_mm_castsi128_ps(vControl), three));

            __m128 shuffled1 = _mm_permutevar_ps(V1, vControl);
            __m128 shuffled2 = _mm_permutevar_ps(V2, vControl);

            __m128 masked1 = _mm_andnot_ps(_mm_castsi128_ps(vSelect), shuffled1);
            __m128 masked2 = _mm_and_ps(_mm_castsi128_ps(vSelect), shuffled2);

            return _mm_or_ps(masked1, masked2);
        }

        inline XMVECTOR XM_CALLCONV XMVectorShiftLeft(FXMVECTOR V1, FXMVECTOR V2, uint32_t Elements)
        {
            assert(Elements < 4);
            _Analysis_assume_(Elements < 4);
            return AVX2::XMVectorPermute(V1, V2, Elements, ((Elements)+ 1), ((Elements)+ 2), ((Elements)+ 3));
        }

        inline XMVECTOR XM_CALLCONV XMVectorRotateLeft(FXMVECTOR V, uint32_t Elements)
        {
            assert(Elements < 4);
            _Analysis_assume_(Elements < 4);
            return AVX2::XMVectorSwizzle(V, Elements & 3, (Elements + 1) & 3, (Elements + 2) & 3, (Elements + 3) & 3);
        }

        inline XMVECTOR XM_CALLCONV XMVectorRotateRight(FXMVECTOR V, uint32_t Elements)
        {
            assert(Elements < 4);
            _Analysis_assume_(Elements < 4);
            return AVX2::XMVectorSwizzle(V, (4 - (Elements)) & 3, (5 - (Elements)) & 3, (6 - (Elements)) & 3, (7 - (Elements)) & 3);
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
            XMVECTOR vTemp = _mm_broadcastss_ps(V); // X
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
            XMVECTOR vTemp = _mm_broadcastss_ps(V); // X
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
            XMVECTOR vTemp = _mm_broadcastss_ps(V); // X
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
            vTemp = _mm_broadcastss_ps(V); // X
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
            vTemp = _mm_broadcastss_ps(V); // X
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
            vTemp = _mm_broadcastss_ps(V); // X
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

            XMMATRIX Transform = AVX2::XMMatrixMultiply(World, View);
            Transform = AVX2::XMMatrixMultiply(Transform, Projection);

            XMVECTOR Result = AVX2::XMVector3TransformCoord(V, Transform);

            Result = AVX2::XMVectorMultiplyAdd(Result, Scale, Offset);

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
            Offset = AVX2::XMVectorMultiplyAdd(Scale, Offset, D.v);

            XMMATRIX Transform = AVX2::XMMatrixMultiply(World, View);
            Transform = AVX2::XMMatrixMultiply(Transform, Projection);
            Transform = XMMatrixInverse(nullptr, Transform);

            XMVECTOR Result = AVX2::XMVectorMultiplyAdd(V, Scale, Offset);

            return AVX2::XMVector3TransformCoord(Result, Transform);
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
            vTemp = _mm_broadcastss_ps(V); // X
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
            XMVECTOR vX = _mm_broadcastss_ps(vW);
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
            vX = _mm_broadcastss_ps(vW);
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            mResult.r[1] = vX;
            vW = M1.r[2];
            vX = _mm_broadcastss_ps(vW);
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            mResult.r[2] = vX;
            vW = M1.r[3];
            vX = _mm_broadcastss_ps(vW);
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
            XMVECTOR vX = _mm_broadcastss_ps(vW);
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
            vX = _mm_broadcastss_ps(vW);
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            __m128 r1 = vX;
            vW = M1.r[2];
            vX = _mm_broadcastss_ps(vW);
            vY = _mm_permute_ps(vW, _MM_SHUFFLE(1, 1, 1, 1));
            vZ = _mm_permute_ps(vW, _MM_SHUFFLE(2, 2, 2, 2));
            vW = _mm_permute_ps(vW, _MM_SHUFFLE(3, 3, 3, 3));
            vX = _mm_mul_ps(vX, M2.r[0]);
            vX = _mm_fmadd_ps(vY, M2.r[1], vX);
            vX = _mm_fmadd_ps(vZ, M2.r[2], vX);
            vX = _mm_fmadd_ps(vW, M2.r[3], vX);
            __m128 r2 = vX;
            vW = M1.r[3];
            vX = _mm_broadcastss_ps(vW);
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


        //-------------------------------------------------------------------------------------
        // Permute Templates
        //-------------------------------------------------------------------------------------

        namespace MathInternal
        {
            // Slow path fallback for permutes that do not map to a single SSE opcode.
            template<uint32_t Shuffle, bool WhichX, bool WhichY, bool WhichZ, bool WhichW> struct PermuteHelper
            {
                static XMVECTOR XM_CALLCONV Permute(FXMVECTOR v1, FXMVECTOR v2)
                {
                    static const XMVECTORU32 selectMask =
                    { { {
                        WhichX ? 0xFFFFFFFF : 0,
                        WhichY ? 0xFFFFFFFF : 0,
                        WhichZ ? 0xFFFFFFFF : 0,
                        WhichW ? 0xFFFFFFFF : 0,
                    } } };

                    XMVECTOR shuffled1 = _mm_permute_ps(v1, Shuffle);
                    XMVECTOR shuffled2 = _mm_permute_ps(v2, Shuffle);

                    XMVECTOR masked1 = _mm_andnot_ps(selectMask, shuffled1);
                    XMVECTOR masked2 = _mm_and_ps(selectMask, shuffled2);

                    return _mm_or_ps(masked1, masked2);
                }
            };

            // Fast path for permutes that only read from the first vector.
            template<uint32_t Shuffle> struct PermuteHelper<Shuffle, false, false, false, false>
            {
                static XMVECTOR XM_CALLCONV Permute(FXMVECTOR v1, FXMVECTOR v2) { (v2); return _mm_permute_ps(v1, Shuffle); }
            };

            // Fast path for permutes that only read from the second vector.
            template<uint32_t Shuffle> struct PermuteHelper<Shuffle, true, true, true, true>
            {
                static XMVECTOR XM_CALLCONV Permute(FXMVECTOR v1, FXMVECTOR v2) { (v1); return _mm_permute_ps(v2, Shuffle); }
            };

            // Fast path for permutes that read XY from the first vector, ZW from the second.
            template<uint32_t Shuffle> struct PermuteHelper<Shuffle, false, false, true, true>
            {
                static XMVECTOR XM_CALLCONV Permute(FXMVECTOR v1, FXMVECTOR v2) { return _mm_shuffle_ps(v1, v2, Shuffle); }
            };

            // Fast path for permutes that read XY from the second vector, ZW from the first.
            template<uint32_t Shuffle> struct PermuteHelper<Shuffle, true, true, false, false>
            {
                static XMVECTOR XM_CALLCONV Permute(FXMVECTOR v1, FXMVECTOR v2) { return _mm_shuffle_ps(v2, v1, Shuffle); }
            };
        };

        // General permute template
        template<uint32_t PermuteX, uint32_t PermuteY, uint32_t PermuteZ, uint32_t PermuteW>
        inline XMVECTOR XM_CALLCONV XMVectorPermute(FXMVECTOR V1, FXMVECTOR V2)
        {
            static_assert(PermuteX <= 7, "PermuteX template parameter out of range");
            static_assert(PermuteY <= 7, "PermuteY template parameter out of range");
            static_assert(PermuteZ <= 7, "PermuteZ template parameter out of range");
            static_assert(PermuteW <= 7, "PermuteW template parameter out of range");

            const uint32_t Shuffle = _MM_SHUFFLE(PermuteW & 3, PermuteZ & 3, PermuteY & 3, PermuteX & 3);

            const bool WhichX = PermuteX > 3;
            const bool WhichY = PermuteY > 3;
            const bool WhichZ = PermuteZ > 3;
            const bool WhichW = PermuteW > 3;

            return AVX2::MathInternal::PermuteHelper<Shuffle, WhichX, WhichY, WhichZ, WhichW>::Permute(V1, V2);
        }

        // Special-case permute templates
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 1, 2, 3>(FXMVECTOR V1, FXMVECTOR) { return V1; }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 5, 6, 7>(FXMVECTOR, FXMVECTOR V2) { return V2; }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 1, 2, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x1); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 5, 2, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x2); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 5, 2, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x3); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 1, 6, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x4); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 1, 6, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x5); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 5, 6, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x6); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 5, 6, 3>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x7); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 1, 2, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x8); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 1, 2, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0x9); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 5, 2, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0xA); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 5, 2, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0xB); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 1, 6, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0xC); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<4, 1, 6, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0xD); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorPermute<0, 5, 6, 7>(FXMVECTOR V1, FXMVECTOR V2) { return _mm_blend_ps(V1, V2, 0xE); }


        //-------------------------------------------------------------------------------------
        // Swizzle Templates
        //-------------------------------------------------------------------------------------

        // General swizzle template
        template<uint32_t SwizzleX, uint32_t SwizzleY, uint32_t SwizzleZ, uint32_t SwizzleW>
        inline XMVECTOR XM_CALLCONV XMVectorSwizzle(FXMVECTOR V)
        {
            static_assert(SwizzleX <= 3, "SwizzleX template parameter out of range");
            static_assert(SwizzleY <= 3, "SwizzleY template parameter out of range");
            static_assert(SwizzleZ <= 3, "SwizzleZ template parameter out of range");
            static_assert(SwizzleW <= 3, "SwizzleW template parameter out of range");

            return _mm_permute_ps(V, _MM_SHUFFLE(SwizzleW, SwizzleZ, SwizzleY, SwizzleX));
        }

        // Specialized swizzles
        template<> inline XMVECTOR XM_CALLCONV XMVectorSwizzle<0, 1, 2, 3>(FXMVECTOR V) { return V; }
        template<> inline XMVECTOR XM_CALLCONV XMVectorSwizzle<0, 0, 0, 0>(FXMVECTOR V) { return _mm_broadcastss_ps(V); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorSwizzle<0, 0, 2, 2>(FXMVECTOR V) { return _mm_moveldup_ps(V); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorSwizzle<1, 1, 3, 3>(FXMVECTOR V) { return _mm_movehdup_ps(V); }


        //-------------------------------------------------------------------------------------
        // Other Templates
        //-------------------------------------------------------------------------------------

        template<uint32_t Elements>
        inline XMVECTOR XM_CALLCONV XMVectorShiftLeft(FXMVECTOR V1, FXMVECTOR V2)
        {
            static_assert(Elements < 4, "Elements template parameter out of range");
            return AVX2::XMVectorPermute<Elements, (Elements + 1), (Elements + 2), (Elements + 3)>(V1, V2);
        }

        template<uint32_t Elements>
        inline XMVECTOR XM_CALLCONV XMVectorRotateLeft(FXMVECTOR V)
        {
            static_assert(Elements < 4, "Elements template parameter out of range");
            return AVX2::XMVectorSwizzle<Elements & 3, (Elements + 1) & 3, (Elements + 2) & 3, (Elements + 3) & 3>(V);
        }

        template<uint32_t Elements>
        inline XMVECTOR XM_CALLCONV XMVectorRotateRight(FXMVECTOR V)
        {
            static_assert(Elements < 4, "Elements template parameter out of range");
            return AVX2::XMVectorSwizzle<(4 - Elements) & 3, (5 - Elements) & 3, (6 - Elements) & 3, (7 - Elements) & 3>(V);
        }

        //-------------------------------------------------------------------------------------
        // Data conversion
        //-------------------------------------------------------------------------------------

        inline float XMConvertHalfToFloat(PackedVector::HALF Value)
        {
            __m128i V1 = _mm_cvtsi32_si128(static_cast<int>(Value));
            __m128 V2 = _mm_cvtph_ps(V1);
            return _mm_cvtss_f32(V2);
        }

        inline PackedVector::HALF XMConvertFloatToHalf(float Value)
        {
            __m128 V1 = _mm_set_ss(Value);
            __m128i V2 = _mm_cvtps_ph(V1, 0);
            return static_cast<PackedVector::HALF>(_mm_cvtsi128_si32(V2));
        }

        inline float* XMConvertHalfToFloatStream
        (
            _Out_writes_bytes_(sizeof(float)+OutputStride*(HalfCount-1)) float* pOutputStream,
            _In_ size_t      OutputStride,
            _In_reads_bytes_(2+InputStride*(HalfCount-1)) const PackedVector::HALF* pInputStream,
            _In_ size_t      InputStride,
            _In_ size_t      HalfCount
        )
        {
            using namespace PackedVector;

            assert(pOutputStream);
            assert(pInputStream);

            assert(InputStride >= sizeof(HALF));
            assert(OutputStride >= sizeof(float));

            auto pHalf = reinterpret_cast<const uint8_t*>(pInputStream);
            auto pFloat = reinterpret_cast<uint8_t*>(pOutputStream);

            size_t i = 0;
            size_t four = HalfCount >> 2;
            if (four > 0)
            {
                if (InputStride == sizeof(HALF))
                {
                    if (OutputStride == sizeof(float))
                    {
                        if ((reinterpret_cast<uintptr_t>(pFloat) & 0xF) == 0)
                        {
                            // Packed input, aligned & packed output
                            for (size_t j = 0; j < four; ++j)
                            {
                                __m128i HV = _mm_loadl_epi64(reinterpret_cast<const __m128i*>(pHalf));
                                pHalf += InputStride * 4;

                                __m128 FV = _mm_cvtph_ps(HV);

                                _mm_stream_ps(reinterpret_cast<float*>(pFloat), FV);
                                pFloat += OutputStride * 4;
                                i += 4;
                            }
                        }
                        else
                        {
                            // Packed input, packed output
                            for (size_t j = 0; j < four; ++j)
                            {
                                __m128i HV = _mm_loadl_epi64(reinterpret_cast<const __m128i*>(pHalf));
                                pHalf += InputStride * 4;

                                __m128 FV = _mm_cvtph_ps(HV);

                                _mm_storeu_ps(reinterpret_cast<float*>(pFloat), FV);
                                pFloat += OutputStride * 4;
                                i += 4;
                            }
                        }
                    }
                    else
                    {
                        // Packed input, scattered output
                        for (size_t j = 0; j < four; ++j)
                        {
                            __m128i HV = _mm_loadl_epi64(reinterpret_cast<const __m128i*>(pHalf));
                            pHalf += InputStride * 4;

                            __m128 FV = _mm_cvtph_ps(HV);

                            _mm_store_ss(reinterpret_cast<float*>(pFloat), FV);
                            pFloat += OutputStride;
                            *reinterpret_cast<int*>(pFloat) = _mm_extract_ps(FV, 1);
                            pFloat += OutputStride;
                            *reinterpret_cast<int*>(pFloat) = _mm_extract_ps(FV, 2);
                            pFloat += OutputStride;
                            *reinterpret_cast<int*>(pFloat) = _mm_extract_ps(FV, 3);
                            pFloat += OutputStride;
                            i += 4;
                        }
                    }
                }
                else if (OutputStride == sizeof(float))
                {
                    if ((reinterpret_cast<uintptr_t>(pFloat) & 0xF) == 0)
                    {
                        // Scattered input, aligned & packed output
                        for (size_t j = 0; j < four; ++j)
                        {
                            uint16_t H1 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;
                            uint16_t H2 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;
                            uint16_t H3 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;
                            uint16_t H4 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;

                            __m128i HV = _mm_setzero_si128();
                            HV = _mm_insert_epi16(HV, H1, 0);
                            HV = _mm_insert_epi16(HV, H2, 1);
                            HV = _mm_insert_epi16(HV, H3, 2);
                            HV = _mm_insert_epi16(HV, H4, 3);
                            __m128 FV = _mm_cvtph_ps(HV);

                            _mm_stream_ps(reinterpret_cast<float*>(pFloat), FV);
                            pFloat += OutputStride * 4;
                            i += 4;
                        }
                    }
                    else
                    {
                        // Scattered input, packed output
                        for (size_t j = 0; j < four; ++j)
                        {
                            uint16_t H1 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;
                            uint16_t H2 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;
                            uint16_t H3 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;
                            uint16_t H4 = *reinterpret_cast<const HALF*>(pHalf);
                            pHalf += InputStride;

                            __m128i HV = _mm_setzero_si128();
                            HV = _mm_insert_epi16(HV, H1, 0);
                            HV = _mm_insert_epi16(HV, H2, 1);
                            HV = _mm_insert_epi16(HV, H3, 2);
                            HV = _mm_insert_epi16(HV, H4, 3);
                            __m128 FV = _mm_cvtph_ps(HV);

                            _mm_storeu_ps(reinterpret_cast<float*>(pFloat), FV);
                            pFloat += OutputStride * 4;
                            i += 4;
                        }

                    }
                }
                else
                {
                    // Scattered input, scattered output
                    for (size_t j = 0; j < four; ++j)
                    {
                        uint16_t H1 = *reinterpret_cast<const HALF*>(pHalf);
                        pHalf += InputStride;
                        uint16_t H2 = *reinterpret_cast<const HALF*>(pHalf);
                        pHalf += InputStride;
                        uint16_t H3 = *reinterpret_cast<const HALF*>(pHalf);
                        pHalf += InputStride;
                        uint16_t H4 = *reinterpret_cast<const HALF*>(pHalf);
                        pHalf += InputStride;

                        __m128i HV = _mm_setzero_si128();
                        HV = _mm_insert_epi16(HV, H1, 0);
                        HV = _mm_insert_epi16(HV, H2, 1);
                        HV = _mm_insert_epi16(HV, H3, 2);
                        HV = _mm_insert_epi16(HV, H4, 3);
                        __m128 FV = _mm_cvtph_ps(HV);

                        _mm_store_ss(reinterpret_cast<float*>(pFloat), FV);
                        pFloat += OutputStride;
                        *reinterpret_cast<int*>(pFloat) = _mm_extract_ps(FV, 1);
                        pFloat += OutputStride;
                        *reinterpret_cast<int*>(pFloat) = _mm_extract_ps(FV, 2);
                        pFloat += OutputStride;
                        *reinterpret_cast<int*>(pFloat) = _mm_extract_ps(FV, 3);
                        pFloat += OutputStride;
                        i += 4;
                    }
                }
            }

            for (; i < HalfCount; ++i)
            {
                *reinterpret_cast<float*>(pFloat) = XMConvertHalfToFloat(reinterpret_cast<const HALF*>(pHalf)[0]);
                pHalf += InputStride;
                pFloat += OutputStride;
            }

            return pOutputStream;
        }


        inline PackedVector::HALF* XMConvertFloatToHalfStream
        (
            _Out_writes_bytes_(2+OutputStride*(FloatCount-1)) PackedVector::HALF* pOutputStream,
            _In_ size_t       OutputStride,
            _In_reads_bytes_(sizeof(float)+InputStride*(FloatCount-1)) const float* pInputStream,
            _In_ size_t       InputStride,
            _In_ size_t       FloatCount
        )
        {
            using namespace PackedVector;

            assert(pOutputStream);
            assert(pInputStream);

            assert(InputStride >= sizeof(float));
            assert(OutputStride >= sizeof(HALF));

            auto pFloat = reinterpret_cast<const uint8_t*>(pInputStream);
            auto pHalf = reinterpret_cast<uint8_t*>(pOutputStream);

            size_t i = 0;
            size_t four = FloatCount >> 2;
            if (four > 0)
            {
                if (InputStride == sizeof(float))
                {
                    if (OutputStride == sizeof(HALF))
                    {
                        if ((reinterpret_cast<uintptr_t>(pFloat) & 0xF) == 0)
                        {
                            // Aligned and packed input, packed output
                            for (size_t j = 0; j < four; ++j)
                            {
                                __m128 FV = _mm_load_ps(reinterpret_cast<const float*>(pFloat));
                                pFloat += InputStride * 4;

                                __m128i HV = _mm_cvtps_ph(FV, 0);

                                _mm_storel_epi64(reinterpret_cast<__m128i*>(pHalf), HV);
                                pHalf += OutputStride * 4;
                                i += 4;
                            }
                        }
                        else
                        {
                            // Packed input, packed output
                            for (size_t j = 0; j < four; ++j)
                            {
                                __m128 FV = _mm_loadu_ps(reinterpret_cast<const float*>(pFloat));
                                pFloat += InputStride * 4;

                                __m128i HV = _mm_cvtps_ph(FV, 0);

                                _mm_storel_epi64(reinterpret_cast<__m128i*>(pHalf), HV);
                                pHalf += OutputStride * 4;
                                i += 4;
                            }
                        }
                    }
                    else
                    {
                        if ((reinterpret_cast<uintptr_t>(pFloat) & 0xF) == 0)
                        {
                            // Aligned & packed input, scattered output
                            for (size_t j = 0; j < four; ++j)
                            {
                                __m128 FV = _mm_load_ps(reinterpret_cast<const float*>(pFloat));
                                pFloat += InputStride * 4;

                                __m128i HV = _mm_cvtps_ph(FV, 0);

                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 0));
                                pHalf += OutputStride;
                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 1));
                                pHalf += OutputStride;
                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 2));
                                pHalf += OutputStride;
                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 3));
                                pHalf += OutputStride;
                                i += 4;
                            }
                        }
                        else
                        {
                            // Packed input, scattered output
                            for (size_t j = 0; j < four; ++j)
                            {
                                __m128 FV = _mm_loadu_ps(reinterpret_cast<const float*>(pFloat));
                                pFloat += InputStride * 4;

                                __m128i HV = _mm_cvtps_ph(FV, 0);

                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 0));
                                pHalf += OutputStride;
                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 1));
                                pHalf += OutputStride;
                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 2));
                                pHalf += OutputStride;
                                *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 3));
                                pHalf += OutputStride;
                                i += 4;
                            }
                        }
                    }
                }
                else if (OutputStride == sizeof(HALF))
                {
                    // Scattered input, packed output
                    for (size_t j = 0; j < four; ++j)
                    {
                        __m128 FV1 = _mm_load_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV2 = _mm_broadcast_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV3 = _mm_broadcast_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV4 = _mm_broadcast_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV = _mm_blend_ps(FV1, FV2, 0x2);
                        __m128 FT = _mm_blend_ps(FV3, FV4, 0x8);
                        FV = _mm_blend_ps(FV, FT, 0xC);

                        __m128i HV = _mm_cvtps_ph(FV, 0);

                        _mm_storel_epi64(reinterpret_cast<__m128i*>(pHalf), HV);
                        pHalf += OutputStride * 4;
                        i += 4;
                    }
                }
                else
                {
                    // Scattered input, scattered output
                    for (size_t j = 0; j < four; ++j)
                    {
                        __m128 FV1 = _mm_load_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV2 = _mm_broadcast_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV3 = _mm_broadcast_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV4 = _mm_broadcast_ss(reinterpret_cast<const float*>(pFloat));
                        pFloat += InputStride;

                        __m128 FV = _mm_blend_ps(FV1, FV2, 0x2);
                        __m128 FT = _mm_blend_ps(FV3, FV4, 0x8);
                        FV = _mm_blend_ps(FV, FT, 0xC);

                        __m128i HV = _mm_cvtps_ph(FV, 0);

                        *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 0));
                        pHalf += OutputStride;
                        *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 1));
                        pHalf += OutputStride;
                        *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 2));
                        pHalf += OutputStride;
                        *reinterpret_cast<HALF*>(pHalf) = static_cast<HALF>(_mm_extract_epi16(HV, 3));
                        pHalf += OutputStride;
                        i += 4;
                    }
                }
            }

            for (; i < FloatCount; ++i)
            {
                *reinterpret_cast<HALF*>(pHalf) = XMConvertFloatToHalf(reinterpret_cast<const float*>(pFloat)[0]);
                pFloat += InputStride;
                pHalf += OutputStride;
            }

            return pOutputStream;
        }


        //-------------------------------------------------------------------------------------
        // Half2
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMLoadHalf2(_In_ const PackedVector::XMHALF2* pSource)
        {
            assert(pSource);
            __m128 V = _mm_load_ss(reinterpret_cast<const float*>(pSource));
            return _mm_cvtph_ps(_mm_castps_si128(V));
        }

        inline void XM_CALLCONV XMStoreHalf2(_Out_ PackedVector::XMHALF2* pDestination, _In_ FXMVECTOR V)
        {
            assert(pDestination);
            __m128i V1 = _mm_cvtps_ph(V, 0);
            _mm_store_ss(reinterpret_cast<float*>(pDestination), _mm_castsi128_ps(V1));
        }


        //-------------------------------------------------------------------------------------
        // Half4
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMLoadHalf4(_In_ const PackedVector::XMHALF4* pSource)
        {
            assert(pSource);
            __m128i V = _mm_loadl_epi64(reinterpret_cast<const __m128i*>(pSource));
            return _mm_cvtph_ps(V);
        }

        inline void XM_CALLCONV XMStoreHalf4(_Out_ PackedVector::XMHALF4* pDestination, _In_ FXMVECTOR V)
        {
            assert(pDestination);
            __m128i V1 = _mm_cvtps_ph(V, 0);
            _mm_storel_epi64(reinterpret_cast<__m128i*>(pDestination), V1);
        }

    } // namespace AVX2

} // namespace DirectX;
