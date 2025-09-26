//-------------------------------------------------------------------------------------
// DirectXMathSSE4.h -- SSE4.1 extensions for SIMD C++ Math library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------

#pragma once

#if defined(_M_ARM) || defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64) || defined(_M_ARM64EC) || __arm__ || __aarch64__
#error SSE4 not supported on ARM platform
#endif

#include <smmintrin.h>

#include <DirectXMath.h>

namespace DirectX
{

    namespace SSE4
    {

        inline bool XMVerifySSE4Support()
        {
            // Should return true on AMD Bulldozer, Intel Core 2 ("Penryn"), and Intel Core i7 ("Nehalem") or later processors

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

            // We only check for SSE4.1 instruction set. SSE4.2 instructions are not used.
            return ((CPUInfo[2] & 0x80000) == 0x80000);
        }


        //-------------------------------------------------------------------------------------
        // Vector
        //-------------------------------------------------------------------------------------

    #ifdef __clang__
    #pragma clang diagnostic ignored "-Wundefined-reinterpret-cast"
    #endif

        inline void XM_CALLCONV XMVectorGetYPtr(_Out_ float *y, _In_ FXMVECTOR V)
        {
            assert(y != nullptr);
            *reinterpret_cast<int*>(y) = _mm_extract_ps(V, 1);
        }

        inline void XM_CALLCONV XMVectorGetZPtr(_Out_ float *z, _In_ FXMVECTOR V)
        {
            assert(z != nullptr);
            *reinterpret_cast<int*>(z) = _mm_extract_ps(V, 2);
        }

        inline void XM_CALLCONV XMVectorGetWPtr(_Out_ float *w, _In_ FXMVECTOR V)
        {
            assert(w != nullptr);
            *reinterpret_cast<int*>(w) = _mm_extract_ps(V, 3);
        }

        inline uint32_t XM_CALLCONV XMVectorGetIntY(FXMVECTOR V)
        {
            __m128i V1 = _mm_castps_si128(V);
            return static_cast<uint32_t>(_mm_extract_epi32(V1, 1));
        }

        inline uint32_t XM_CALLCONV XMVectorGetIntZ(FXMVECTOR V)
        {
            __m128i V1 = _mm_castps_si128(V);
            return static_cast<uint32_t>(_mm_extract_epi32(V1, 2));
        }

        inline uint32_t XM_CALLCONV XMVectorGetIntW(FXMVECTOR V)
        {
            __m128i V1 = _mm_castps_si128(V);
            return static_cast<uint32_t>(_mm_extract_epi32(V1, 3));
        }

        inline void XM_CALLCONV XMVectorGetIntYPtr(_Out_ uint32_t *y, _In_ FXMVECTOR V)
        {
            assert(y != nullptr);
            __m128i V1 = _mm_castps_si128(V);
            *y = static_cast<uint32_t>(_mm_extract_epi32(V1, 1));
        }

        inline void XM_CALLCONV XMVectorGetIntZPtr(_Out_ uint32_t *z, _In_ FXMVECTOR V)
        {
            assert(z != nullptr);
            __m128i V1 = _mm_castps_si128(V);
            *z = static_cast<uint32_t>(_mm_extract_epi32(V1, 2));
        }

        inline void XM_CALLCONV XMVectorGetIntWPtr(_Out_ uint32_t *w, _In_ FXMVECTOR V)
        {
            assert(w != nullptr);
            __m128i V1 = _mm_castps_si128(V);
            *w = static_cast<uint32_t>(_mm_extract_epi32(V1, 3));
        }

        inline XMVECTOR XM_CALLCONV XMVectorSetY(FXMVECTOR V, float y)
        {
            XMVECTOR vResult = _mm_set_ss(y);
            vResult = _mm_insert_ps(V, vResult, 0x10);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVectorSetZ(FXMVECTOR V, float z)
        {
            XMVECTOR vResult = _mm_set_ss(z);
            vResult = _mm_insert_ps(V, vResult, 0x20);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVectorSetW(FXMVECTOR V, float w)
        {
            XMVECTOR vResult = _mm_set_ss(w);
            vResult = _mm_insert_ps(V, vResult, 0x30);
            return vResult;
        }

        inline XMVECTOR XM_CALLCONV XMVectorSetIntY(FXMVECTOR V, uint32_t y)
        {
            __m128i vResult = _mm_castps_si128(V);
            vResult = _mm_insert_epi32(vResult, static_cast<int>(y), 1);
            return _mm_castsi128_ps(vResult);
        }

        inline XMVECTOR XM_CALLCONV XMVectorSetIntZ(FXMVECTOR V, uint32_t z)
        {
            __m128i vResult = _mm_castps_si128(V);
            vResult = _mm_insert_epi32(vResult, static_cast<int>(z), 2);
            return _mm_castsi128_ps(vResult);
        }

        inline XMVECTOR XM_CALLCONV XMVectorSetIntW(FXMVECTOR V, uint32_t w)
        {
            __m128i vResult = _mm_castps_si128(V);
            vResult = _mm_insert_epi32(vResult, static_cast<int>(w), 3);
            return _mm_castsi128_ps(vResult);
        }

        inline XMVECTOR XM_CALLCONV XMVectorRound(FXMVECTOR V)
        {
            return _mm_round_ps(V, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
        }

        inline XMVECTOR XM_CALLCONV XMVectorTruncate(FXMVECTOR V)
        {
            return _mm_round_ps(V, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
        }

        inline XMVECTOR XM_CALLCONV XMVectorFloor(FXMVECTOR V)
        {
            return _mm_floor_ps(V);
        }

        inline XMVECTOR XM_CALLCONV XMVectorCeiling(FXMVECTOR V)
        {
            return _mm_ceil_ps(V);
        }


        //-------------------------------------------------------------------------------------
        // Vector2
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVector2Dot(FXMVECTOR V1, FXMVECTOR V2)
        {
            return _mm_dp_ps(V1, V2, 0x3f);
        }

        inline XMVECTOR XM_CALLCONV XMVector2LengthSq(FXMVECTOR V)
        {
            return SSE4::XMVector2Dot(V, V);
        }

        inline XMVECTOR XM_CALLCONV XMVector2ReciprocalLengthEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x3f);
            return _mm_rsqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector2ReciprocalLength(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x3f);
            XMVECTOR vLengthSq = _mm_sqrt_ps(vTemp);
            return _mm_div_ps(g_XMOne, vLengthSq);
        }

        inline XMVECTOR XM_CALLCONV XMVector2LengthEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x3f);
            return _mm_sqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector2Length(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x3f);
            return _mm_sqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector2NormalizeEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x3f);
            XMVECTOR vResult = _mm_rsqrt_ps(vTemp);
            return _mm_mul_ps(vResult, V);
        }

        inline XMVECTOR XM_CALLCONV XMVector2Normalize(FXMVECTOR V)
        {
            XMVECTOR vLengthSq = _mm_dp_ps(V, V, 0x3f);
            // Prepare for the division
            XMVECTOR vResult = _mm_sqrt_ps(vLengthSq);
            // Create zero with a single instruction
            XMVECTOR vZeroMask = _mm_setzero_ps();
            // Test for a divide by zero (Must be FP to detect -0.0)
            vZeroMask = _mm_cmpneq_ps(vZeroMask, vResult);
            // Failsafe on zero (Or epsilon) length planes
            // If the length is infinity, set the elements to zero
            vLengthSq = _mm_cmpneq_ps(vLengthSq, g_XMInfinity);
            // Reciprocal mul to perform the normalization
            vResult = _mm_div_ps(V, vResult);
            // Any that are infinity, set to zero
            vResult = _mm_and_ps(vResult, vZeroMask);
            // Select qnan or result based on infinite length
            XMVECTOR vTemp1 = _mm_andnot_ps(vLengthSq, g_XMQNaN);
            XMVECTOR vTemp2 = _mm_and_ps(vResult, vLengthSq);
            vResult = _mm_or_ps(vTemp1, vTemp2);
            return vResult;
        }


        //-------------------------------------------------------------------------------------
        // Vector3
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVector3Dot(FXMVECTOR V1, FXMVECTOR V2)
        {
            return _mm_dp_ps(V1, V2, 0x7f);
        }

        inline XMVECTOR XM_CALLCONV XMVector3LengthSq(FXMVECTOR V)
        {
            return SSE4::XMVector3Dot(V, V);
        }

        inline XMVECTOR XM_CALLCONV XMVector3ReciprocalLengthEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x7f);
            return _mm_rsqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector3ReciprocalLength(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x7f);
            XMVECTOR vLengthSq = _mm_sqrt_ps(vTemp);
            return _mm_div_ps(g_XMOne, vLengthSq);
        }

        inline XMVECTOR XM_CALLCONV XMVector3LengthEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x7f);
            return _mm_sqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector3Length(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x7f);
            return _mm_sqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector3NormalizeEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0x7f);
            XMVECTOR vResult = _mm_rsqrt_ps(vTemp);
            return _mm_mul_ps(vResult, V);
        }

        inline XMVECTOR XM_CALLCONV XMVector3Normalize(FXMVECTOR V)
        {
            XMVECTOR vLengthSq = _mm_dp_ps(V, V, 0x7f);
            // Prepare for the division
            XMVECTOR vResult = _mm_sqrt_ps(vLengthSq);
            // Create zero with a single instruction
            XMVECTOR vZeroMask = _mm_setzero_ps();
            // Test for a divide by zero (Must be FP to detect -0.0)
            vZeroMask = _mm_cmpneq_ps(vZeroMask, vResult);
            // Failsafe on zero (Or epsilon) length planes
            // If the length is infinity, set the elements to zero
            vLengthSq = _mm_cmpneq_ps(vLengthSq, g_XMInfinity);
            // Divide to perform the normalization
            vResult = _mm_div_ps(V, vResult);
            // Any that are infinity, set to zero
            vResult = _mm_and_ps(vResult, vZeroMask);
            // Select qnan or result based on infinite length
            XMVECTOR vTemp1 = _mm_andnot_ps(vLengthSq, g_XMQNaN);
            XMVECTOR vTemp2 = _mm_and_ps(vResult, vLengthSq);
            vResult = _mm_or_ps(vTemp1, vTemp2);
            return vResult;
        }


        //-------------------------------------------------------------------------------------
        // Vector4
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMVector4Dot(FXMVECTOR V1, FXMVECTOR V2)
        {
            return _mm_dp_ps(V1, V2, 0xff);
        }

        inline XMVECTOR XM_CALLCONV XMVector4LengthSq(FXMVECTOR V)
        {
            return SSE4::XMVector4Dot(V, V);
        }

        inline XMVECTOR XM_CALLCONV XMVector4ReciprocalLengthEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0xff);
            return _mm_rsqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector4ReciprocalLength(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0xff);
            XMVECTOR vLengthSq = _mm_sqrt_ps(vTemp);
            return _mm_div_ps(g_XMOne, vLengthSq);
        }

        inline XMVECTOR XM_CALLCONV XMVector4LengthEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0xff);
            return _mm_sqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector4Length(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0xff);
            return _mm_sqrt_ps(vTemp);
        }

        inline XMVECTOR XM_CALLCONV XMVector4NormalizeEst(FXMVECTOR V)
        {
            XMVECTOR vTemp = _mm_dp_ps(V, V, 0xff);
            XMVECTOR vResult = _mm_rsqrt_ps(vTemp);
            return _mm_mul_ps(vResult, V);
        }

        inline XMVECTOR XM_CALLCONV XMVector4Normalize(FXMVECTOR V)
        {
            XMVECTOR vLengthSq = _mm_dp_ps(V, V, 0xff);
            // Prepare for the division
            XMVECTOR vResult = _mm_sqrt_ps(vLengthSq);
            // Create zero with a single instruction
            XMVECTOR vZeroMask = _mm_setzero_ps();
            // Test for a divide by zero (Must be FP to detect -0.0)
            vZeroMask = _mm_cmpneq_ps(vZeroMask, vResult);
            // Failsafe on zero (Or epsilon) length planes
            // If the length is infinity, set the elements to zero
            vLengthSq = _mm_cmpneq_ps(vLengthSq, g_XMInfinity);
            // Divide to perform the normalization
            vResult = _mm_div_ps(V, vResult);
            // Any that are infinity, set to zero
            vResult = _mm_and_ps(vResult, vZeroMask);
            // Select qnan or result based on infinite length
            XMVECTOR vTemp1 = _mm_andnot_ps(vLengthSq, g_XMQNaN);
            XMVECTOR vTemp2 = _mm_and_ps(vResult, vLengthSq);
            vResult = _mm_or_ps(vTemp1, vTemp2);
            return vResult;
        }


        //-------------------------------------------------------------------------------------
        // Plane
        //-------------------------------------------------------------------------------------

        inline XMVECTOR XM_CALLCONV XMPlaneNormalizeEst(FXMVECTOR P)
        {
            XMVECTOR vTemp = _mm_dp_ps(P, P, 0x7f);
            XMVECTOR vResult = _mm_rsqrt_ps(vTemp);
            return _mm_mul_ps(vResult, P);
        }

        inline XMVECTOR XM_CALLCONV XMPlaneNormalize(FXMVECTOR P)
        {
            XMVECTOR vLengthSq = _mm_dp_ps(P, P, 0x7f);
            // Prepare for the division
            XMVECTOR vResult = _mm_sqrt_ps(vLengthSq);
            // Failsafe on zero (Or epsilon) length planes
            // If the length is infinity, set the elements to zero
            vLengthSq = _mm_cmpneq_ps(vLengthSq, g_XMInfinity);
            // Reciprocal mul to perform the normalization
            vResult = _mm_div_ps(P, vResult);
            // Any that are infinity, set to zero
            vResult = _mm_and_ps(vResult, vLengthSq);
            return vResult;
        }

    } // namespace SSE4

} // namespace DirectX
