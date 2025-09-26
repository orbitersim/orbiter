//-------------------------------------------------------------------------------------
// DirectXMathAVX.h -- AVX (version 1) extensions for SIMD C++ Math library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------

#pragma once

#if defined(_M_ARM) || defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64) || defined(_M_ARM64EC) || __arm__ || __aarch64__
#error AVX not supported on ARM platform
#endif

#include <DirectXMath.h>

namespace DirectX
{

    namespace AVX
    {

        inline bool XMVerifyAVXSupport()
        {
            // Should return true for AMD Bulldozer, Intel "Sandy Bridge", and Intel "Ivy Bridge" or later processors
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

            // We check for AVX, OSXSAVE, SSSE4.1, and SSE3
            return ((CPUInfo[2] & 0x18080001) == 0x18080001);
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
            return _mm_permute_ps(V, _MM_SHUFFLE(0, 0, 0, 0));
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
            return AVX::XMVectorPermute(V1, V2, Elements, ((Elements)+ 1), ((Elements)+ 2), ((Elements)+ 3));
        }

        inline XMVECTOR XM_CALLCONV XMVectorRotateLeft(FXMVECTOR V, uint32_t Elements)
        {
            assert(Elements < 4);
            _Analysis_assume_(Elements < 4);
            return AVX::XMVectorSwizzle(V, Elements & 3, (Elements + 1) & 3, (Elements + 2) & 3, (Elements + 3) & 3);
        }

        inline XMVECTOR XM_CALLCONV XMVectorRotateRight(FXMVECTOR V, uint32_t Elements)
        {
            assert(Elements < 4);
            _Analysis_assume_(Elements < 4);
            return AVX::XMVectorSwizzle(V, (4 - (Elements)) & 3, (5 - (Elements)) & 3, (6 - (Elements)) & 3, (7 - (Elements)) & 3);
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

            return AVX::MathInternal::PermuteHelper<Shuffle, WhichX, WhichY, WhichZ, WhichW>::Permute(V1, V2);
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
        template<> inline XMVECTOR XM_CALLCONV XMVectorSwizzle<0, 0, 2, 2>(FXMVECTOR V) { return _mm_moveldup_ps(V); }
        template<> inline XMVECTOR XM_CALLCONV XMVectorSwizzle<1, 1, 3, 3>(FXMVECTOR V) { return _mm_movehdup_ps(V); }


        //-------------------------------------------------------------------------------------
        // Other Templates
        //-------------------------------------------------------------------------------------

        template<uint32_t Elements>
        inline XMVECTOR XM_CALLCONV XMVectorShiftLeft(FXMVECTOR V1, FXMVECTOR V2)
        {
            static_assert(Elements < 4, "Elements template parameter out of range");
            return AVX::XMVectorPermute<Elements, (Elements + 1), (Elements + 2), (Elements + 3)>(V1, V2);
        }

        template<uint32_t Elements>
        inline XMVECTOR XM_CALLCONV XMVectorRotateLeft(FXMVECTOR V)
        {
            static_assert(Elements < 4, "Elements template parameter out of range");
            return AVX::XMVectorSwizzle<Elements & 3, (Elements + 1) & 3, (Elements + 2) & 3, (Elements + 3) & 3>(V);
        }

        template<uint32_t Elements>
        inline XMVECTOR XM_CALLCONV XMVectorRotateRight(FXMVECTOR V)
        {
            static_assert(Elements < 4, "Elements template parameter out of range");
            return AVX::XMVectorSwizzle<(4 - Elements) & 3, (5 - Elements) & 3, (6 - Elements) & 3, (7 - Elements) & 3>(V);
        }

    } // namespace AVX

} // namespace DirectX;
