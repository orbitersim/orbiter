//-------------------------------------------------------------------------------------
// DirectXMathF16C.h -- F16C/CVT16 extensions for SIMD C++ Math library
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
//
// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------

#pragma once

#if defined(_M_ARM) || defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64) || defined(_M_ARM64EC) || __arm__ || __aarch64__
#error F16C not supported on ARM platform
#endif

#include <DirectXMath.h>
#include <DirectXPackedVector.h>

namespace DirectX
{

    namespace F16C
    {

        inline bool XMVerifyF16CSupport()
        {
            // Should return true for AMD "Piledriver" and Intel "Ivy Bridge" processors
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

            // We check for F16C, AVX, OSXSAVE, and SSE4.1
            return ((CPUInfo[2] & 0x38080000) == 0x38080000);
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
            _Out_writes_bytes_(sizeof(float) + OutputStride * (HalfCount - 1)) float* pOutputStream,
            _In_ size_t      OutputStride,
            _In_reads_bytes_(2 + InputStride * (HalfCount - 1)) const PackedVector::HALF* pInputStream,
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
            _Out_writes_bytes_(2 + OutputStride * (FloatCount - 1)) PackedVector::HALF* pOutputStream,
            _In_ size_t       OutputStride,
            _In_reads_bytes_(sizeof(float) + InputStride * (FloatCount - 1)) const float* pInputStream,
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

    } // namespace F16C

} // namespace DirectX
