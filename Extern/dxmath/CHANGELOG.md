# DirectXMath

https://github.com/Microsoft/DirectXMath

Release available for download on [GitHub](https://github.com/microsoft/DirectXMath/releases)

## Release History

### April 2025 (3.20b)
* `XM_DEPRECATED` macro uses C++14 ``[[deprecated]]`` standard attribute when supported
* Cmake project updates with build options for XDSP and SHMath

### October 2024 (3.20)
* Fixed close-to-zero bug in the implementation of `TriangleTests::Intersects`
* Renamed implementation namespace from `DirectX::Internal` to `DirectX::MathInternal` to avoid some conformance issues with other libraries
* CMake project updates including support for ARM64EC
* Added GitHub Actions YAML files

### February 2024 (3.19)
* Fix to address MinGW issue with ``__cpuid`` in cpuid.h vs. intrin.h
* Additional updates for clang/LLVM and GNUC
* Minor comment updates

### December 2023 (3.18b)
* Hot-fix to address ``-Wunsafe-buffer-usage`` warnings from clang v16
* Hot-fix to address MinGW issue with ``__cpuid`` in cpuid.h vs. intrin.h
* CMake project updates including pkg-config file generation

### December 2022 (3.18)
* C++20 spaceship operators for XMFLOAT2, XMFLOAT3, etc. when building with ``/std:c++20 /Zc:_cplusplus``
* Improved conformance for ARM64 when using `/Zc:arm64-aliased-neon-types-`
* Minor code review
* CMake project updated to require 3.20 or later
* Added Azure Dev Ops Pipeline YAML files

### May 2022 (3.17b)
* Hot-fix to address ``-Wreserved-identifier`` warnings with clang v13
* C++20 spaceship operators for XMFLOAT2, XMFLOAT3, etc. when building with ``/std:c++20 /Zc:_cplusplus``
* Minor CMake project update

### January 2022 (3.17)
* Added ColorsLinear namespace to DirectXColors.h with linear versions of .NET colors
* Optimized the ``XMMatrixRotationRollPitchYaw(FromVector)`` functions
* Fixed overread problem for 16bpp GPU types Load functions:
  * ``XMUNIBBLE4``, ``XMU555``, ``XMU565``, ``XMBYTEN2``, ``XMBYTE2``, ``XMUBYTEN2``, ``XMUBYTE2``
* ``XM_CACHE_LINE_SIZE`` updated for ARM/ARM64 targets to 128 bytes
* A few comments added to improve IntelliSense experience
* Conformance improvements for GNU compiler
* Minor code cleanup

### January 2021 (3.16b)
* Hot-fixes to resolve build breaks for clang/LLVM and GCC on ARM64
* ``XM_ALIGNED_DATA`` and ``XM_ALIGNED_STRUCT`` macros updated to use C++17 ``alignas`` when available

### December 2020 (3.16)
* Added ``XMVectorLog10`` / ``XMVectorExp10``
* Added ``XMColorRGBToYUV_UHD`` / ``XMColorYUVToRGB_UHD`` for Rec. 2020 YUV
* Added optional ``rhcoords`` parameter for BoundingFrustum ``CreateFromMatrix``
* Added use of Intel&reg; Short Vector Matrix Library (SVML) supported by VS 2019
  * Opt-in with ``_XM_SVML_INTRINSICS_``; opt-out with ``_XM_DISABLE_INTEL_SVML_``
* Fixed denorm handling for ``XMConvertFloatToHalf``
* Fixed flush (too small for denorm) handling for ``XMStoreFloat3PK``
* Fixed clamping bug in ``XMStoreByteN4``
* Cleaned up ARM-NEON intrinsics type issues for improved portability on GNUC
* Fixed ``GXMVECTOR`` for x86 ``__vectorcall``
* Code review

### April 2020 (3.15)
* Added ``XMMatrixVectorTensorProduct`` for creating a matrix from two vectors
* Use of m256 registers and FMA3 with ``/arch:AVX2`` for stream and some matrix functions
* Optimized load/stores for SSE2 float2 & float3 functions
* Optimized some instruction choices for better AMD CPU support
* Improved conformance for clang/LLVM, GCC, and MinGW compilers
* Code review (``constexpr`` / ``noexcept`` usage)
* Retired VS 2015 support

### August 2019 (3.14)
* Added float control around IsNan functions to resolve issue with VS 2019 with ``/fp:fast``
* XMVerifyCPUSupport updated for clang/LLVM cpuid implementation on x86/x64
* Added support for clang/LLVM built-in platform defines as well as the MSVC ones
* Cleaned up ARM-NEON intrinsics type issues for improved portability
* Removed unneeded malloc.h include in DirectXMath.h
* Whitespace cleanup

### July 2018 (3.13)
* ``XMFLOAT3X4``, ``XMFLOAT3X4A``, and associated Load/Store functions
* Move/copy constructors and assignment operators for C++ types
* Minor fix for XMVectorClamp behavior with NaN
* Fixed compilation warnings with VS 2017 (15.7 update), Intel C++ 18.0 compiler, and clang 6
* Retired VS 2013 support
* Minor code cleanup

### February 2018 (3.12)
* ARM64 use of fused multiply-accumulate intriniscs
* Conformance fix for XMConvertFloatToHalf
* Minor code cleanup

### June 2017 (3.11)
* AVX optimization of XMMatrixMultiply and XMMatrixMultiplyTranspose
* AVX2 optimization for XMVectorSplatX
* FMA3 optimization of XMVectorMultiplyAdd and XMVectorNegativeMultiplySubtract (implied by /arch:AVX2)
* Conformance fixes to support compilation with Clang 3.7

### January 2017 (3.10)
* Added XMVectorSum for horizontal adds
* ARMv8 intrinsics use for ARM64 platform (division, rounding, half-precision conversion)
* Added SSE3 codepaths using opt-in ``_XM_SSE3_INTRINSICS_``
* XMVectorRound fix for no-intrinsics to match round to nearest (even)
* XMStoreFloat3SE fix when max channel isn't a perfect power of 2
* constexpr conformance fix and workaround for compiler bug in VS 2015 RTM
* Remove support for VS 2012 compilers
* Remove ``__vector4i`` deprecated type

### June 2016 (3.09)
* Includes support for additional optimizations when built with /arch:AVX or /arch:AVX2
* Added use of constexpr for type constructors, XMConvertToRadians, and XMConvertToDegrees
* Marked ``__vector4i``, ``XMXDEC4``, ``XMDECN4``, ``XMDEC4``, and associated Load & Store functions as deprecated.
  * These are vestiges of Xbox 360 support and will be removed in a future release
* Renamed parameter in XMMatrixPerspectiveFov* to reduce user confusion when relying on IntelliSense
* XMU565, XMUNIBBLE4 constructors take uint8_t instead of int8_t

### May 2016
* DirectXMath 3.08 released under the MIT license

### November 2015 (3.08)
* Added use of ``_mm_sfence`` for Stream methods
* Fixed bug with non-uniform scaling transforms for BoundingOrientedBox
* Added asserts for Near/FarZ in XMMatrix* methods
* Added use of ``=default`` for PODs with VS 2013/2015
* Additional SSE and ARM-NEON optimizations for PackedVector functions

### April 2015 (3.07)
* Fix customer reported bugs in BoundingBox methods
* Fix customer reported bug in XMStoreFloat3SE
* Fix customer reported bug in XMVectorATan2, XMVectorATan2Est
* Fix customer reported bug in XMVectorRound

### October 2013 (3.06)
* Fixed load/store of XMFLOAT3SE to properly match the ``DXGI_FORMAT_R9G9B9E5_SHAREDEXP``
* Added ``XMLoadUDecN4_XR`` and ``XMStoreUDecN4_XR`` to match ``DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM``
* Added ``XMColorRGBToSRGB`` and ``XMColorSRGBToRGB`` to convert linear RGB <-> sRGB

### July 2013 (3.05)
* Use x86/x64 ``__vectorcall`` calling-convention when available (``XM_CALLCONV``, ``HXMVECTOR``, ``FXMMATRIX`` introduced)
* Fixed bug with XMVectorFloor and XMVectorCeiling when given whole odd numbers (i.e. 105.0)
* Improved XMVectorRound algorithm
* ARM-NEON optimizations for XMVectorExp2, XMVectorLog2, XMVectorExpE, and XMVectorLogE
* ARM-NEON code paths use multiply-by-scalar intrinsics when supported
* Additional optimizations for ARM-NEON Stream functions
* Fixed potential warning C4723 using ``operator/`` or ``operator/=``

### March 2013 (3.04)
* ``XMVectorExp2``, ``XMVectorLog2``, ``XMVectorExpE``, and ``XMVectorLogE`` functions added to provide base-e support in addition to the existing base-2 support
* ``XMVectorExp`` and ``XMVectorLog`` are now aliases for XMVectorExp2 and XMVectorLog2
* Additional optimizations for Stream functions
* XMVector3Cross now ensures w component is zero on ARM
* XMConvertHalfToFloat and XMConvertFloatToHalf  now use IEEE 754 standard float16 behavior for INF/QNAN
* Updated matrix version Transform for BoundingOrientedBox and BoundingFrustum to handle scaling

### March 2012 (3.03)
* *breaking change* Removed union members from XMMATRIX type to make it a fully 'opaque' type
* Marked single-parameter C++ constructors for XMFLOAT2, XMFLOAT2A, XMFLOAT3, XMFLOAT3A, XMFLOAT4, and XMFLOAT4A explicit

### February 2012 (3.02)
* ARM-NEON intrinsics (selected by default for the ARM platform)
* Reworked XMVectorPermute, change of ``XM_PERMUTE_`` defines, removal of XMVectorPermuteControl
* Addition of ``XM_SWIZZLE_`` defines
* Optimizations for transcendental functions
* Template forms for permute, swizzle, shift-left, rotate-left, rotation-right, and insert
* Removal of deprecated types and functions
  * ``XM_CACHE_LINE_SIZE`` define, XMVectorExpEst, XMVectorLogEst, XMVectorPowEst, XMVectorSinHEs, XMVectorCosHEst, XMVectorTanHEst, XMVector2InBoundsR, XMVector3InBoundsR, XMVector4InBoundsR
* Removed ``XM_STRICT_VECTOR4``; XMVECTOR in NO-INTRINSICS always defined without .x, .y, .z, .w, .v, or .u
* Additional bounding types
* SAL fixes and improvements

### September 2011 (3.00)
* Renamed and reorganized the headers
* Introduced C++ namespaces
* Removed the Xbox 360-specific GPU types
  * HENDN3, XMHEND3, XMUHENDN3, XMUHEND3, XMDHENN3, XMDHEN3, XMUDHENN3, XMUDHEN3, XMXICON4, XMXICO4, XMICON4, XMICO4, XMUICON4, XMUICO4

### July 2012 (XNAMath 2.05)
* Template forms have been added for `XMVectorPermute`, `XMVectorSwizzle`, `XMVectorShiftLeft`, `XMVectorRotateLeft`, `XMVectorRotateRight`, and `XMVectorInsert`
* The `XM_STRICT_XMMATRIX` compilation define has been added for opaque `XMMATRIX`.
* Stream stride and count arguments have been changed to `size_t`
* The ``pDeterminant`` parameter of `XMMatrixInverse` is now optional
* Additional operator= overloads for `XMBYTEN4`, `XMBYTE4`, `XMUBYTEN4`, and `XMUBYTE4` types are now available

### February 2011 (XNAMath 2.04)
* Addition of new data types and associated load-store functions:
  * `XMBYTEN2, XMBYTE2, XMUBYTEN2, XMUBYTE2`
  * `XMLoadByteN2, XMLoadByte2, XMLoadUByteN2, XMLoadUByte2`
  * `XMStoreByteN2, XMStoreByte2, XMStoreUByteN2, XMStoreUByte2`
  * `XMINT2, XMUINT2, XMINT3, XMUINT3, XMINT4, XMUINT4`
  * `XMLoadSInt2, XMLoadUInt2, XMLoadSInt3, XMLoadUInt3, XMLoadSInt4, XMLoadUInt4`
  * `XMStoreSInt2, XMStoreUInt2, XMStoreSInt3, XMStoreUInt3, XMStoreSInt4, XMStoreUInt4`
* Marked most single-parameter C++ constructors with `explicit` keyword
* Corrected range issues with SSE implementations of `XMVectorFloor` and `XMVectorCeiling`

### June 2010 (XNAMath 2.03)
* Addition of ``XMVectorDivide`` to optimize SSE2 vector division operations
* Unified handling of floating-point specials between the Windows SSE2 and no-intrinsics implementations
* Use of Visual Studio style SAL annotations
* Modifications to the C++ declarations for `XMFLOAT2A/3A/4A/4X3A/4X4A` to better support these types in C++ templates

### February 2010 (XNAMath 2.02)
* Fixes to `XMStoreColor`, `XMQuaternionRotationMatrix`, `XMVectorATan2`, and `XMVectorATan2Est`

### August 2009 (XNAMath 2.01)
* Adds ``XM_STRICT_VECTOR4``. This opt-in directive disallows the usage of XboxMath-like  member accessors such as .x, .y, and .z. This makes it easier to write portable XNA Math code.
* Added conversion support for the following Windows graphics formats:
  * 16-bit color formats (565, 555X, 5551)
  * 4-bits per channel color formats (4444)
  * Unique Direct3D 10/11 formats (``DXGI_FORMAT_R9G9B9E5_SHAREDEXP`` and ``DXGI_FORMAT_R11G11B10_FLOAT``)

### March 2009 (XNAMath 2.00)
* Initial release (based on the Xbox 360 Xbox math library)
