![DirectX Logo](https://raw.githubusercontent.com/wiki/Microsoft/DirectXMath/X_jpg.jpg)

# DirectXMath

https://github.com/Microsoft/DirectXMath

Copyright (c) Microsoft Corporation.

## April 2025

This package contains the DirectXMath library, an all inline SIMD C++ linear algebra library for use in games and graphics apps.

This code is designed to build with Visual Studio 2019 (16.11), Visual Studio 2022, or clang/LLVM for Windows. It is recommended that you make use of the latest updates.

These components are designed to work without requiring any content from the legacy DirectX SDK. For details, see [Where is the DirectX SDK?](https://aka.ms/dxsdk).

## Directory Layout

* ``Inc\``

  * DirectXMath Files (in the DirectX C++ namespace)

    * DirectXMath.h - Core library
    * DirectXPackedVector.h - Load/Store functions and types for working with various compressed GPU formats
    * DirectXColors.h - .NET-style Color defines in sRGB and linear color space
    * DirectXCollision.h - Bounding volume collision library

* ``Extentions\``

  * Advanced instruction set variants for guarded codepaths

    * DirectXMathSSE3.h - SSE3
    * DirectXMathBE.h - Supplemental SSE3 (SSSE3)
    * DirectXMathSSE4.h - SSE4.1
    * DirectXMathAVX.h - Advanced Vector Extensions (AVX)
    * DirectXMathAVX2.h - Advanced Vector Extensions 2 (AVX2)
    * DirectXMathF16C.h - Half-precision conversions (F16C)
    * DirectXMathFMA3.h - Fused multiply-accumulate (FMA3)
    * DirectXMathFMA4.h - Fused multiply-accumulate (FMA4)

* ``SHMath\``

  * Spherical Harmonics math functions

    * DirectXSH.h - Header for SHMath functions
    * DirectXSH.cpp, DirectXSHD3D11.cpp, DirectXSHD3D12.cpp - Implementation

* ``XDSP\``

  * XDSP.h - Digital Signal Processing helper functions

* ``build\``

  * Contains miscellaneous build files and scripts.

## Documentation

Documentation is available on the [Microsoft Docs](https://docs.microsoft.com/en-us/windows/desktop/dxmath/directxmath-portal). Additional information can be found on the [project wiki](https://github.com/microsoft/DirectXMath/wiki).

## Compiler support

Officially the library is supported with Microsoft Visual C++ 2019 (16.11) or later, clang/LLVM v12 or later, and GCC 10 or later. It should also compile with the Intel C++ and MinGW compilers.

When building with clang/LLVM or other GNU C compilers, the ``_XM_NO_XMVECTOR_OVERLOADS_`` control define is set because these compilers do not support creating operator overloads for the ``XMVECTOR`` type. You can choose to enable this preprocessor define explicitly to do the same thing with Visual C++ for improved portability.

To build for non-Windows platforms, you need to provide a ``sal.h`` header in your include path. You can obtain an open source version from [GitHub](https://raw.githubusercontent.com/dotnet/runtime/main/src/coreclr/pal/inc/rt/sal.h).

With GCC, the SAL annotation preprocessor symbols can conflict with the GNU implementation of the Standard C++ Library. The workaround is to include the system headers before including DirectXMath:

```cpp
#include <algorithm>
#include <iterator>
#include <utility>

#include <format> // C++20 header

#include <DirectXMath.h>
```

## Notices

All content and source code for this package are subject to the terms of the [MIT License](https://github.com/microsoft/DirectXMath/blob/main/LICENSE).

For the latest version of DirectXMath, bug reports, etc. please visit the project site on [GitHub](https://github.com/microsoft/DirectXMath).

## Release Notes

FOR SECURITY ADVISORIES, see [GitHub](https://github.com/microsoft/DirectXMath/security/advisories).

For a full change history, see [CHANGELOG.md](https://github.com/microsoft/DirectXMath/blob/main/CHANGELOG.md).

* The clang/LLVM toolset currently does not respect the ``float_control`` pragma for SSE instrinsics. Therefore, the use of ``/fp:fast`` is not recommended on clang/LLVM until this issue is fixed. See [55713](https://github.com/llvm/llvm-project/issues/55713).

## Support

For questions, consider using [Stack Overflow](https://stackoverflow.com/questions/tagged/directxmath) with the *directxmath* tag, or the [DirectX Discord Server](https://discord.gg/directx) in the *dx12-developers* or *dx9-dx11-developers* channel.

For bug reports and feature requests, please use GitHub [issues](https://github.com/microsoft/DirectXMath/issues) for this project.

## Contributing

This project welcomes contributions and suggestions. Most contributions require you to agree to a Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions provided by the bot. You will only need to do this once across all repos using our CLA.

Tests for new features should also be submitted as a PR to the [Test Suite](https://github.com/walbourn/directxmathtest/wiki) repository.

## Code of Conduct

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/). For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft trademarks or logos is subject to and must follow [Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general). Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship. Any use of third-party trademarks or logos are subject to those third-party's policies.

## Credits

The xboxmath library was originated by Matt Bronder with contributions from Sakphong Chanbai and David Hefner for the Xbox 360.

The xnamath library for the DirectX SDK and Xbox XDK was the work of Chuck Walbourn and Becky Heineman based on xboxmath, with contributions from Jeremy Gup, Dan Haffner, Matt Lee, Casey Meekhof, Rich Sauer, Jason Strayer, and Xiaoyue Zheng.

The DirectXMath library for the Windows SDK and Xbox One XDK is the work of Chuck Walbourn based on xnamath, with contributions from Darren Anderson, Matt Lee, Aaron Rodriguez Hernandez, Yuichi Ito, Reza Nourai, Rich Sauer, and Jason Strayer.

Thanks to Dave Eberly for his contributions particularly in improving the transcendental functions.

Thanks to Bruce Dawson for his help with the rounding functions.

Thanks to Andrew Farrier for the fixes to ``XMVerifyCPUSupport`` to properly support clang.

Thanks to Scott Matloff for his help in getting the library updated to use Intel SVML for VS 2019.
