ORBITER VISUALISATION PROJECT -
D3D9 Client Build Instructions

This Document describes the steps needed to build D3D9Client from the sources.
This document uses Visual Studio 2015 (Community Edition) as the main compiler,
but it should be very similar with Visual Studio 2017. As new versions will
appear (Visual Studio 2019) and time goes by there might be some difference in
the procedures.
In case your current Visual Studio versions needs some changes procedures,
please notify the developers at the orbiter-forums "D3D9Client Development"
thread (https://www.orbiter-forum.com/showthread.php?t=18431).


1. Required components
----------------------

To be able to compile D3D9 graphics client from the sources, you need:

* The fitting Orbiter libraries and headers.
  As a minimal build environment you only need the files from
  - Orbitersdk/include
  - Orbitersdk/lib
  - Orbitersdk/resources (Orbiter 2016)
    rsp.
    Orbitersdk/VS2015    (Orbiter BETA)

  The easiest way to get these directories is to run the get_orbiter_libs.bat
  script from Utils/D3D9Client.

  Another option is just to install the complete Orbiter installation, which
  will also have the benefits of being able to instantly debug the client.

* A Windows C++ compiler (Visual Studio 2015 Community Edition is ok, later
  versions should be fine, clang should also work, other compilers may or may
  not work).

* For a DirectX graphic client, you obviously also need the respective 3-D
  graphics SDKs. The D3D9 client builds with the MS DirectX June 2010
  SDK.
  You need the DirectX Software Development Kit (Version 9.29.1962 or newer) to
  build the client. It can be downloaded at Microsoft Download Center at
    https://www.microsoft.com/en-us/download/details.aspx?id=6812

  If you have an existing Microsoft Visual C++ 2010 Redistributable installed
  on your computer, you may receive an "S1023" error when you install the June
  2010 DirectX SDK.
  To resolve this issue follow the instructions from
    https://support.microsoft.com/en-us/help/2728613/s1023-error-when-you-install-the-directx-sdk-june-2010

  To run the client later on you also need the DirectX End-User Runtimes (June
  2010 or newer). These can be downloaded at Microsoft Download Center at
    https://www.microsoft.com/en-us/download/details.aspx?id=8109
  (See http://users.kymp.net/~p501474a/D3D9Client/#DirectX%20Runtimes for
  details on that).

* OPTIONAL (nVIDIA)
  NVIDIA API (needed since stereoscopic option was introduced in the Client)
  The NVAPI can be downloaded from the nVIDIA developer site at
    https://developer.nvidia.com/nvapi

  If you don't like to install the NVIDIA API (it's not needed if stereoscopic
  option should not be available in the client), you do not need to do
  anything.
  During the build process (pre compiling) a dummy header "nvapi.h" will be
  generated into the main source directory (Orbitersdk/D3D9Client)
  automatically if it does not yet exist.
  This also means that if you *have* installed the NVIDIA API and copied the
  header file into the main source directory (Orbitersdk/D3D9Client) it will
  *not* be touched or overwritten by any further build process.

* OPTIONAL (Documentation)
  To recompile the D3D9 source documentation you need Doxygen from
    http://www.stack.nl/~dimitri/doxygen/

  There is a doxygen configuration file in D3D9Client/doc/Doxyfile which
  can be used to generate the compressed html help file
  Orbitersdk/D3D9Client/doc/D3D9Client.chm included in the repository.
  Additionally to doxygen itself some other packets might be needed (depending
  on the output format you would like to generate) to create the 'chm' file
  you need:
  - Microsoft HTML Help Workshop (compiler for .CHM files)
      http://go.microsoft.com/fwlink/?LinkId=14188
  - Graphviz (for the 'dot' tool to generate graphs)
      http://www.graphviz.org/

* OPTIONAL (7-Zip)
  In order to create release-packages (ZIPs) a convenient build_release.bat
  script is used. If you like to be able to run that script without any errors
  you also need 7-Zip to be installed:
    https://www.7-zip.org/

  The build_release.bat script expects the executable to be at
  "C:\Program Files\7-Zip\7z.exe", so to minimize any issues with that you
  should keep that (default) location.


2. Building the D3D9 client
---------------------------

To compile the sources of the D3D9 graphics client, you have two options:

* Via IDE
  Just start the solution file that fits your compiler version.
  - Orbitersdk\D3D9Client\D3D9ClientVS2010.sln (Visual Studio 2010)
  - Orbitersdk\D3D9Client\D3D9ClientVS2012.sln (Visual Studio 2012)
  - Orbitersdk\D3D9Client\D3D9ClientVS2015.sln (Visual Studio 2015)
  - Orbitersdk\D3D9Client\D3D9ClientVS2017.sln (Visual Studio 2017)

  In case you have a more recent version of Visual Studio (like Visual Studio
  2019 for example), you should take the solution file with the highest number.
  Visual Studio will usually be able to migrate the project- and solution-files
  to the current version (The names however will not match your Visual Studio
  version, though).

* Via build_release.bat
  Just run build_release.bat from the folder Utils/D3D9Client and the client
  should be build without any further action required.
  The script likes to creates two ZIP release-packages at the end of a
  (successful) build, so to make that part work you need 7-Zip as well (see
  Required components).


3. Testing the D3D9 client
--------------------------

* If the compilation of the D3D9 client was successful, it has created a
  plugin in Modules/Plugin/D3D9Client.dll.

* Run the "no-graphics" version of orbiter (orbiter_ng) and activate
  the D3D9Client plugin from the Modules tab.

* You should now get a video tab in the Launchpad dialog. Configure your
  graphics driver and screen options, then launch a scenario.

* The Visual Studio project files should all be able to just run the Debug-
  Button. This however always starts with the "(Current state)" Scenario, so
  you might need to start at least once via orbiter_ng.exe to setup a fitting
  scenario to work with when debugging.
