ORBITER VISUALISATION PROJECT -
D3D9 Client Installation Instructions

1. Required components
----------------------

To compile the sources of the D3D9 graphics client, you need:

* The latest Orbiter release (base + SDK packages), available from the
  download page at the Orbiter site http://orbit.medphys.ucl.ac.uk/

* The latest Orbiter beta diffs, available under
  download.orbit.m6.net/betaNG/orbiter_beta.html

* A Windows C++ compiler (Visual Studio 2008 Express is ok, later should be
  fine, other compilers may or may not work).

* For the various graphics clients, you also need the respective 3-D
  graphics SDKs. The D3D9 client was built with the MS DirectX February 2010
  SDK.
  You need the DirectX End-User Runtimes (February 2010 or newer) to run
  D3D9Client. It can be downloaded at Microsoft Download Center at

  http://www.microsoft.com/download/en/details.aspx?id=9033

  (I don't know whether later DX SDK's still support the DX9 interface, but it
  may be worth a try.)

* NVIDIA API (needed since stereoscopic option was introduced in the Client)
  The NVAPI can be downloaded from the nVIDIA developer site at

  https://developer.nvidia.com/nvapi

  If you don't like to install the NVIDIA API (it's not needed if stereoscopic
  option should not be available in the client), you might need to add an empty
  header "nvapi.h" into the main source directory (Orbitersdk\D3D9Client\).
  That file does not need to contain anything, but here's an example that says
  it all:

  // ==============================================================
  // nvapi.h
  // Dummy header to be able to build the D3D9Client without
  // the NVIDIA NvAPI interface installed.
  // ==============================================================


* To recompile the D3D9 source documentation you need Doxygen from

  http://www.stack.nl/~dimitri/doxygen/

  There is a doxygen configuration file in D3D9Client/doc/Doxyfile which
  can be used to generate the compressed html help file
  orbitersdk/D3D9Client/doc/D3D9Client.chm included in the repository.
  Additionally to doxygen itself some other packets might be needed (depending
  on the output format you would like to generate) to create the 'chm' file
  you need:
  - Microsoft HTML Help Workshop (compiler for .CHM files)
    http://go.microsoft.com/fwlink/?LinkId=14188
  - Graphviz (for the 'dot' tool to generate graphs)
    http://www.graphviz.org/


2. Installation
---------------

For a first-time installation only:

* Create a directory for the Orbiter distribution, e.g.
  Program Files\Orbiter_OVP

* Install the latest Orbiter release by unpacking the base package
  (orbiter100830.zip) in that directory. Make sure to preserve the directory
  structure of the zip archives.

* Download the latest (currently it's orbiter111105-100830diff.7z) orbiter beta
  diffs from

  http://sourceforge.net/projects/orbitervis/files/Orbiter%20beta%20files/

  Unpack in the Orbiter directory. Make sure to preserve the directory
  structure of the zip archive. Answer "yes" if asked whether to overwrite
  existing files.
  The graphics API linking the orbiter core and external graphics clients
  is continuously evolving, so it is important to make sure you have the
  latest orbiter beta installed when compiling the current D3D9Client snapshot.

* Download the D3D9Client package from

  http://www.orbiter-forum.com/showthread.php?t=18431

  (it includes the source tree) and extract it as usual into the Orbiter
  directory. Again, make sure to preserve the directory structure of the zip/7z
  archive.

* Open the D3D9Client project (For Visual Studio 2008, the project file is in
  orbitersdk/D3D9Client/D3D9Client.sln, For Visual Studio 2010, the project
  file is orbitersdk/D3D9Client/D3D9ClientVS2010.sln) and compile
  (this requires the DX9 SDK to be installed).


3. Testing the D3D9 client
--------------------------

* If the compilation of the D3D9 client was successful, it has created a
  plugin in Modules/Plugin/D3D9Client.dll.

* Run the "no-graphics" version of orbiter (orbiter_ng) and activate
  the D3D7Client plugin from the Modules tab.

* You should now get a video tab in the Launchpad dialog. Configure your
  graphics driver and screen options, then launch a scenario.

* The current version of the D3D9 client is not yet complete. Some of the
  features of the orbiter built-in graphics engine are still missing.
  However, the client is slowly taking shape, and new features are added on
  a regular basis.
