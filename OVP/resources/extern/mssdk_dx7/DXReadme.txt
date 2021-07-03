Microsoft(R) DirectX(R) 7.0a Software Development Kit

__________________________________________________________________


Contents:

- *** DirectX 7.0a Update ***
- Installation
- Updated SDK
- CD Layout
- What's New
- Known Issues 
- Compiler Support

__________________________________________________________________

*** DirectX 7.0a Update ***

This is an update to the DirectX 7.0 SDK.  The primary change is with
the DirectX run-time install and redistributable components.  Specifically
the changes are to DirectInput.  We have also taken this opportunity to
update the D3DX library and DirectMusic Producer.  The remainder of this
SDK is exactly that of the DirectX 7.0 SDK

DirectInput Run-Time and Redistributable Update:

   After DirectX 7.0 was released, problems were discovered which 
   negatively affected some titles' compatibility with gaming input 
   devices (mostly USB). The DirectInput component had flaws that caused 
   legacy interfaces to act slightly different than previous releases. 
   Although the differences were small, a few legacy applications began 
   to function improperly. Due to these issues, we have updated the 
   DirectInput system components and repackaged the DirectX redistributable
   files in order for you to include them with your latest releases.

   Specifically, updates were made in the following areas:

     - USB device compatibility is improved.
     - USB device configuration persistence is withheld across reboots, 
       and configuration data is produced more accurately.
     - Legacy applications are not able to receive exclusive keyboard 
       access (only DirectX 7.0 apps should be able to do this).
     - Force feedback performance has been improved. This should improve 
       performance on Red Baron 3D and the Microsoft Flight Simulator 
       line of applications, as well as others.


D3DX Library Update:

   This SDK includes a new version of the D3DX library.  The new version 
   contains the following bug fixes:

     - Fixed problems in cooperative level settings for multiple 
       D3DXContexts on the same DDraw device
     - Fixed memory leak in ID3DXContext::Resize
     - Fixed incorrect handing of SrcRect and DstRect in D3DXLoadTexture 
       functions.
     - Fixed incorrect setting of DEVICEDESC.ddGuid in 
       D3DXGetDeviceDescription
     - Fixes in D3DXMatrixLootAt and D3DXMatrixLookAtLH
     - Fixed problem in D3DXCreateContext handling windows created 
       minimized

   The D3DX samples have been rebuilt using the new library.


DirectMusic Producer Update:

   This DirectX 7.0a update to DirectMusic Producer was primarily in
   response to a bug in DLS Designer that could disable playback of DLS
   content on a DLS-2 synth.  It was discovered that the velocity range of
   DLS regions is being incorrectly written as 0-0 (rather than 0-127).
   Playback on DLS-1 synths is not affected (regions are not sorted into
   multiple layers that track velocity) but it could cause content not to 
   play on DLS-2 synths.  Because of the seriousness of this bug it is 
   important to update all DLS collections created in previous versions of 
   DirectMusic Producer (by opening and saving in Producer) to avoid this 
   problem in the future. 

   Additionally, there are three bug fixes included in this release: 
     - Segment Designer GPF when loading in a runtime version of a segment 
       file with a sequence track that has pchannels numbered higher than 16
     - DLS-2 collections GPF when loaded into Producer
     - Allocated memory size for DLS collections is a byte short and in some 
       limited circumstances can cause a GPF in software synths (if the synth 
       is not designed to work around this limitation)

___________________________________________________________________

INSTALLATION:

You should UNINSTALL PREVIOUS RELEASES of the DirectX SDK prior to 
installing DirectX 7.0a (see UnInstall).  The directory structure and file 
names have changed significantly.  

To install the DirectX 7.0a SDK and/or Runtime, select "Install DirectX 
Foundation 7.0a SDK" from the main menu or execute setup.exe in the
\DXF directory of the CD.  If you are installing over the top of an
existing MSDN Platform SDK, read the "UPDATED SDK" section of this readme.


UnInstall:

To uninstall, use "Add/Remove Programs" from the Control Panel and 
uninstall this or any previous versions of the DirectX Foundation SDK.

__________________________________________________________________


UPDATED SDK

New to this DirectX SDK is the addition of support for Visual Basic
developers.  You can chose to install "C" sample code, Visual Basic 
sample code or both.  A "Complete" installation will install both.
Use the "Custom" installation to minimize installation time and
required disk space.

As with the previous DX6.1 release, we've tried to integrate better 
with the MSDN Platform SDK.  If you have a previous installation of
the Platform SDK, installation of the DX7a Foundation SDK will detect 
this and offer to install to that directory.  Otherwise, you can 
choose to install the SDK wherever you'd like.  

__________________________________________________________________


CD LAYOUT

The following is a brief description of the directories found in the \DXF
directory of this CD.  Depending on options specified during installation,
some of these directories can be installed on your hard drive.

\Bin
    High level DirectX applications & tools.
    All can be accessed from the Start menu if "Utilities" are installed.

\Debug
    Debug versions of the DirectX 7.0a DLLs.

\Doc
    Contains documentation for the DirectX APIs.  The latest documentation
    for DX7 is best viewed with HTMLHelp.  The HTMLHelp viewer requires
    installation Internet Explorer 5.0 (IE5).  This is only required on Win95
    installations.  Win98 and Win2K will have the required IE components.
    To install IE5 visit the web page at http://www.microsoft.com/ie.
    If you chose not to install IE, you can still view the DX7 documentation
    by accessing the files in DXF\docs\directx7\word.

\Essentls
    DMusProd (Direct Music Producer)
       - Direct Music Producer is the authoring tool for Direct Music.  It
         allows composers to use the interactive and variable resources of 
         DirectMusic along with the consistent sound performance of DLS.  The
         Music Producer setup program and all files are located here.
 
\Extras
    DDSPlugin
       - This contains a Photoshop plugin to support import and export of 
         DXTn-compressed textures using the DDS file format.
    DMusic
       - This directory contains a single file, loader.zip.  It contains a
         piece of example code that shows how an application can write its
         own DirectMusic loader.  It is included here for educational purposes
         only, and should be used as a reference only.
    DSound
       - This directory contains a single file, mulchaud.doc.  It describes
         the new waveformat structures for multi-channel and high bit-depth
         audio data files.  The previous waveformat structure was limited to
         8 and 16-bit integer formats.  Further, when the number of channels
         was greater than two, the meaning of the additional channels was 
         un-specified.  Using the formats in this specification, an application
         may create or use wave files with more than two channels and with 
         formats not restricted to 8 or 16-bit integer.  Where the number of
         channels is greater than two, the proper outputs and speaker 
         designations can be specified.
    DX Symbols
       - We have added symbol files to assist developers with debugging.
         You will find retail and debug for Win9x (.sym & .pdb).
    VTune
       - VTune(TM) Performance Analyzer, version 4.0, from Intel(R).  A trial 
         version of the VTune Performance Analyser is included in this directory.
         The VTune Performance Analyzer is designed to provide an integrated 
         tuning environment for Microsoft Windows 95, Windows 98, and Windows NT
         systems.  The VTune Performance Analyzer collects, analyzes, and 
         provides Intel Architecture-specific software performance data from the
         system-wide view down to a specific module, function, or instruction. 
         The VTune coach provides recommendations on how code can be modified to
         increase performance on Intel Architecture.  Additional information on
         this software as well as other Intel software performance products is 
         available at http://developer.intel.com/vtune.
         Note: Microsoft is not responsible for the testing, stability or 
         accuracy of this product.
    Win98 PIII DBG
       - The file contained in this directory (VMCPD.VXD) is necessary when 
         debugging D3D applications on Win98 Pentium III machines with DX6.1 
         or DX7.  Note:  This is an issue on Win98 only (not Win98 SE).
         See the readme.txt in that directory for more details.
  
\Include
    contains include files for DirectDraw, Direct3D, DirectSound, 
    DirectInput, DirectPlay and DirectMusic

\Lib
    contains library files for DirectDraw, Direct3D, DirectSound, 
    DirectInput and DirectPlay.
    *.LIB : COFF libraries (used by Microsoft Visual C++ 2.0 or higher).
    There is also a sub-directory that contains Borland 11.0 versions 
    of the libraries.

\License
    Text versions of the DirectX SDK and End User License Agreements and 
    the Redistributable License Agreement.

\Redist
    Redistributable versions of the DirectX 7.0a DLLs.

\Samples
    Contains all sample code and sample binaries.   See readme.txt file 
    in each sample's directory for more details.  Most samples can be
    accessed from the Start menu.
	
__________________________________________________________________


WHAT'S NEW:

DirectX for Visual Basic
========================
DirectX has traditionally been the domain of C and C++ developers developing
game content. By providing language support for Visual Basic, the DirectX API
is opened up to a new kind of developer writing similar or very different kinds
of applications.  The goal of the project is simply to provide the same access
to high performance multimedia functionality that's available to C developers.
To that end, we've also extended the SDK with Visual Basic sample code and 
helper controls to make DirectX accessible and easier to understand for the 
Visual Basic programmer.

At the core of DirectX for Visual Basic is a DLL that marshals objects between
the DirectX run time and Visual Basic. The DLL exposes DirectX functionality 
using types that are friendly to Visual Basic and insulates the Visual Basic 
language developer from some of DirectX's object semantics. To provide the most
flexibility and speed, the object model mirrors that of C.

Reference the DirectX documentation, which is now multilingual (C and VB), and
the Visual Basic sample code in the SDK for additional information.


Direct3DIM
==========

- Direct3D for DirectX 7.0 introduces a new set of interfaces which offer a 
simplified programming model and higher performance, as well as a number of 
new features.  The programming model is simplified though a consolidation of 
Direct3D interfaces.  Material, viewport, and light information is now part 
of the device state instead of being separate objects.  The need for separate
texture objects has been eliminated, and textures are now simply DirectDraw
surfaces.

Features enabled through these new interfaces include:
   - hardware transform and lighting
   - vertex blending
   - arbitrary clip planes
   - cube environment maps
   - texture transforms and projected textures
   - enhanced texture management
   - state blocks


D3DX
====
D3DX is a helper API, based on COM, that sits on top of the Direct3D & 
DirectDraw of DirectX 7.0. It is designed as a set of header files and a 
library (d3dx.lib for retail and d3dxd.lib for debug) that applications can 
link to statically. The Direct3DX utility library provides helper functions to
simplfy the common tasks encountered by 3-D graphics developers. 

The utility library is designed for developers who want a quick and efficient
way to use Direct3D Immediate Mode. By eliminating some of the more tedious
tasks, the utility library provides a tool-kit of short-cuts, allowing 
developers to immediately focus on the core application or game development. 

The Direct3DX utility library provides helper functionality for enumerating 
device configurations, setting up a device, running full-screen or windowed 
mode uniformly, running resizing operations, calculating vector and matrix 
operations, and simplifying image file loading and texture creation. 
In addition, it provides functions for drawing simple shapes, sprites, and 
cube maps. However, Direct3DX does not provide a scene hierarchy or support 
X Files.

Note that the utility library requires that at least DirectX 7.0 is installed 
on your system. The Direct3DX utility library supports multiple monitor 
configurations. Note that none of the Direct3DX objects are multithread-safe. 
A multithreaded application must add its own locks and synchronization 
primitives to access Direct3DX objects.


DirectDraw
==========

- For DX7 there is a new DirectDraw Interface. This is IDirectDraw7.  
Note that the relationship between older DirectDraw interfaces and Direct3D 
interfaces is as follows:

Changes have been made to segregate DirectX7 Direct3D interfaces from older 
version interfaces. There are now two classes of DirectDraw objects: those 
created by DirectDrawCreateEx, which are DX7 objects, and those created by 
DirectDrawCreate, which are older pre-DX7 objects. While both DX7 and 
pre-DX7 DirectDraw objects support all (up to and including IDirectDraw*7) 
interfaces, only the DX7 DirectDraw object supports the DX7 D3D interface 
IDirect3D7. Additionally, Direct3D7 interfaces methods deal with IDirectDraw*7 
interfaces exclusively.

The practical result is that to use legacy Direct3D interfaces, applications 
should not use version 7 DirectDraw interfaces, and to use DirectX 7 Direct3D 
interfaces, applications should use DirectX 7 DirectDraw interfaces obtained 
from DirectDrawCreateEx. 

As a concrete example, an application would use DirectDrawCreateEx to create
a DirectX 7 DirectDraw object, and receives an IDirectDraw7 pointer from that
function. The application then uses QueryInterface to retrieve an IDirect3D7
interface, and IDirectDraw7::CreateSurface to create surfaces and receive 
IDirectDrawSurface7 interfaces. Since the DirectDraw objects support all
interface levels, it is possible to QueryInterface for IDirectDrawSurface4
from the IDirectDrawSurface7 interface in order, for example, to use this 
surface with DirectShow.  Since the DirectX7 DirectDraw object does not support
IDirect3D4 and below interfaces, it is not possible to use QueryInterface to
retrieve an IDirect3D4 interface, even from older DirectDraw interfaces 
obtained (via QueryInterface or any other method) from this DirectDraw object.

Additionally, in previous interfaces textures were created through the 
IDirect3DTexture interface. With DX7 this interface goes away. Instead the 
GetTexture, SetTexture and Load methods on IDirect3DDevice7 take 
IDirectDrawSurface7 pointers. The IdirectDrawSurface7 interface includes 
the following related methods:
   - SetPriority and GetPriority
   - SetLOD and GetLOD
 
In addition to these interface changes, DirectDraw has support for two new 
features. These are:
   Stereo - This includes support for stereo flipping surfaces and refresh 
            rate enumeration.
   Cubic environment mapping - DirectDraw supports the atomic creation of 
            these complex surface structures. The application can then treat
            each individual face in the structure as a DirectDraw surface, and
            thus lock, GetDC, or BLT images into these surfaces, or use them as
            render targets.


DInput
======
Dinput has added the ablility to disable the windows key by acquiring keyboard
devices in exclusive mode.  Also, the addition of Fedit and new interfaces help
to simplify the process of authoring complex forces.


DirectMusic
===========

- This version of DirectMusic supports DLS hardware acceleration on the Win 98SE 
and Win 2000 platforms. Also, DirectMusic can now playback DLS2 files on 
DirectMusic synthesizers that support DLS2. (Please note that the DirectMusic 
software synthesizer DOES NOT support DLS2 playback in this release). 
Additionally, DirectMusic now supports the "RMID" format.  This format combines,
into a single file, a DLS collection and associated MIDI file.


DirectMusic Producer
====================
This version of Producer features many new features to aid in both authoring 
and auditioning of DirectMusic content, including an expanded secondary segment 
toolbar, an Echo MIDI In feature to map the output of your sequencer to MSSynth, 
as well as Wave and MIDI file export.  Many ease-of-use enhancements are also 
included, such as complete display and edit of sequence tracks, hybrid notation, 
display of pchannel names on output status bars, multiple instrument mixing and
the ability to edit wave loop points via the property page.


DirectPlay
==========

- DirectPlay has added support for ripple launching. Ripple launching is 
defined as launching an application using another. This is one technique used 
for encryption protection. DPlay handles the ripple launching through the use
of command line parameters entered in the registry during application 
installation. This can be read about in the SDK help pages.


DirectSound
===========

- This version of DirectSound provides a built-in hardware voice management.  
With this feature, an application can create DirectSound buffers that are not 
allocated to hardware or software until the buffer is played.  This provides 
for maximum utilization of audio hardware buffers.

- DirectSound 7 also provides a selectable 3D HEL.  This allows an application 
to select on a buffer-by-buffer basis the software algorithm to use when 
processing 3D sounds.  There are three available algorithms: NO_VIRTUALIZATION,
which maps x,y,z coordinates to simple left-right panning, Doppler and volume
scaling.  NO_VIRTUALIZATION uses the same amount of CPU as a normal, non-3D 
buffer. HRTF_LIGHT provides a simple and fairly CPU efficient algorithm and 
HRTF_FULL provides a very compelling 3D audio effect for somewhat more CPU.
Note that HRTF_LIGHT and HRTF_FULL are only available on Windows 98 Second 
Edition.

- DirectSound also allows, on Windows 98 Second Edition and Windows 2000, 
the ability to create and playback multi-channel buffers of arbitrary format
on systems with WDM audio drivers.

- When using the debug version of DirectSound, DirectSound buffers are 
initialized with a static-like noise.  This allows an application running
debug DirectSound to determine if it is attempting to play a buffer that has
not had valid audio data written to it.  If you hear a burst of noise when
you play a DirectSound buffer, this is the most likely cause.

__________________________________________________________________


KNOWN ISSUES


Documentation
=============

- The HTML Help engine that the DirectX SDK documentation uses can print 
multiple topics from a section, with the following limitations:
     1) Some formatting (fonts, alignment) is lost.
     2) Language filtering is not performed.

- The fifth parameter for the IDirect3DVertexBuffer7::ProcessVerticesStrided 
function is incorrectly listed in the Documentation.  The correct prototype
and parameter description is:

     HRESULT ProcessVerticesStrided( 
       DWORD dwVertexOp, 
       DWORD dwDestIndex, 
       DWORD dwCount, 
       LPD3DDRAWPRIMITIVESTRIDEDDATA lpVertexArray, 
       DWORD dwVertexTypeDesc, 
       LPDIRECT3DDEVICE7 lpD3DDevice, 
       DWORD dwFlags
     );

     dwVertexTypeDesc
       A combination of flexible vertex format flags that describes the
       vertex format.  


DirectDraw/Direct3D IM
======================

- 3dfx Voodoo2 and Windows 2000
DirectDraw for Windows 2000 does not fully support the 
passthrough feature of the 3dfx Voodoo2. A workaround has been put in 
place that causes DirectDrawEnumerate to list any secondary (multimon) 
devices as passthrough-style devices. Thus, to develop software for use 
with the Voodoo2, developers should setup the Voodoo2 as a secondary 
multimon device, and then enable the DirectDraw workaround. The workaround 
is enabled by setting the following DWORD registry entry to a non-zero 
value: HKLM\Software\Microsoft\DirectDraw\EnumerateAttachedSecondaries.

- There are known limitations with using the legacy D3DLIGHT structure:  
1) D3DLIGHTSTATE_COLORVERTEX light state is not only not supported, but 
the behavior is incorrect. It is supported in specular calculations and 
is not supported in diffuse calculations;
2)  Specular calculations with point and spot lights is incorrect if 
D3DLIGHT structure was used to create these lights. The attenuation 
factor is divided by the distance from light source to the vertex.

- Exceptions are unmasked on Win98 Pentium III machines when debugging.  
On Win98, this can produce "Unhandled exceptions" or "Illegal Instructions".
To correct this problem, you will need to copy a file.  You'll find more 
information in the "\DXF\Extras\Win98 PIII DBG" directory.

- For performance reasons, a rendering device that supports the new 
IDirect3DDevice7 interface sets the FPU to single-precision, round-to-nearest
mode with exceptions disabled by default. This behavior is opposite from 
that of devices that expose legacy interfaces such as IDirect3DDevice3.
For more information, see the "DirectDraw Cooperative Levels and FPU 
Precision" topic in the Direct3D Immediate Mode SDK documentation.

- When using STL with Direct3D, the STL header must be included after d3d.h.

- Some popular cards cannot create render targets unless color buffers and 
z-buffers are of the same bit depth.  The constraints are:
     16-bit color buffers require 16-bit z-buffer (no stencils available)
     32-bit color buffers require 32-bit z-buffer of which 8 bits can be 
          stencil if desired.
If these conditions are not met, then the calls will fail on either 
CreateDevice or at SetTendertarget().  If you see such behavior and need 
to avoid it, use GetDeviceIdentifier().


DirectPlay
==========

- The IDirectPlay4A::EnumConnections( ) API will always return single-byte 
character descriptions of the service provider available on the system.  
In order to obtain fully localized multibyte descriptions without having 
to convert your entire application to Unicode, the following procedure 
may be used:

     a) QueryInterface for an IDirectPlay4 interface (Unicode)
     b) call IDirectPlay4::EnumConnections - this will give you Unicode 
        strings
     c) convert each string to ANSI using the Windows WideCharToMultiByte( )
        function
     d) release the IDirectPlay4 interface
     e) continue to use the IDirectPlay4A interface for the rest of your 
        application

     Alternatively, you can QueryInterface for an IDirectPlay3A interface. 
This will return localized multibyte character strings and eliminate step c).

- Incorrectly using the latest US Robotics drivers on older modems has been
known to cause problems hosting a session.  This can be fixed by downloading
the appropriate drivers for your modem from the 3Com site.

- Disconnecting the phone line while enumerating modem sessions can 
result in lock ups.

- Using the Protocol flag in DirectPlay may not work through the ICS 
(Internet Connection Sharing) feature of Windows 98SE. Windows 2000 does not 
have this problem.


Direct3DRM for Visual Basic
===========================

- D3DRM Callbacks.  Callbacks such as Frame3.AddMoveCallback and
Object.AddDestroyCallback must be cleaned up by removing them before the 
application exits.  For example, callbacks created with 
Frame3.AddMoveCallback must be removed by calling Frame3.DeleteMoveCallback.
Failure to clean up callbacks may result in a system crash.


Samples 
=======

- The D3DIM samples can be run with the DX7 reference rasterizer, however
the required registry key is not set by default.  You can set the proper
registry key by using the files in the \DXF\samples\Multimedia\D3DIM\bin 
directory.  Without the software rasterizer registry key set you will get 
a message suggesting to enable it when first trying DX7 specific samples
such as BumpMap.  Because this is a software rasterizer you should expect 
very slow frame rates.

- Depending on the resolution of your desktop and the available amount of video 
memory in your system, there may not be enough video memory for the sample 
textures.  In this case, the D3DIM samples will still run but will not be able 
to display textures (surfaces will be white).  In order to see the textures you 
can lower your desktop resolution or use a display card with more memory.

- The DMHook DLL (helper DLL for DMShell) cannot be compiled with Watcom or 
Borland compilers. Watcom and Borland users should use the pre-made DMHook.dll
that is supplied with the SDK sample binaries.

- There are miscellaneous issues with some display hardware & drivers.
Please let us know of any problems you encounter and specify whether you
believe it is sample or driver related.  You can use the reference rasterizer 
to help this determination.

- If you are using Japanese version of Windows, dialog box text in sample
programs may not display Kanji characters. In order to display Kanji
characters, you need to edit .RC file to change the dialog font to 
Japanese font.

- For further information on samples, reference the dxreadme.txt at
<drive:>\(sdk path)\samples\multimedia.


Other
=====

- If a machine is upgraded to Win2000 after the DirectX 7a SDK is installed,
the DirectX Control Panel will not migrate properly.  However, you can 
add the DirectX Control Panel by reinstalling the Direct 7a SDK following
the upgrade to Win2000.

__________________________________________________________________


COMPILER SUPPORT

C/C++
=====

DX7 samples will work with VC++ 4.2 or better.  We have included 
VC project files (.mdp for support of VC 4.2) and makefiles for VC.

As in previous release, we continue to attempt to offer some support for
Watcom v11.0 and Borland CBuilder 3 users.  However, the following samples
will not compile with Watcom and Borland as this release was made: 

   Watcom: 1) The D3DIM Screensaver sample;  2) the DMusic sample DMHook.

   Borland: 1) The DMusic DLL DMhook;  2) DDraw's DXTex;  3) the D3DIM 
            Screensaver; and 4) D3DIM's VideoTex.

When installing your compiler, we highly recommend installing with support 
for MFC.  Many of our samples utilize MFC.  Without this support, some
samples will not compile.

Also, when compiling with Watcom or Borland, you must be in the directory
of the sample wish to compile.  The dxall.mak files provide to compile all
samples will only work with Visual C++.

For instructions on building the samples, open
CDRom:\DXF\samples\multimedia\DXReadme.txt.  In this readme you will find 
sections on How to Build and Notes for VC++ Users.  The SDK will install 
this readme file to <drive:>\(sdk path)\samples\multimedia.


Visual Basic
============

The DirectX 7 SDK will support Microsoft Visual Basic v5.0 with service 
pack 3 (SP3) and higher.  Installing the DirectX 7 SDK on a system where
a lesser version of Visual Basic is installed will upgrade system files
to Visual Basic 6.0.  

If you distribute Visual Basic applications using DirectX Events to 
Windows 95 users, it is important that you include DCOM95 in your installer.
Otherwise users running Windows 95 may experience page faults when exiting 
the application.  The DCOM components are included with Internet Explorer 4
and Visual Basic, however you will not know what your end user may have 
installed.  To ensure your Windows 95 users do not experience problems, it 
would be wise to include this.  You can find DCOM95 on your VB5 cd, or at 
the web site http://www.microsoft.com/dcom.

