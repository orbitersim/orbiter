PREREQUISITES
=============

To build Orbiter from its sources you need a C++ compiler able to create Windows binaries.
Orbiter has been built successfully with Visual Studio 2019 Community
https://visualstudio.microsoft.com/downloads/

To quickly install Visual Studio components required for development, launch installer and 
import the [.vsconfig](./.vsconfig) file provided in the repository ([documentation](https://learn.microsoft.com/en-us/visualstudio/install/import-export-installation-configurations?view=vs-2022#import-a-configuration-using-the-visual-studio-installer))

If you want to build the Orbiter documentation, you need LaTeX. Multiple LaTeX distros for
Windows are available, for example MiKTeX.
https://miktex.org/download

To build the code-level documentation, you need doxygen.
https://www.doxygen.nl/index.html


BUILDING ORBITER
================
After cloning the Orbiter Git repository, you can either

- Load the Orbiter download directory as a local directory into VS2019 or later.
  
- Select Project | CMake Settings for Orbiter
  to check the build settings and make sure that all required components are found
  
- Select Project | Configure Orbiter
  This will configure the CMake build files.

- Select Build | Build All
  to build Orbiter and all its components.
  
Or, run CMake externally:

- Create a build directory separate from the Orbiter source directory.

- Run CMake, and select the correct source and build directories.

- Select Configure, and pick the Win32 platform

- Edit options as required.

- Select Generate, then close CMake

- This should have generated a solution file (Orbiter.sln) in the build directory.
  Load this into Visual Studio, and Build All.
  
  
PLANETARY TEXTURES
==================
The Orbiter git repository does not include the planetary texture files for most
celestial bodies. You need to install these separately (e.g. by installing Orbiter
2016 and optionally downloading high-res texture packs from the Orbiter website).

Set environment variable ORBITER_PLANET_TEXTURE_INSTALL_DIR in your profile so that
Orbiter build correctly configures the reference in configuration. 

Assuming Orbiter 2016 is installed in c:\orbiter2016:

```
setx ORBITER_PLANET_TEXTURE_INSTALL_DIR c:\orbiter2016\Textures
```

Alternatively, you can specify the location of the texture files as CMake variable:
```
cmake --preset windows-x64-debug -DORBITER_PLANET_TEXTURE_INSTALL_DIR=c:\orbiter2016\Textures
```
You can also set the planetary texture directory after building
Orbiter by setting the PlanetTexDir entry in Orbiter.cfg.


TROUBLESHOOTING
===============
* If you get errors during build, in particular when building documentation (pdf from
latex sources), try disabling multithreaded build support (limit to a single
thread). Some of the document converters/compilers you are using may not be
thread-safe. 

* CMake errors during build (cannot find system include files etc.). This may happen
when using the Ninja generator. You may need to install and configure vcpkg to allow
Ninja to find the VS2019 toolset (https://github.com/microsoft/vcpkg).

* LaTeX build components not found: If using MiKTeX, make sure you install it for all
users instead of locally for a single user. CMake won't automatically find the 
single-user installation, so you would have to specify the paths to all components
manually.

* Problems launching Orbiter from the build directory: If you use the VS2019
generator, it puts binaries in configuration-dependent subdirectories (Debug/Release).
This means that Orbiter may not find plugin DLLs. You need to run Orbiter from
the install directory. The Ninja generator separates the Debug and Release builds
at the top level and doesn't have that problem.