ORBITER VISUALISATION PROJECT
Graphics Client Plugins for Orbiter Space Flight Simulator
Copyright (C) 2006-2014  Martin Schweiger


The aim of the Orbiter Visualisation Project (OVP) is to provide 3-D
visualisation support for Orbiter Space Flight Simulator via external plugin
modules.

The next Orbiter release distribution will include a "non-graphics" version
of the Orbiter core executable. This can be used as a standalone application
(e.g. as a server in a multi-user environment), but it also allows to load
a graphics client module to provide visualisation support. 

Separating the physics engine (orbiter core) and 3-D graphics support
(graphics clients) will ease code maintenance and provide an upgrade path
for future versions of rendering interfaces. It also allows developers to
implement new graphics features not found in the standard Orbiter
distribution.

OVP is an open source project that spans a number of (relatively independent)
client implementations.

- A DirectX-7 client (D3D7Client) which implements most of the graphics
  features of the Orbiter inline graphics engine. This is intended as a
  reference implementation.

- A DirectX-9 client

- An OpenGL client

Other clients may be added in the future.


Installing OVP
--------------
The installation requirements for the different clients may vary. Check the
documentation in the individual client directories for installation and
compilation prerequisites.

Compiling the D3D7 client requires Microsoft Visual Studio. It was tested with
VS2008. Later versions should work, but you will have to convert the project
files and property sheets to newer versions.

Before building an OVP client, you should download the latest Orbiter beta snaphot
(the OVP HEAD revision is usually linked against the latest Orbiter beta).
See http://orbit.medphys.ucl.ac.uk/betainstall.html for Orbiter beta installation
instructions.

- Download the OVP sources. You need an SVN client:

  svn co svn://svn.code.sf.net/p/orbitervis/code ovp

- Set the Orbiter installation directory: Open
  ovp\resources\PropertySheets\orbiterroot.vsprops with a text editor and edit
  the "Value" entry to point to your Orbiter root directory.
  
- Set the OVP source directory: Open ovp\resources\PropertySheets\ovproot.vsprops
  and edit the "Value" entry to point to the main OVP directory you just checked
  out.
  
- Open the solution file ovp\D3D7Client\D3D7Client.sln in Visual Studio and compile.
  On successful completion, the client libraries will have been placed in the
  correct locations of your Orbiter installation (provided you set the orbiterroot
  entry correctly) and are ready to use.
  
- Launch orbiter_ng.exe from your Orbiter main directory, activate the D3D7Client
  module on the Modules tab, set the parameters on the Video tab, and launch a
  scenario.

