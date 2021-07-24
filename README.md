![Orbiter logo](./Src/Orbiter/Bitmaps/banner.png)

# Orbiter Space Flight Simulator

Orbiter is a spaceflight simulator based on Newtonian mechanics. Its playground
is our solar system with many of its major bodies – the sun, planets and moons.
You take control of a spacecraft – either historic, hypothetical, or purely
science fiction. Orbiter is unlike most commercial computer games with a space
theme – there are no predefined missions to complete (except the ones you set
yourself), no aliens to destroy and no goods to trade. Instead, you will get a
pretty good idea about what is involved in real space flight – how to plan an
ascent into orbit, how to rendezvous with a space station, or how to fly to
another planet. It is more difficult, but also more of a challenge. Some people
get hooked, others get bored. Finding out for yourself is easy – simply give it
a try. Orbiter is free, so you don’t need to invest more than a bit of your
spare time.

## License

Orbiter is now published as an Open Source project under the MIT License (see
[LICENSE](./LICENSE) file for details).

## Installation

Get the Orbiter source repository from github
```bash
git clone https://github.com/orbiter-project/orbiter.git
```

To configure and generate the makefiles, you need a recent
[CMake](https://cmake.org/download/).

To compile Orbiter from its sources, you need
[Microsoft Visual Studio](https://visualstudio.microsoft.com/downloads/).
Orbiter has been successfully built with VS Community 2019, but other versions should
also work. Note that VS2019 comes with built-in CMake support, so you don't
need a separate CMake installation.

If you are using the [Ninja](https://cmake.org/cmake/help/latest/generator/Ninja.html)
generator (default for the VS built-in CMake), you may also need
[vspkg](https://github.com/microsoft/vcpkg) to configure the VS toolset.

Orbiter is a 32-bit application. Be sure to configure vspkg and CMake accordingly.

If you want to build the documentation, you need a few additional tools:
- a filter to convert ODT and DOC sources to PDF, such as
  [LibreOffice](https://www.libreoffice.org/download/download/).
- a LaTeX compiler suite such as [MiKTeX](https://miktex.org/download).
- [Doxygen](https://www.doxygen.nl/index.html) for building the source-level
  documentation for developers.

By default, the Orbiter version with built-in DX7 graphics engine is built.
If you want to build the server version for external graphics client support,
set the ORBITER_BUILD_WITH_DX7 CMake option to OFF. Both versions can be
built into the same build directory. The version with built-in DX7 graphics
is launched with the `./orbiter.exe` executable. The server version is
located in `./Modules/Server/orbiter.exe` and is launched with `./orbiter_ng.exe`.
The Orbiter source distribution ships with a DX7 reference graphics client
(D3D7Client) with essentially the same functionality as the built-in graphics
client. Use 3rd party client implementations to make use of more modern
graphics engines.

See [README.compile](./README.compile) for details on building Orbiter.

## Planet textures

The Orbiter git repository does not include most of the planetary texture files
required for running Orbiter.
You need to install those separately. The easiest way to do so is by installing
[Orbiter 2016](http://orbit.medphys.ucl.ac.uk/download.html). Optionally you can
also install high-resolution versions of the textures from the Orbiter website.
You should keep the Orbiter 2016 installation separate from your Orbiter git
repository.

To configure Orbiter to use the 2016 texture installation, set the
ORBITER_PLANET_TEXTURE_INSTALL_DIR entry in CMake. For example, if Orbiter 2016
was installed in `C:\Orbiter2016`, the CMake option should be set to
`C:/Orbiter2016/Textures`.
Alternatively, you can configure the texture directory after building Orbiter
by setting the `PlanetTexDir` entry in `Orbiter.cfg`.

## Help

Help files are located in the Doc subfolder (if you built them). Orbiter.pdf is the
main Orbiter user manual.

The in-game help system can be opened via the "Help" button on
the Orbiter Launchpad dialog, or with Alt-F1 while running
Orbiter.

Remaining questions can be posted on the Orbiter user forum at
[orbiter-forum.com](https://www.orbiter-forum.com).