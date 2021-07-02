ORBITER BETA GENERAL INFORMATION
================================

Beta Installation:
------------------

* The beta packages contain diffs from the latest release version (currently
  Orbiter 2006-P1, v.060929). To install the beta:

* Create a fresh installation of Orbiter 2006-P1 (release 060929) containing
  base and SDK packages. High-res texture packages are optional. The Orbiter
  release can be found on the download page of the Orbiter site at
  http://orbit.medphys.ucl.ac.uk/

* Unpack the beta package inside the Orbiter root directory. Make sure to
  preserve the directory structure of the archive. Answer "YES" if asked
  whether to overwrite existing files.


Non-graphics Orbiter version
----------------------------

* Development of a Orbiter version without its own graphics subsystem is
  currently under way (in planning/very early development). This version
  will allow external graphics clients to be attached to the core in the
  form of DLL plugins. The graphics clients communicate with the Orbiter
  core via a dedicated graphics API.

* Orbiter_NG: The executable of the "non-graphics" Orbiter version resides
  in Modules/Server/orbiter.exe. You can launch it via the "Orbiter_NG
  shortcut in the Orbiter root directory, but you may have to edit the
  symbolic link target in the sortcut to reflect your installation.

* DX7 client: An experimental skeleton DX7 client for Orbiter_NG is provided
  in Modules/Plugin/D3D7Client.dll. Use this only with Orbiter_NG, not with
  the standard Orbiter executable. This executable has been compiled from
  the sources available from the Orbiter Visualisation Project on
  sourceforge. It is in the early stages of development, and intended for
  developers, rather than for public use.

* Other graphics clients: Don't exist yet.
