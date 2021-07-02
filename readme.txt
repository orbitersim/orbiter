Orbiter 2016 Readme
===================

1. Orbiter distribution formats
-------------------------------
Orbiter 2016 is distributed in two formats:

- Microsoft Installer package: Orbiter2016.msi
- Zip file archive Orbiter2016.zip

The contents of both packages are identical. Users can choose
whichever package format they prefer.

The MSI installer package provides a convenient way to install
Orbiter 2016. It comes with a graphical user interface that allows
to select the installation location, component selection, destop
icon, and start menu item. It also provides a removal function.
This method is particularly useful for new users.

The zip-file archive package must be extracted manually into a
target directory, using either the Windows built-in zip extraction
facility, or a 3rd party utility such as 7-zip.
The directory structure of the zip archive must be preserved when
extracting to the target folder.
This method is self-contained and portable. It does not
modify the registry, and does not add or change files outside the
installation folder. Multiple installations are possible.
Deinstallation only requires deletion of the Orbiter folder and
all subfolders.


2. Runtime libraries
--------------------
Note: Orbiter requires the VC2015 runtime libraries installed
on the computer. On most systems, these libraries will already
be installed. The MSI installer package will install these
libraries automatically if required (unless disabled by the user
during a custom installation). The zip file package does not
install these libraries by default. If Orbiter fails to launch
with an error message similar to

"It wasn't possible to start this application because the
configuration of the application is incorrect. The reinstallation
of the application might correct this problem."

then the runtime libraries may be missing. You can install them
by executing the vcredist.x86.exe installer package in the Install
folder, or by downloading from the Microsoft web site.

Note that some 3rd party Orbiter addons may require additional
runtime versions. Refer to the installation instructions of the
addon.

3. Launching Orbiter
--------------------
The Orbiter simulator can be launched either with a built-in
graphics engine (orbiter.exe, with the red "Delta-glider" icon),
or with an external graphics interface (orbiter_ng.exe, with
the blue "Delta-glider" icon). The second option allows to
connect to external graphics engines with enhanced features and
performance. This requires downloading and installing 3rd party
Orbiter graphics engines, such as the D3D9Client plugin.

4. Help
-------
Help files are located in the Doc subfolder. Orbiter.pdf is the
main Orbiter user manual.

The in-game help system can be opened via the "Help" button on
the Orbiter Launchpad dialog, or with Alt-F1 while running
Orbiter.

Remaining questions can be posted on the Orbiter user forum at
orbiter-forum.com.