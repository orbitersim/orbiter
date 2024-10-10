Orbiter 2024 Readme
===================

1. Orbiter instalation
----------------------
Create a new folder for the Orbiter installation, e.g. C:\Orbiter
or \%HOMEPATH\%\Orbiter. Note that creating an Orbiter folder in
Program files or Program files (x86) is not recommended, because
Windows puts some restrictions on these locations.

If a previous version of Orbiter is already installed on your
computer, you should not install the new version into the same
folder, because this could lead to file conflicts. You may want
to keep your old installation until you have made sure that the
latest version works without problems. Multiple Orbiter
installations can exist on the same computer.

Unzip the Orbiter ZIP installation package into the new folder,
using either the default Windows unzip function, or an external
tool like 7-zip or WinZip. Important: Take care to preserve the
directory structure of the package (for example, in WinZip this
requires to activate the "Use Folder Names" option).

After unzipping the package, make sure your Orbiter folder
contains the executables (orbiter.exe and orbiter_ng.exe) and,
among other files, the Config, Meshes, Scenarios and Textures
subfolders.

To uninstall Orbiter, simply remove the Orbiter folders with
all contents and subdirectories. This will completely remove
Orbiter from your hard drive.


2. Runtime libraries
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TODO
--------------------
Note: Orbiter requires the VC2008 runtime libraries installed
on the computer. On most systems, these libraries will already
be installed.
The zip file package does not
install these libraries by default. If Orbiter fails to launch
with an error message similar to

"It wasn't possible to start this application because the
configuration of the application is incorrect. The reinstallation
of the application might correct this problem."

then the runtime libraries may be missing. You can install them
by executing the vcredist_x86.exe installer package in the Install
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
Orbiter graphics engines, or using the included D3D9Client plugin.
Once running, Orbiter will show you the "Launchpad" dialog, where
you can select video options and simulation parameters.

You are now ready to start, select a scenario from the Launchpad
dialog and click the "Launch Orbiter" button!


4. Help
-------
Help files are located in the Doc subfolder.
Orbiter User Manual.pdf is the main Orbiter manual, and it is
highly recommended you read it.

The in-game help system can be opened via the "Help" button on
the Orbiter Launchpad dialog, or with Alt-F1 while running
Orbiter.

Remaining questions can be posted on the Orbiter user forum at
https://orbiter-forum.com.