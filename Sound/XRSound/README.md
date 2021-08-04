# XRSound
XRSound is an add-on for the Orbiter Space Flight Simulator that automatically plays sounds for all Orbiter vessels, as well as enabling custom Orbiter vessels to play custom sounds.


## License

XRSound is open source and is licensed under the MIT license. Refer to the [LICENSE](./LICENSE) file for details.

## Installing and Using XRSound

Refer to the [XRSound User Manual](./XRSound/assets/Doc/XRSound%20User%20Manual.pdf) for details about how to install and use XRSound.

## Building XRSound 

You do not need to build XRSound in order to use it with Orbiter. However, if you want to build XRSound from the source, follow the steps below. These instructions assume you are building both the 32-bit and 64-bit versions of XRSound. However, you may build only one version if you prefer.

1. Install Visual Studio 2019 from https://visualstudio.microsoft.com/downloads/.
2. Download the 32-bit and 64-bit versions of the irrKlang sound engine from https://www.ambiera.com/irrklang/downloads.html.
3. Download and install (or build) Orbiter 2016 or later from either https://github.com/mschweiger/orbiter or http://orbit.medphys.ucl.ac.uk/download.html.
4. Clone the XRSound repository from GitHub to your local machine with:
```bash
git clone git@github.com:mschweiger/orbiter.git
```
or
```bash
git clone https://github.com/mschweiger/orbiter.git
```

If you're looking for an excellent GUI that makes working with Git easier, I recommend [Tower](https://www.git-tower.com/).

5. Create four environment variables pointing to where you plan to install each of the above software packages:

* `ORBITER_ROOT` => your 32-bit Orbiter root folder
* `ORBITER_ROOT_X64` => your 64-bit Orbiter root folder
* `IRRKLANG_ROOT` => your 32-bit irrKlang root folder
* `IRRKLANG_ROOT_X64` => your 64-bit irrKlang root folder

6. Install 32-bit Orbiter to %ORBITER_ROOT%.
7. Install 64-bit Orbiter to %ORBITER_ROOT_X64%.
8. Extract 32-bit irrKlang to %IRRKLANG_ROOT%.
9. Extract 64-bit irrKlang to %IRRKLANG_ROOT_X64%.
10. Copy `%IRRKLANG_ROOT%\bin\win32-visualStudio\*.dll` to %ORBITER_ROOT%.
11. Copy `%IRRKLANG_ROOT_X64%\bin\winx64-visualStudio\*.dll` to %ORBITER_ROOT_X64%.
12. Copy everything under `XRSound\assets\XRSound\` to `%ORBITER_ROOT%\XRSound` (or create a symbolic link if you prefer).
13. Copy everything under `XRSound\assets\XRSound\` to `%ORBITER_ROOT_X64%\XRSound` (or create a symbolic link if you prefer).

Now you are ready to compile and link XRSound.

14. Bring up Visual Studio 2019 and open the solution `orbiter\Sound\XRSound\XRSound\src\XRSound.sln`.
15. Set the build target to `Debug` and `x86` (i.e., 32-bit) to start. Click `Build -> Rebuild Solution`. This will build `XRSoundD.lib` and `XRSound.dll` and copy those files to their proper locations under `%ORBITER_ROOT%` via a Post-Build step. If you get build errors, double-check that the above environment variables are set correctly and that you restarted Visual Studio 2019 _after_ you defined those environment variables.
16. After the build succeeds, bring up Orbiter and activate the XRSound module as detailed in the [XRSound User Manual](./XRSound/assets/Doc/XRSound%20User%20Manual.pdf).
17. Launch a DeltaGlider scenario -- you should hear an audio greeting from your ship's new A.I. If you don't hear any sound, check your `%ORBITER_ROOT%\XRSound.log` file for error messages.

For more information and support about Orbiter and XRSound, visit https://www.orbiter-forum.com/.

Happy Orbiting!
