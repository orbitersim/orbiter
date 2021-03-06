# XRSound-lib-linktest
This simple DLL solution is only so we can easily test the `XRSound.lib` and `XRSoundD.lib` build artifacts generated by the Orbiter CMake project. The .h and .lib include paths are all relative, so this project should be able to build successfully with no additional configuration, provided you did not move the project folder. No build artifacts from this project should be released for end users.

## Building this test app to verify the Orbiter CMake XRSound.lib and XRSoundD.lib build artifacts

Before attempting to build this test app, first build all four Orbiter build configurations using CMake:
* `x86-Debug`
* `x86-Release`
* `x64-Debug`
* `x64-Release`

Then open `XRSound-lib-linktest.sln`. You can test either one specific build target (see table below) by building and linking that configuration, or test all of them at once with `Build -> Batch Build -> click Select All -> Click Rebuild`.

In each of the `orbiter\out\build\*\Orbitersdk\XRSound\` folders created by Orbiter's CMake project, `XRSound.lib` should be a `Release` build and `XRSoundD.lib` should be a Debug build. This means there are a total of eight build combinations to test; this linktest project tests them all by creating a DLL compiled and linked with each of the following configurations:


|XRSoundLib-linktest Project Configuration |XRSoundLib-linktest Platform|...tests this generated XRSound lib file|
|----------------------------------------|:----------------------------:|----------------------------------------|
|Debug-with-OrbiterDebug|x86|orbiter\out\build\x86-Debug\Orbitersdk\XRSound\XRSoundD.lib|
|Release-with-OrbiterDebug|x86|orbiter\out\build\x86-Debug\Orbitersdk\XRSound\XRSound.lib| 
|Debug-with-OrbiterRelease|x86|orbiter\out\build\x86-Release\Orbitersdk\XRSound\XRSoundD.lib|
|Release-with-OrbiterRelease|x86|orbiter\out\build\x86-Release\Orbitersdk\XRSound\XRSound.lib|  
|Debug-with-OrbiterDebug|x64|orbiter\out\build\x64-Debug\Orbitersdk\XRSound\XRSoundD.lib|
|Release-with-OrbiterDebug|x64|orbiter\out\build\x64-Debug\Orbitersdk\XRSound\XRSound.lib| 
|Debug-with-OrbiterRelease|x64|orbiter\out\build\x64-Release\Orbitersdk\XRSound\XRSoundD.lib| 
|Release-with-OrbiterRelease|x64|orbiter\out\build\x64-Release\Orbitersdk\XRSound\XRSound.lib|  

Note: it is not strictly necessary for all eight combinations to link successfully, since only the `OrbiterRelease x86` and `OrbiterRelease x64` build artifacts will ever be packaged up and released for end users. Therefore, only those these four build targets need to pass:

* Debug-with-OrbiterRelease x86
* Release-with-OrbiterRelease x86
* Debug-with-OrbiterRelease x64
* Release-with-OrbiterRelease x64

Having the the other four build configurations work as well would just a matter of convenience for developers linking with XRSound as they work in the Orbiter project tree, but it is not required.
