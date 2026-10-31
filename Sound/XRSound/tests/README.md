# XRSound API test (manual)

`XRSoundApiTest` is an Orbiter plugin that calls the XRSound API the way an add-on does and checks every result. Run it by hand after a change to XRSound. It does not run in the build pipeline: it needs a running Orbiter, real vessels, real simulation frames, and a sound engine. The pipeline only builds it, so a change that breaks the XRSound API breaks the build.

It checks:

- **Volume levels:** setting each volume type, per vessel and per module, reads back the same value. Bad values are refused, and the Main, ATC, and Music defaults match `XRSound.cfg`.
- **Settings:** every setting round-trips, bad values are refused, and the string and numeric forms don't mix. Every setting is also checked for its effect:
  - **Logging:** verbose logging on and off, and animation and thruster logging on and off.
  - **Sound behavior:** silence of space on and off, the update interval, the RCS sound threshold, custom engine thrusters (set, and empty), and the time-acceleration autopilot cutoff.
  - **Music:** music in the cockpit and outside, for each of off, space, and on, in space and in the atmosphere. Music order sequential plays the folder in turn; random never repeats a file and does not follow the sequential order.
  - **Cabin ambience and ATC:** their delays, the ATC rules for landed, in-atmosphere, and reentry flight, and the ATC delay far from the planet.
  - **Gear-up warning:** its altitude, and the landing gear animation.
- **Default sounds and groups:** turning a built-in sound off stops it and turning it on starts it again. A sound group set to a new folder plays files from it, including each vessel's own music folder.
- **Per-sound volume, pan, speed, and position:** this includes values set before a sound starts.
- **Playback types and durations:** each playback type fades by camera distance, view, focus, and air pressure as it should, in space and at partial air pressure. A playback type change reaches a sound that's already playing, without restarting it. The duration checks cover WAV, OGG, MP3, and FLAC files.
- **Isolation:** one vessel's levels, settings, and sound folders never change another vessel's.
- **The actual volume:** each sound plays at its volume times its type's level times the Main level. Level changes reach a sound that's already playing. Each kind of built-in sound (engines, custom engines, RCS, effects, door sounds, callouts, cabin ambience, ATC) answers to its own volume type and no other.
- **Simulation pause:** sounds hold their position while paused, and continue from there on resume.
- **Music:** each vessel can switch music on and off only for itself. The music plays at the levels of the vessel that has focus, and follows focus from one vessel to the other. Only the vessel with focus can change the music's pan, speed, or position, and only while it plays.
- **Older XRSound DLLs:** with an XRSound.dll older than 4.1, every 4.1 call returns false without crashing, and the older calls still work ("legacy" mode).

## What you need

- **An installed Orbiter built from this source, with XRSound:** see "Install the test" below.
- **A sound device:** the test plays 12-second test tones and music. It works with your volume turned down or muted.

## Install the test

The test is built with XRSound, but it is installed only when you ask for it. Configure once with the option on, then build and install as usual:

```
cmake --preset windows-x64-release -DXRSOUND_INSTALL_API_TEST=ON
cmake --build --preset windows-x64-release
cmake --install out/build/windows-x64-release
```

That adds three things to the installed Orbiter folder (`out/install/windows-x64-release/Orbiter`):

- **The plugin:** `Modules/Plugin/XRSoundApiTest.dll`
- **The test tones:** in `XRSound/ApiTest`
- **The scenario:** `Scenarios/Tests/Manual/XRSound API Test.scn`

Run the test from the installed Orbiter. The build folder's Orbiter also has the plugin, because every build makes it, but it has no test tones or scenario, so the test fails there.

## Run it

1. Start `Orbiter.exe` from the installed folder.
2. On the Launchpad's **Modules** tab, turn on **XRSound** and **XRSoundApiTest**.
3. Launch the scenario **Tests > Manual > XRSound API Test**. It has two Delta-gliders in Earth orbit, GL-01 and GL-02, and a third, GL-03, at Cape Canaveral.
4. Leave it alone for about 3 minutes. Don't press keys or change the camera: the test drives everything itself, so that XRSound has a reason to play each sound.
   - **GL-01:** it fires GL-01's main engine and RCS, switches its RCS mode, lowers and raises its gear, and turns its autopilots on and off.
   - **Simulation:** it switches to the cockpit view and back, and runs at 100x time acceleration for a moment. It also pauses the simulation for about 4 seconds.
   - **Focus:** it moves focus between the vessels.
   - **GL-03:** it moves GL-03 between the runway, 5 km up in the air, a reentry-speed flight, a short glide down from 600 m, and 25 km up. GL-03 is held in place while the test waits for ATC calls and checks its sounds, and it ends the run parked on the runway.
   - **Camera:** the music checks need the external view in space, which is how the scenario starts.
   Keep the Orbiter window in front, and don't minimize it. While the simulation is paused, the test only runs while the window is active. If the run stops at the pause, press Ctrl+P to resume: the test then reports the pause checks as failed and carries on.
5. Exit Orbiter.
6. Open `Orbiter.log` in the Orbiter folder, and find the summary line:

```
XRSOUNDTEST SUMMARY mode=full pass=... fail=0 skip=0
```

The run passes when `fail=0` and `skip=0`. Search the log for `XRSOUNDTEST FAIL` to see what failed. Each line names the check and shows the values it got and expected. `XRSOUNDTEST SKIP` lines say why a check could not run.

If the line says `SUMMARY INCOMPLETE`, Orbiter was closed before the test finished. The line shows how far it got.

The test runs once per Orbiter start. To run it again, exit Orbiter completely and start it again.

### Test an older XRSound DLL

To check that add-ons built against XRSound 4.1 still work with an older XRSound:

1. Replace `Modules/Plugin/XRSound.dll` with the older DLL, such as XRSound 4.0 from Orbiter 2024.
2. Run the scenario the same way.
3. Expect `XRSOUNDTEST SUMMARY mode=legacy ... fail=0`. This run takes a few seconds.
4. Put the new `XRSound.dll` back afterward.

## Notes

- **XRSound.log:** the test reads the volumes that XRSound logs to `XRSound.log`, because the API reports only the levels that go into a volume, not the volume itself. For that it turns on XRSound's verbose logging for most of the run, and its animation and thruster logging for a few seconds, so `XRSound.log` grows by a thousand lines or so. (A Debug build of XRSound starts with verbose logging on, whatever `XRSound.cfg` says.)
- **Orbiter's damage simulation:** if it's on, GL-03 can be damaged while the test holds it at reentry speed. That doesn't affect any check.
- **What the test leaves changed:** it changes nothing on disk, but the running session is left changed:
  - The built-in sounds it checked get their usual playback type back, but as an override, because the API has no way to remove one. They sound the same.
  - GL-01's custom engine sound stays set to the test tone.
  - A sound group that had no folder in your configuration keeps the test folder, because the API has no way to remove one. The stock configuration has a folder for every group.
  - The vessels have moved.

  Exit Orbiter after the test.
- **Different levels in XRSound.cfg:** the checks read the default levels from your `XRSound/XRSound.cfg`, so different levels there are fine. The music checks need music to play in the external view in space. That is the default (`MusicPlayExternal = space`).
- **The tones:** each file is the same 12-second, 440 Hz tone, stored as WAV (22.05 kHz, 16-bit mono), OGG, MP3, and FLAC. They were made with ffmpeg:

```
ffmpeg -f lavfi -i "sine=frequency=440:sample_rate=22050:duration=12" -ac 1 -c:a pcm_s16le tone12.wav
ffmpeg -i tone12.wav -c:a libvorbis -q:a 2 tone12.ogg
ffmpeg -i tone12.wav -c:a libmp3lame -b:a 48k tone12.mp3
ffmpeg -i tone12.wav -c:a flac tone12.flac
```

  The MP3 encoder pads the file slightly, to about 12.07 seconds. The duration check allows for that.
- **Music order "random":** the check plays 8 random tracks from the 4 test tones and expects them not to follow the sequential order. Random never repeats the file it played last, so each track follows the sequential order by chance one time in 3, and all 8 do so about once in 2,200 runs. If `setting.effect.MusicOrder.random` fails only on "not all in the sequential order", run the test again.
