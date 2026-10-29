// Copyright (c) 2024-2026 TheGondos
// Licensed under the MIT License

#define INTERPRETER_IMPLEMENTATION

#include "Interpreter.h"
#include "XRSound.h"
#include <vector>

void Interpreter::LoadXRSoundAPI ()
{
	static const struct luaL_reg xrsoundLib[] = {
		{"create_instance", xrsound_create_instance},
		{"is_present", xrsound_is_present},
		{"get_version", xrsound_get_version},
		{"load_wav", xrsound_load_wav},
		{"play_wav", xrsound_play_wav},
		{"stop_wav", xrsound_stop_wav},
		{"is_wavplaying", xrsound_is_wavplaying},
		{"set_paused", xrsound_set_paused},
		{"is_paused", xrsound_is_paused},
		{"set_defaultsoundenabled", xrsound_set_defaultsoundenabled},
		{"get_defaultsoundenabled", xrsound_get_defaultsoundenabled},
		{"set_defaultsoundgroupfolder", xrsound_set_defaultsoundgroupfolder},
		{"get_defaultsoundgroupfolder", xrsound_get_defaultsoundgroupfolder},
		{"set_pan", xrsound_set_pan},
		{"get_pan", xrsound_get_pan},
		{"set_playbackspeed", xrsound_set_playbackspeed},
		{"get_playbackspeed", xrsound_get_playbackspeed},
		{"set_playposition", xrsound_set_playposition},
		{"get_playposition", xrsound_get_playposition},
		{"set_volume", xrsound_set_volume},
		{"get_volume", xrsound_get_volume},
		{"set_soundvolume", xrsound_set_soundvolume},
		{"get_soundvolume", xrsound_get_soundvolume},
		{"set_playbacktype", xrsound_set_playbacktype},
		{"get_playbacktype", xrsound_get_playbacktype},
		{"get_duration", xrsound_get_duration},
		{"set_setting", xrsound_set_setting},
		{"get_setting", xrsound_get_setting},
		{"get_defaultvolume", xrsound_get_defaultvolume},
		{"get_defaultsetting", xrsound_get_defaultsetting},
		{"__gc", xrsound_collect},
		{NULL, NULL}
	};

	luaL_newmetatable (L, "XRSound.vtable");
	lua_pushstring (L, "__index");
	lua_pushvalue (L, -2); // push metatable
	lua_settable (L, -3);  // metatable.__index = metatable
	luaL_openlib (L, NULL, xrsoundLib, 0);

	lua_createtable (L, 0, 7);
	lua_pushnumber (L, (int)XRSound::PlaybackType::InternalOnly); lua_setfield (L, -2, "InternalOnly");
	lua_pushnumber (L, (int)XRSound::PlaybackType::BothViewFar); lua_setfield (L, -2, "BothViewFar");
	lua_pushnumber (L, (int)XRSound::PlaybackType::BothViewMedium); lua_setfield (L, -2, "BothViewMedium");
	lua_pushnumber (L, (int)XRSound::PlaybackType::BothViewClose); lua_setfield (L, -2, "BothViewClose");
	lua_pushnumber (L, (int)XRSound::PlaybackType::Radio); lua_setfield (L, -2, "Radio");
	lua_pushnumber (L, (int)XRSound::PlaybackType::Wind); lua_setfield (L, -2, "Wind");
	lua_pushnumber (L, (int)XRSound::PlaybackType::Global); lua_setfield (L, -2, "Global");
	lua_setglobal (L, "PlaybackType");

	lua_createtable (L, 0, 9);
	lua_pushnumber (L, (int)XRSound::VolumeType::Main); lua_setfield (L, -2, "Main");
	lua_pushnumber (L, (int)XRSound::VolumeType::Engines); lua_setfield (L, -2, "Engines");
	lua_pushnumber (L, (int)XRSound::VolumeType::RCS); lua_setfield (L, -2, "RCS");
	lua_pushnumber (L, (int)XRSound::VolumeType::Effects); lua_setfield (L, -2, "Effects");
	lua_pushnumber (L, (int)XRSound::VolumeType::Callouts); lua_setfield (L, -2, "Callouts");
	lua_pushnumber (L, (int)XRSound::VolumeType::CabinAmbience); lua_setfield (L, -2, "CabinAmbience");
	lua_pushnumber (L, (int)XRSound::VolumeType::ATC); lua_setfield (L, -2, "ATC");
	lua_pushnumber (L, (int)XRSound::VolumeType::Music); lua_setfield (L, -2, "Music");
	lua_pushnumber (L, (int)XRSound::VolumeType::Custom); lua_setfield (L, -2, "Custom");
	lua_setglobal(L, "VolumeType");

	lua_createtable(L, 0, 22);
	lua_pushnumber (L, (int)XRSound::Setting::SilenceOfSpace); lua_setfield (L, -2, "SilenceOfSpace");
	lua_pushnumber (L, (int)XRSound::Setting::MusicOrder); lua_setfield (L, -2, "MusicOrder");
	lua_pushnumber (L, (int)XRSound::Setting::UpdateInterval); lua_setfield (L, -2, "UpdateInterval");
	lua_pushnumber (L, (int)XRSound::Setting::EnableVerboseLogging); lua_setfield (L, -2, "EnableVerboseLogging");
	lua_pushnumber (L, (int)XRSound::Setting::LogVesselAnimations); lua_setfield (L, -2, "LogVesselAnimations");
	lua_pushnumber (L, (int)XRSound::Setting::LogThrusterData); lua_setfield (L, -2, "LogThrusterData");
	lua_pushnumber (L, (int)XRSound::Setting::DisableAutopilotsTimeAccThreshold); lua_setfield (L, -2, "DisableAutopilotsTimeAccThreshold");
	lua_pushnumber (L, (int)XRSound::Setting::MinThrusterLevelForRCSSoundEffects); lua_setfield (L, -2, "MinThrusterLevelForRCSSoundEffects");
	lua_pushnumber (L, (int)XRSound::Setting::ATCMinDelay); lua_setfield (L, -2, "ATCMinDelay");
	lua_pushnumber (L, (int)XRSound::Setting::ATCMaxDelay); lua_setfield (L, -2, "ATCMaxDelay");
	lua_pushnumber (L, (int)XRSound::Setting::ATCAllowWhileLanded); lua_setfield (L, -2, "ATCAllowWhileLanded");
	lua_pushnumber (L, (int)XRSound::Setting::ATCAllowInAtmosphere); lua_setfield (L, -2, "ATCAllowInAtmosphere");
	lua_pushnumber (L, (int)XRSound::Setting::ATCAllowDuringReentry); lua_setfield (L, -2, "ATCAllowDuringReentry");
	lua_pushnumber (L, (int)XRSound::Setting::ATCDelayPlanetDistance); lua_setfield (L, -2, "ATCDelayPlanetDistance");
	lua_pushnumber (L, (int)XRSound::Setting::ATCDelayPlanetMultiplier); lua_setfield (L, -2, "ATCDelayPlanetMultiplier");
	lua_pushnumber (L, (int)XRSound::Setting::CabinAmbienceMin); lua_setfield (L, -2, "CabinAmbienceMin");
	lua_pushnumber (L, (int)XRSound::Setting::CabinAmbienceMax); lua_setfield (L, -2, "CabinAmbienceMax");
	lua_pushnumber (L, (int)XRSound::Setting::MusicPlayInternal); lua_setfield (L, -2, "MusicPlayInternal");
	lua_pushnumber (L, (int)XRSound::Setting::MusicPlayExternal); lua_setfield (L, -2, "MusicPlayExternal");
	lua_pushnumber (L, (int)XRSound::Setting::WarningGearIsUpAltitude); lua_setfield (L, -2, "WarningGearIsUpAltitude");
	lua_pushnumber (L, (int)XRSound::Setting::CustomEnginesThrusterIndexes); lua_setfield (L, -2, "CustomEnginesThrusterIndexes");
	lua_pushnumber (L, (int)XRSound::Setting::LandingGearAnimationID); lua_setfield (L, -2, "LandingGearAnimationID");
	lua_setglobal(L, "Setting");

	lua_createtable(L, 0, 2);
	lua_pushnumber (L, (int)XRSound::MusicOrder::Sequential); lua_setfield (L, -2, "Sequential");
	lua_pushnumber (L, (int)XRSound::MusicOrder::Random); lua_setfield (L, -2, "Random");
	lua_setglobal(L, "MusicOrder");

	lua_createtable(L, 0, 3);
	lua_pushnumber (L, (int)XRSound::MusicPlay::Off); lua_setfield (L, -2, "Off");
	lua_pushnumber (L, (int)XRSound::MusicPlay::Space); lua_setfield (L, -2, "Space");
	lua_pushnumber (L, (int)XRSound::MusicPlay::On); lua_setfield (L, -2, "On");
	lua_setglobal(L, "MusicPlay");

	lua_createtable (L, 0, 57);
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AirConditioning); lua_setfield (L, -2, "AirConditioning");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::LandedWind); lua_setfield (L, -2, "LandedWind");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AudioGreeting); lua_setfield (L, -2, "AudioGreeting");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::MainEngines); lua_setfield (L, -2, "MainEngines");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RetroEngines); lua_setfield (L, -2, "RetroEngines");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::HoverEngines); lua_setfield (L, -2, "HoverEngines");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSSustain); lua_setfield (L, -2, "RCSSustain");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::SwitchOn); lua_setfield (L, -2, "SwitchOn");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::SwitchOff); lua_setfield (L, -2, "SwitchOff");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Rotation); lua_setfield (L, -2, "Rotation");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Translation); lua_setfield (L, -2, "Translation");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Off); lua_setfield (L, -2, "Off");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::CustomEngines); lua_setfield (L, -2, "CustomEngines");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AFPitch); lua_setfield (L, -2, "AFPitch");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AFOn); lua_setfield (L, -2, "AFOn");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AFOff); lua_setfield (L, -2, "AFOff");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Crash); lua_setfield (L, -2, "Crash");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::MetalCrunch); lua_setfield (L, -2, "MetalCrunch");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::WheelChirp); lua_setfield (L, -2, "WheelChirp");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Touchdown); lua_setfield (L, -2, "Touchdown");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::WheelStop); lua_setfield (L, -2, "WheelStop");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::TiresRolling); lua_setfield (L, -2, "TiresRolling");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::OneHundredKnots); lua_setfield (L, -2, "OneHundredKnots");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Liftoff); lua_setfield (L, -2, "Liftoff");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::WarningGearIsUp); lua_setfield (L, -2, "WarningGearIsUp");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::YouAreClearedToLand); lua_setfield (L, -2, "YouAreClearedToLand");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Docking); lua_setfield (L, -2, "Docking");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::DockingCallout); lua_setfield (L, -2, "DockingCallout");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Undocking); lua_setfield (L, -2, "Undocking");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::UndockingCallout); lua_setfield (L, -2, "UndockingCallout");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::Wheekbrakes); lua_setfield (L, -2, "Wheekbrakes");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::DockingRadarBeep); lua_setfield (L, -2, "DockingRadarBeep");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::FlightWind); lua_setfield (L, -2, "FlightWind");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::ReentryPlasma); lua_setfield (L, -2, "ReentryPlasma");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::SonicBoom); lua_setfield (L, -2, "SonicBoom");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AutopilotOn); lua_setfield (L, -2, "AutopilotOn");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AutopilotOff); lua_setfield (L, -2, "AutopilotOff");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::SubsonicCallout); lua_setfield (L, -2, "SubsonicCallout");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSAttackPlusX); lua_setfield (L, -2, "RCSAttackPlusX");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSAttackPlusY); lua_setfield (L, -2, "RCSAttackPlusY");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSAttackPlusZ); lua_setfield (L, -2, "RCSAttackPlusZ");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSAttackMinusX); lua_setfield (L, -2, "RCSAttackMinusX");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSAttackMinusY); lua_setfield (L, -2, "RCSAttackMinusY");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSAttackMinusZ); lua_setfield (L, -2, "RCSAttackMinusZ");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSClosePlusX); lua_setfield (L, -2, "RCSClosePlusX");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSClosePlusY); lua_setfield (L, -2, "RCSClosePlusY");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSClosePlusZ); lua_setfield (L, -2, "RCSClosePlusZ");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSCloseMinusX); lua_setfield (L, -2, "RCSCloseMinusX");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSCloseMinusY); lua_setfield (L, -2, "RCSCloseMinusY");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RCSCloseMinusZ); lua_setfield (L, -2, "RCSCloseMinusZ");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::LastDefaultSound); lua_setfield (L, -2, "LastDefaultSound");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::RadioATCGroup); lua_setfield (L, -2, "RadioATCGroup");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::CabinAmbienceGroup); lua_setfield (L, -2, "CabinAmbienceGroup");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::MachCalloutsGroup); lua_setfield (L, -2, "MachCalloutsGroup");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::AltitudeCalloutsGroup); lua_setfield (L, -2, "AltitudeCalloutsGroup");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::DockingDistanceCalloutsGroup); lua_setfield (L, -2, "DockingDistanceCalloutsGroup");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::MusicFolder); lua_setfield (L, -2, "MusicFolder");
	lua_pushnumber (L, (int)XRSound::DefaultSoundID::LastDefaultGroup); lua_setfield (L, -2, "LastDefaultGroup");
	lua_setglobal (L, "DefaultSoundID");
}

int Interpreter::lua_isxrsound(lua_State *L, int idx)
{
	return luaL_tryudata(L, idx, "XRSound.vtable") != NULL;
}

XRSound *Interpreter::lua_toxrsound (lua_State *L, int idx)
{
	ASSERT_SYNTAX(lua_isxrsound(L, idx), "Argument is not an xrsound object");
	XRSound **pxrsound = (XRSound**)lua_touserdata(L,idx);
	return *pxrsound;
}

/***
XRSound interface functions
@module xrsound
*/
/// @lookup types

/***
Create an instance of an XRSound proxy object for the supplied vessel; invoke this from your vessel's clbk_postcreation method.
Normally you will only invoke this method from your own vessel's code; however, if you call this method from a *module*, 
you will be able to use the returned proxy object to control the sounds for the supplied vessel.

@function create_instance
@tparam handle vessel handle
@treturn xrsound XRSound proxy to use with the specified vessel
@usage
function clbk_postcreation()
	snd = xrsound.create_instance(vi)
end
*/
#ifndef XRSOUND
int Interpreter::xrsound_create_instance(lua_State *L)
{
	// XRSound module not build. Can't call CreateInstance()
	return luaL_error(L, "Application not build with XRSound support");
}

#else
int Interpreter::xrsound_create_instance (lua_State *L)
{
	XRSound *snd = NULL;
	if(lua_isstring(L, 1)) {
		const char *moduleName = lua_tostring(L, 1);
		snd = XRSound::CreateInstance(moduleName);
	} else if (VESSEL *v = lua_tovessel(L, 1)) {
		snd = XRSound::CreateInstance(v);
	} else {
		return luaL_error(L, "Invalid parameter to xrsound.create_instance, string or vessel handle needed");
	}

	XRSound **proxy = (XRSound **)lua_newuserdata(L, sizeof(XRSound *));
	*proxy = snd;
	luaL_getmetatable(L, "XRSound.vtable");
	lua_setmetatable(L, -2);

	return 1;
}
#endif // !XRSOUND

/***
xrsound class: Lua access to XRSound objects
@classmod xrsound

XRSound 4.1 volume and setting calls read or change only the proxy for this object. They never change the configuration defaults or another vessel's or module's values. A getter returns nil when the corresponding C++ API call fails, including when the loaded XRSound.dll does not support 4.1.
*/

/***
XRSound status.

Returns true if XRSound.dll is present, false if not.

@function is_present
@treturn boolean XRSound status
*/
int Interpreter::xrsound_is_present(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	lua_pushboolean(L, snd->IsPresent());
	return 1;
}

/***
XRSound version.

Returns the version of XRSound.dll, or 0 if DLL not present.

@function get_version
@treturn number XRSound version
*/
int Interpreter::xrsound_get_version(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	lua_pushnumber(L, snd->GetVersion());
	return 1;
}

/***
Load wav file.

Loads the specified sound file and assigns the supplied sound ID to it.  Note: it does not need to be a .wav file: it may be any supported sound file format
supported by XRSound.  Note that this call is lightweight and does not actually load the sound file data into memory: 
the sound data is not loaded until it is needed by PlayWav later.
If you load a different sound file with the same ID as an existing loaded sound file, the previous sound file is
automatically stopped if it is playing and replaced by this sound.

Note: for module sounds, you should pass PlaybackType.Global for this since other values have no effect when playing sounds from an Orbiter module (as opposed to an Orbiter vessel).

@function load_wav
@tparam number soundID sound ID to be assigned to this sound file (vessel-instance unique)
@tparam string filename path relatve to $ORBITER_ROOT of sound file to load
@tparam number playbackType denotes how sound will be faded. See @{types.PlaybackType|PlaybackType}
@treturn boolean true on success, false if file not found, playbackType is not a valid PlaybackType, or XRSound.dll not present.
@usage
function clbk_postcreation()
	snd = xrsound.create_instance(vi)
	snd:load_wav(1, "XRSound/Default/On.wav", PlaybackType.InternalOnly)
	snd:load_wav(2, "XRSound/Default/Off.wav", PlaybackType.InternalOnly)
end
*/
int Interpreter::xrsound_load_wav(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	const char *filename = luaL_checkstring(L, 3);
	int type = luaL_checkinteger(L, 4);
	if ((type < (int)XRSound::PlaybackType::InternalOnly) || (type > (int)XRSound::PlaybackType::Global)) {
		// an out-of-range type would be loaded and later trip an assert in XRSound; report failure like any other failed load
		lua_pushboolean(L, false);
		return 1;
	}
	lua_pushboolean(L, snd->LoadWav(ID, filename, (XRSound::PlaybackType)type));
	return 1;
}

/***
Play wav file.

Play the sound file with the specified ID. If the sound is already playing, this call will only alter its loop or volume settings.

When volume is omitted or nil, XRSound uses the value stored for this sound ID. A newly loaded sound starts at 1.0, and an explicit volume supplied to this call becomes the stored value for later calls that omit it. XRSound.dll versions older than 4.1 use 1.0 for the omitted-volume default.

@function play_wav
@tparam number soundID vessel-instance-unique sound ID originally passed to LoadWav
@tparam[opt=false] boolean loop true to loop sound continuously until stop_wav called, false to play only once
@tparam[opt=XRSound::UseStoredVolume] number volume 0 (muted) - 1.0 (loudest); omitted or nil uses the volume stored for this sound ID. It starts at 1.0 and each explicit PlayWav volume is stored for later calls without a volume. Older XRSound.dll versions use 1.0 for this default.
@treturn boolean true on success, false if invalid sound ID or if XRSound.dll not present.
@usage
function clbk_consumebufferedkey(key, down, kstate)
    if oapi.keydown(kstate, OAPI_KEY.A) then
		snd:play_wav(1)
    end
    if oapi.keydown(kstate, OAPI_KEY.B) then
		snd:play_wav(2)
    end
end
*/
int Interpreter::xrsound_play_wav(lua_State *L)
{
	bool loop = false;
	float volume = XRSound::UseStoredVolume;

	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	if(lua_gettop(L)>=3)
		loop = lua_toboolean(L, 3);
	if(lua_gettop(L)>=4 && !lua_isnil(L, 4))
		volume = luaL_checknumber(L, 4);

	lua_pushboolean(L, snd->PlayWav(ID, loop, volume));
	return 1;
}

/***
Stop wav file.

Stop playing the sound file with the specified ID.

@function stop_wav
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@treturn boolean true on success, false if invalid sound ID or if XRSound.dll not present.
*/
int Interpreter::xrsound_stop_wav(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushboolean(L, snd->StopWav(ID));
	return 1;
}

/***
Sound status.

Returns the status of a sound.

@function is_wavplaying
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@treturn boolean false if the specified sound is not playing or XRSound.dll is not present.
*/
int Interpreter::xrsound_is_wavplaying(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushboolean(L, snd->IsWavPlaying(ID));
	return 1;
}

/***
Pause/unpause.

Pause or unpause a sound.

@function set_paused
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@tparam boolean pause true to pause sound, false to unpause it
@treturn boolean true on success, false if invalid sound ID or XRSound.dll not present.
*/
int Interpreter::xrsound_set_paused(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	bool paused = lua_toboolean(L, 3);
	lua_pushboolean(L, snd->SetPaused(ID, paused));
	return 1;
}

/***
Pause status.

Detect if a sound is paused.

@function is_paused
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@treturn boolean true if sound is paused, or false if not paused, invalid sound ID, or XRSound.dll not present.
*/
int Interpreter::xrsound_is_paused(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushboolean(L, snd->IsPaused(ID));
	return 1;
}

/***
Default sounds.

Enable or disable a default sound or group.  This can also be done globally via the configuration file; however, this option 
is useful if you want to disable some sound effects at runtime for a specific vessel class.

Not supported for module sounds.

For MusicFolder, this changes only this vessel's music enabled flag. The track is shared and played by whichever vessel has focus; disabling MusicFolder here never affects another vessel.

@function set_defaultsoundenabled
@tparam number which default sound to enable or disable. See @{types.DefaultSoundID|DefaultSoundID}
@tparam boolean enabled true to enable default sound, false to disable it
@treturn boolean true on success, false if XRSound.dll is unavailable or soundID is not a default sound or group. XRSound 4.1 and newer also return true when disabled by configuration.
*/
int Interpreter::xrsound_set_defaultsoundenabled(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	bool enabled = lua_toboolean(L, 3);
	lua_pushboolean(L, snd->SetDefaultSoundEnabled((XRSound::DefaultSoundID)ID, enabled));
	return 1;
}

/***
Default sound status.

Get the status of a default sound or group.

Not supported for module sounds.

@function get_defaultsoundenabled
@tparam number ID which default sound ID to check. See @{types.DefaultSoundID|DefaultSoundID}
@treturn boolean true if the specified default sound or group is enabled, false if sound or group is disabled or XRSound.dll not present.
*/
int Interpreter::xrsound_get_defaultsoundenabled(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushboolean(L, snd->GetDefaultSoundEnabled((XRSound::DefaultSoundID)ID));
	return 1;
}

/***
Set sound folder.

Set the default subfolder path for a default sound group, relative to $ORBITER_ROOT.

Not supported for module sounds.

@function set_defaultsoundgroupfolder
@tparam number ID which default XRSound group to update (only @{types.DefaultSoundID|DefaultSoundID}s that end in "Group" are valid for this call).
@tparam string subfolderpath subfolder path relative to $ORBITER_ROOT
@treturn boolean true on success, false if defaultSoundID is not a valid default group sound ID, no default sounds loaded for the supplied defaultSoundID, or XRSound.dll not present.
*/
int Interpreter::xrsound_set_defaultsoundgroupfolder(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	const char *path = luaL_checkstring(L, 3);
	lua_pushboolean(L, snd->SetDefaultSoundGroupFolder((XRSound::DefaultSoundID)ID, path));
	return 1;
}

/***
Get sound folder.

Returns the default subfolder path for a default sound group, relative to $ORBITER_ROOT.

Not supported for module sounds.

@function get_defaultsoundgroupfolder
@tparam number ID which default XRSound group to query (only @{types.DefaultSoundID|DefaultSoundID}s that end in "Group" are valid for this call).
@treturn string|nil default subfolder path for a default sound group, or nil if no default sounds loaded for the supplied defaultSoundID or XRSoundDLL not present.
*/
int Interpreter::xrsound_get_defaultsoundgroupfolder(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);

	const char *path = snd->GetDefaultSoundGroupFolder((XRSound::DefaultSoundID)ID);
	if(path)
		lua_pushstring(L, path);
	else
		lua_pushnil(L);
	return 1;
}

/***
Pan configuration.

Sets the pan (left/right balance) of the sound with the specified  ID.

XRSound 4.1 and newer remembers pan set on a loaded sound that is not playing and applies it once, the next time it starts. Older DLL versions require playback to have started. A remembered value cannot be read with get_pan until the sound is playing. For MusicFolder, this works only for the focused vessel while music is playing; it is not remembered for a later track.

@function set_pan
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@tparam number pan range is from -1.0 (full left) to 1.0 (full right). Zero is the center.
@treturn boolean true on success, false if the sound ID or pan is invalid, or if the sound is not playing on older XRSound versions.
*/
int Interpreter::xrsound_set_pan(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	float pan = luaL_checknumber(L, 3);
	lua_pushboolean(L, snd->SetPan(ID, pan));
	return 1;
}

/***
Pan status.

Returns the pan of the sound with the specified ID, from -1.0 (full left) to 1.0 (full right). Zero is the center.

The sound must already be playing; a pan remembered for its next start is not readable. For MusicFolder, the focused vessel's currently playing track is required.

@function get_pan
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@treturn number range is from -1.0 (full left) to 1.0 (full right). Zero is the center. Returns -100 if sound ID is invalid or is not currently playing.
*/
int Interpreter::xrsound_get_pan(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushnumber(L, snd->GetPan(ID));
	return 1;
}

/***
Speed configuration.

Sets the playback speed of the sound with the specified ID.

XRSound 4.1 and newer remembers speed set on a loaded sound that is not playing and applies it once, the next time it starts. Older DLL versions require playback to have started. A remembered value cannot be read with get_playbackspeed until the sound is playing. For MusicFolder, this works only for the focused vessel while music is playing; it is not remembered for a later track.

Plays the sound at a higher or lower speed, increasing or decreasing its frequency, which makes it sound lower or higher.

@function set_playbackspeed
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@tparam[opt=1.0] number speed factor of the speed increase or decrease; 2 is twice as fast, 0.5 is only half as fast. The default is 1.0.
@treturn boolean true on success, false if the sound ID or speed is invalid, or if the sound is not playing on older XRSound versions.
*/
int Interpreter::xrsound_set_playbackspeed(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	float speed = 1.0f;
	if(lua_gettop(L)>=3)
		speed = luaL_checknumber(L, 3);

	lua_pushboolean(L, snd->SetPlaybackSpeed(ID, speed));
	return 1;
}

/***
Speed status.

Returns the playback speed of the sound with the specified ID.

The sound must already be playing; a speed remembered for its next start is not readable.

@function get_playbackspeed
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@treturn number factor of the speed increase or decrease; 2 is twice as fast, 0.5 is only half as fast. The default is 1.0. Returns 0 if sound ID is invalid or is not currently playing.
*/
int Interpreter::xrsound_get_playbackspeed(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushnumber(L, snd->GetPlaybackSpeed(ID));
	return 1;
}

/***
Play position configuration.

Sets the playback position of the sound with the specified ID.

XRSound 4.1 and newer remembers a position set on a loaded sound that is not playing and seeks once when the sound next starts. Older DLL versions require playback to have started. A remembered value cannot be read with get_playposition until the sound is playing. For MusicFolder, this works only for the focused vessel while music is playing; it is not remembered for a later track.

@function set_playposition
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@tparam number positionMillis  must be between zero (the start of the file) and the length of the file, in milliseconds
@treturn boolean true on success, false if the position or sound ID is invalid, or if the sound is not playing on older XRSound versions.
*/
int Interpreter::xrsound_set_playposition(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	int position = luaL_checkinteger(L, 3);

	lua_pushboolean(L, snd->SetPlayPosition(ID, position));
	return 1;
}

/***
Play position status.

Returns the playback position of the sound in milliseconds with the specified ID.

The sound must already be playing; a position remembered for its next start is not readable.

@function get_playposition
@tparam number soundID unique sound ID originally passed to LoadWav and PlayWav
@treturn number between zero (the start of the file) and the length of the file, in milliseconds. Returns < 0 if sound ID is invalid or is not currently playing.
*/
int Interpreter::xrsound_get_playposition(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	lua_pushnumber(L, snd->GetPlayPosition(ID));
	return 1;
}

/***
Set one of this proxy's active volume levels.
@function set_volume
@tparam number volumeType See @{types.VolumeType|VolumeType}.
@tparam number level Absolute level from 0 to 1.
@treturn boolean true on success; false if the level or type is invalid, or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_set_volume(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int type = luaL_checkinteger(L, 2);
	float level = (float)luaL_checknumber(L, 3);
	lua_pushboolean(L, snd->SetVolume((XRSound::VolumeType)type, level));
	return 1;
}

/***
Read one of this proxy's active volume levels: its configured starting level or most recent value set in this session.
@function get_volume
@tparam number volumeType See @{types.VolumeType|VolumeType}.
@treturn number|nil Current level, or nil if the type is invalid or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_volume(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int type = luaL_checkinteger(L, 2);
	float value;
	if (snd->GetVolume((XRSound::VolumeType)type, value))
		lua_pushnumber(L, value);
	else
		lua_pushnil(L);
	return 1;
}

/***
Set the volume of one loaded, default, group, or animation sound. For a client-loaded sound this is its stored playback volume, initially 1.0 and reset to 1.0 when a different file is loaded for that ID. For default, group, or animation sounds it is an item-level multiplier on XRSound's automatic volume, retained if the sound file is replaced. Both take effect immediately.
@function set_soundvolume
@tparam number soundID
@tparam number volume Level from 0 to 1.
@treturn boolean true on success; false if the value or ID is invalid, or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_set_soundvolume(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	float value = (float)luaL_checknumber(L, 3);
	lua_pushboolean(L, snd->SetVolume(ID, value));
	return 1;
}

/***
Read the stored volume for a client-loaded sound or the item-level multiplier for a default, group, or animation sound.
@function get_soundvolume
@tparam number soundID
@treturn number|nil Volume, or nil if the ID is unsupported or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_soundvolume(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	float value;
	if (snd->GetVolume(ID, value))
		lua_pushnumber(L, value);
	else
		lua_pushnil(L);
	return 1;
}

/***
Override a sound's playback type. This takes effect immediately and can be set before a default or animation sound is loaded; the override is retained when XRSound reloads its file. A later load_wav of another sound replaces it. Module proxies store the type, but module sounds always play as Global.
@function set_playbacktype
@tparam number soundID
@tparam number playbackType See @{types.PlaybackType|PlaybackType}.
@treturn boolean true on success; false if the type or ID is invalid, or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_set_playbacktype(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	int type = luaL_checkinteger(L, 3);
	lua_pushboolean(L, snd->SetPlaybackType(ID, (XRSound::PlaybackType)type));
	return 1;
}

/***
Read a sound's effective playback type: its override, if set, or the most recently supplied load_wav type.
@function get_playbacktype
@tparam number soundID
@treturn number|nil Playback type, or nil if the ID is unsupported or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_playbacktype(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	XRSound::PlaybackType type;
	if (snd->GetPlaybackType(ID, type))
		lua_pushnumber(L, (int)type);
	else
		lua_pushnil(L);
	return 1;
}

/***
Measure a loaded sound's duration without playing it. The first call measures the file and caches the result until a different file is loaded for that ID.
@function get_duration
@tparam number soundID
@treturn number|nil Duration in milliseconds, or nil if the file cannot be measured, the ID is unsupported, or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_duration(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int ID = luaL_checkinteger(L, 2);
	unsigned int duration;
	if (snd->GetDuration(ID, duration))
		lua_pushnumber(L, duration);
	else
		lua_pushnil(L);
	return 1;
}

/***
Set a setting on this proxy without changing configuration defaults or another proxy's value. Strings select the string overload; booleans become 0 or 1 and numbers select the numeric overload. Getters return boolean settings as numbers.
@function set_setting
@tparam number setting See @{types.Setting|Setting}.
@param value Number or boolean for numeric settings; string for string settings.
@treturn boolean true on success; false if the setting type or value is invalid, or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_set_setting(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int setting = luaL_checkinteger(L, 2);
	bool result;
	if (lua_type(L, 3) == LUA_TSTRING)
		result = snd->SetSetting((XRSound::Setting)setting, luaL_checkstring(L, 3));
	else if (lua_type(L, 3) == LUA_TBOOLEAN)
		result = snd->SetSetting((XRSound::Setting)setting, lua_toboolean(L, 3) ? 1.0 : 0.0);
	else
		result = snd->SetSetting((XRSound::Setting)setting, luaL_checknumber(L, 3));
	lua_pushboolean(L, result);
	return 1;
}

// Try the numeric setting form first, then bounded string buffers; the bool result cannot distinguish a too-small buffer from other string failures.
static void xrsound_push_setting(lua_State *L, XRSound *snd, XRSound::Setting setting, bool isDefault)
{
	double number;
	if (isDefault ? snd->GetDefaultSetting(setting, number) : snd->GetSetting(setting, number)) {
		lua_pushnumber(L, number);
		return;
	}
	// The API reports buffer-too-small and other string failures identically. Include room for the terminator and stop at 64 KB.
	for (int size = 256; size <= 65536; size *= 2) {
		std::vector<char> buffer(size);
		bool ok = isDefault ? snd->GetDefaultSetting(setting, buffer.data(), size) : snd->GetSetting(setting, buffer.data(), size);
		if (ok) {
			lua_pushstring(L, buffer.data());
			return;
		}
	}
	lua_pushnil(L);
}

/***
Read a setting from this proxy. Numeric and boolean settings return numbers (booleans are 0 or 1); string settings return strings. This reads the active value, not its configured default.
@function get_setting
@tparam number setting See @{types.Setting|Setting}.
@treturn number|string|nil Value, or nil if unsupported or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_setting(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int setting = luaL_checkinteger(L, 2);
	xrsound_push_setting(L, snd, (XRSound::Setting)setting, false);
	return 1;
}

/***
Read a configured starting volume level. This value is read-only and does not change when the active level changes.
@function get_defaultvolume
@tparam number volumeType See @{types.VolumeType|VolumeType}.
@treturn number|nil Configured level, or nil if the type is invalid or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_defaultvolume(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int type = luaL_checkinteger(L, 2);
	float value;
	if (snd->GetDefaultVolume((XRSound::VolumeType)type, value))
		lua_pushnumber(L, value);
	else
		lua_pushnil(L);
	return 1;
}

/***
Read a configured starting setting. Numeric and boolean settings return numbers (booleans are 0 or 1); string settings return strings. This value is read-only and does not change when the active setting changes.
@function get_defaultsetting
@tparam number setting See @{types.Setting|Setting}.
@treturn number|string|nil Configured value, or nil if unsupported or XRSound 4.1 is unavailable.
*/
int Interpreter::xrsound_get_defaultsetting(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	int setting = luaL_checkinteger(L, 2);
	xrsound_push_setting(L, snd, (XRSound::Setting)setting, true);
	return 1;
}

// Clean up if handle is garbage collected
int Interpreter::xrsound_collect(lua_State *L)
{
	XRSound *snd = lua_toxrsound(L, 1);
	delete snd;
	return 0;
}


