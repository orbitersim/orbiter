# Lua Script Interface for Orbiter

@lookup intro.md

Orbiter contains a script interpreter module which allows to control a variety of simulation tasks with the help of scripts. Script applications include autopilots, MFD control, interactive tutorials, mission control, and many others.

The Orbiter script engine uses the Lua scripting language ([www.lua.org](http://www.lua.org)). A large number of functions and methods have been added to the standard Lua command set to provide an interface to the Orbiter simulation environment. To a large extent the Lua-Orbiter interface replicates the Orbiter C++ API interface.

This manual describes the Orbiter extensions to Lua. It is not a general introduction to Lua. To learn how to write Lua scripts, please consult the documentation and resources available on the Lua website.

## Invoking the interpreter

There are currently two ways to access an interpreter instance: using a system console window, and opening a Lua input MFD. In addition, a script can be processed automatically when running a scenario.

Addon developers can create interpreter instances in their modules using API functions, and then send commands or scripts to the interpreter for processing.

### Console window

Activate the LuaConsole plugin in the Launchpad modules tab. In the running simulation, open the Custom functions list (Ctrl-F4) and activate the Lua console window function.

You can now start typing commands into the console window. User input is displayed in black, script output is displayed in green. The console window can be resized.

Some parameters defining the console appearance and behaviour can be adjusted from the Console Configuration entry in the Extra tab of the Launchpad window.

### Lua script MFD

To enable the Lua MFD mode, activate the LuaMFD plugin in the Launchpad modules tab. This will add the Script MFD mode on all MFD mode selection lists. Opening this mode will bring up the interpreter console in the MFD display. To enter a script command, click the INP button and type the command.

The MFD interpreter works identically to the console window, except that it displays fewer characters per line.

The script MFD is generic, i.e. is not specific to the vessel it is running in. One exception is the fact that each MFD interpreter predefines a global variable "V" as vessel instance. Therefore, 

	name=V:get_name() 

is equivalent to

	v=vessel.get_focusinterface()
	name=v:get_name() 

The script MFD can run more than one interpreter. Clicking the "New" button will open a new console page and associate a new interpreter with it. You can run separate scripts in different pages simultaneously (but obviously they should not conflict with each other). Use the "<PG" and "PG>" buttons to switch between pages. "Del" will delete the current page and kill the corresponding interpreter.

The interpreters are shared between all the MFDs of a given vessel. That is, opening Script mode on multiple MFDs will display the same data in all of them on corresponding pages. Opening a new page in one MFD will insert a new page in all other MFDs, and deleting a page will delete the same page in all other MFDs.

The interpreters remain active when the MFD is closed. This means that a script launched in the MFD will continue to run even if the MFD is switched to a different mode.

### Automatic script processing

It is possible to associate a Lua script with a scenario so that it is invoked automatically (using Orbiter's built-in script interpreter) when the scenario is launched. No additional modules need to be activated for this.

To associate a script with a scenario, add the line 

	Script <name> 

to the _Environment_ block of the scenario file. Here, `<name>` is the name of the Lua script file (including any applicable path relative to the _Script_ subdirectory, and excluding (but assuming) the file extension .lua).

For automatically executed scripts, no console support is provided. Any `term.*` function calls in the script are ignored. User I/O, if required, must be implemented by alternative means (for example by using the screen annotation methods for output, and `proc.WaitInput()` for input.

The _Script/Challenges/Challenge1.lua_ script is an example for an automatically invoked script. It is used by the _2010 Edition/Challenges/Challenge 1_ scenario.

### Interactive usage examples

This section covers both the console window and script MFD.

For a general introduction to the syntax of the Lua scripting language, please see the documentation on the Lua website.

To load and execute a script, use the run command: 

	run(script) 

where _script_ is the script file name (with optional path). The script file extension '.lua' is assumed and should not be included in the call parameter. The default script path is the Script subdirectory below the Orbiter main directory. All paths specified in the call to run are relative to this directory. Therefore, 

	run('myscripts/demo')
	
will load the script Script/myscripts/demo.lua.

You can also enter Lua commands directly, but please note that direct input commands cannot span multiple lines. Flow constructs such as loops must be submitted in a single line.

Examples: 

	term.out(oapi.get_simtime())

displays the current simulation time. 

	n=oapi.create_annotation()
	n:set_text('Hello, world!') 

displays the text as an onscreen annotation.

	v = vessel.get_focusinterface()
	term.out(v:get_name())

displays the name of the current focus vessel. 

	n=vessel.get_count()
	for i=0,n-1 do v=vessel.get_interface(i); term.out(v:get_name()) end 

lists the vessels in the current simulation.

As a script example, a simple ascent autopilot script for the space shuttle has been included. To try it, launch the _"2010 Edition/Scripts/Space shuttle launch"_ scenario. Open either the console window, or go to an interior view and open the Script MFD in one of the MFD displays. Enter the following commands: 

	run('atlantis/launch')
	launch() 

This will initiate an automated launch and orbit insertion. You can modify the launch parameters by resetting some of the script parameters after loading the script. For example,

	run('atlantis/launch')
	orbit_alt = 400e3
	azimuth = 135*RAD
	launch() 

will set the target orbit altitude to 400km and the launch azimuth angle to 135 degrees. 
Have a look at this script (located in Script/Atlantis/launch.lua) to see how it works. Note that this is a rather quick and dirty example, intended to show the current capabilities and concepts of the interpreter. Have a go and try to improve or extend it!
