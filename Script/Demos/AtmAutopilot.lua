v1 = vessel.get_interface('GL-01S')

note = oapi.create_annotation()
note:set_pos (0.25,0.1,0.8,0.95)
note:set_colour ({r=0.9,g=0.5,b=0.2})

intro = 'This tutorial gives a brief hands-on demonstration of the Delta-glider\'s atmospheric autopilot.'
note:set_text(intro)

proc.wait_sysdt(10)

intro = intro..'\n\
The autopilot can be programmed via the Script MFD.\
You should see the terminal in the display at the lower left of the window. If it doesn\'t show, terminate the simulation and make sure that the "LuaMFD" module is activated.'
note:set_text(intro)

proc.wait_sysdt(20)

note:set_text('First, we set the target airspeed. Press the MFD button labelled \'INP\' and type the following text into the input box:\n\
aap.spd(300)\n\
(aap.spd is the airspeed autopilot, and 300 is the target speed in m/s)\n\
Press Enter to submit the command.')

while v1:get_airspeed() < 10 do proc.skip() end

note:set_text('Good. The glider now fires its main engines and starts to roll. At a ground speed of 100m/s, pull the stick to rotate.')

while v1:get_altitude() < 30 do proc.skip() end

note:set_text('Excellent. Now raise the gear. Either simply press the "G" key, or enter the following into the MFD:\n\
V:Gear(UP)\n\
(note the capital \'V\'. Terminal input is case-sensitive.)')

proc.wait_sysdt(20)

note:set_text('Now, let\'s set target altitude. Enter the following text in the MFD:\n\
aap.alt(10e3)\n\
(This sets the target altitude to 10km.)\n\
Make sure you don\'t crash into the ground while programming the autopilot!')

while v1:get_altitude() < 9e3 do proc.skip() end

note:set_colour ({r=1,g=0.6,b=0.2})
note:set_text('As we approach the target altitude, the autopilot levels out the climb angle.')

proc.wait_sysdt(20)

note:set_text('You can reset the target speed simply by entering a new value. Enter the following in the MFD:\n\
aap.spd(600)')

while v1:get_airspeed() < 590 do proc.skip() end

note:set_text('Reaching the new target speed. The same mechanism works for resetting the target altitude. Enter this in the MFD:\n\
aap.alt(20e3)')

while v1:get_altitude() < 19e3 do proc.skip() end

note:set_text('The autopilot only controls the main engines. However, if you engage the glider\'s scram engines manually, the main engines will be throttled down automatically to maintain the target speed.\n\
Try it by setting the scram engines with [Alt]-[+] and [Alt]-[-] (The Alt key and the + and - keys on the numeric keypad).')

proc.wait_sysdt(70)

txt = 'You can manually override the autopilot settings.\
Pull the stick to climb to 25km.'
note:set_text(txt)

while v1:get_altitude() < 24.5e3 do proc.skip() end

txt = txt..'\nNow release the stick to allow the autopilot to return to its target altitude.'
note:set_text(txt)

while v1:get_altitude() > 20.5e3 do proc.skip() end

note:set_text('Certain combinations of speed and altitude cannot be sustained by the glider. Try resetting the target speed back to 250 m/s.\
(Turn off scram engines if still engaged)')

while v1:get_airspeed() > 500 do proc.skip() end

note:set_text('The autopilot pitches up in an attempt to maintain the target altitude.')

while v1:get_airspeed() > 310 do proc.skip() end

note:set_text('Eventually, even with fully extended elevator, the glider can\'t maintain an altitude of 20km at a speed of 250m/s.\
It sinks until the denser air produces sufficient lift to balance the gravitational force.')

proc.wait_sysdt(60)

txt = 'This concludes the tutorial. If you want, stay in the air and experiment with the autopilot settings.'
note:set_text(txt)
proc.wait_sysdt(10)

txt = txt..'If you want to find out more details about the atmospheric autopilot, type:\n\
help(dg_aap)\n\
into the MFD. This will bring up a help page.'
note:set_text(txt)
proc.wait_sysdt(20)

oapi.del_annotation(note)
