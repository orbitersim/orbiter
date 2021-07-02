v=vessel.get_interface('SH-15')

adicnt = {x=817,y=113,z=0}

adiball_area = {{x=733,y=29,z=0},{x=901,y=29,z=0},{x=733,y=197,z=0},{x=901,y=197,z=0}}

adictrl_ref = {x=732,y=200,z=0}
adictrl_area = {adictrl_ref,vec.add(adictrl_ref,{x=169,y=0,z=0}),vec.add(adictrl_ref,{x=0,y=179,z=0}),vec.add(adictrl_ref,{x=169,y=179,z=0})}
-- adictrl_area = {{x=715,y=200,z=0},{x=884,y=200,z=0},{x=715,y=379,z=0},{x=884,y=379,z=0}}

proc.wait_simdt(1)

note = oapi.create_annotation()
note:set_pos (0.2,0.05,0.8,0.95);
note:set_colour ({r=0.7,g=0.8,b=1})
note:set_text("This training unit introduces the Attitude Director Indicator instrument for the Shuttle-A. It runs for about 15 minutes. You can fast-forward through any sections with the 'T' and 'R' keys.")


proc.wait_simdt (10)

note:set_pos (0.4,0.1,0.8,0.95);
note:set_colour ({r=1,g=0.7,b=0})
note:set_text('The new 2-D panel of the Shuttle-A transport vessel has been equipped with a versatile new instrument: the Attitude Director Indicator (or ADI for short).')

proc.wait_simdt(5)

txt = 'The ADI consists of a ball that represents the orientation of an attitude reference frame with respect to the current vessel orientation.'
note:set_text(txt)
oapi.set_panelblink(adiball_area[1],adiball_area[2],adiball_area[3],adiball_area[4])

proc.wait_simdt(8)

txt = "\nWhen the vessel rotates, the ball remains aligned with the reference frame."
note:set_text(txt)
v:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT,1)
v:set_thrustergrouplevel(THGROUP.ATT_PITCHUP,1)
oapi.set_panelblink()

proc.wait_simdt(5)

v:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT,0)
v:set_thrustergrouplevel(THGROUP.ATT_PITCHUP,0)

proc.wait_simdt(5)

v:set_navmode(NAVMODE.KILLROT)

proc.wait_simdt(5)

txt = "The markings on the ball represent a grid of pitch and yaw lines ..."
note:set_text(txt);

proc.wait_simdt(10)
txt = txt.."\n\n... and the current vessel pitch and yaw in the reference frame can be read off from the center marker."
note:set_text(txt);
mx = adicnt.x
my = adicnt.y
oapi.set_panelblink({x=mx-15,y=my-15,z=0},{x=mx+15,y=my-15,z=0},{x=mx-15,y=my+15,z=0},{x=mx+15,y=my+15,z=0})

proc.wait_simdt(10)
txt = "The ring around the ball contains a scale for the roll angle, and the vessel's current roll angle is indicated with a red marker."
note:set_text(txt)

b=v:get_bank()
rad=54
mx = adicnt.x + math.sin(b)*rad
my = adicnt.y - math.cos(b)*rad
oapi.set_panelblink({x=mx-15,y=my-15,z=0},{x=mx+15,y=my-15,z=0},{x=mx-15,y=my+15,z=0},{x=mx+15,y=my+15,z=0})

proc.wait_simdt(10)

txt = "The spacecraft's current roll, pitch and yaw angles with respect to the reference frame are also shown on the 'DEV' line in the digital display below the ADI."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=8,y=27,z=0}),vec.add(adictrl_ref,{x=162,y=27,z=0}),vec.add(adictrl_ref,{x=8,y=38,z=0}),vec.add(adictrl_ref,{x=162,y=38,z=0}))

proc.wait_simdt(10)

txt = "You have a choice of attitude reference frames to select from. The most suitable frame depends on the particular maneuver or flight phase you are currently executing."
note:set_text(txt)
oapi.set_panelblink()

proc.wait_simdt(10)

txt = "The controls for selecting the reference frame, as well as a number of other functions relating to the attitude reference and ADI operation, are located in the panel below the ADI."
note:set_text(txt)
oapi.set_panelblink(adictrl_area[1],adictrl_area[2],adictrl_area[3],adictrl_area[4])

proc.wait_simdt(10)

txt = txt.."\n\nThe attitude frame can be selected with the 'FRAME' dial."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=9,y=64,z=0}),vec.add(adictrl_ref,{x=85,y=64,z=0}),vec.add(adictrl_ref,{x=9,y=124,z=0}),vec.add(adictrl_ref,{x=85,y=124,z=0}))

proc.wait_simdt(10)

txt = "'ECL' stands for ecliptic frame: The plane of the ecliptic is represented by the (pitch=0) plane. The vernal equinox is represented by the (pitch=0,yaw=0) direction, and the ecliptic north pole is represented by the (pitch=+90) direction."
note:set_text(txt)
v:set_attrefmode(0)
oapi.set_panelblink(vec.add(adictrl_ref,{x=5,y=93,z=0}),vec.add(adictrl_ref,{x=27,y=93,z=0}),vec.add(adictrl_ref,{x=5,y=110,z=0}),vec.add(adictrl_ref,{x=27,y=110,z=0}))

proc.wait_simdt(20)

txt = "'EQU' stands for equatorial frame: The equatorial plane of the body we are in orbit around is represented by the (pitch=0) plane. The ascending node of the ecliptic is represented by the (pitch=0,yaw=0) direction, and the positive (north) direction of the planet's rotation axis is represented by the (pitch=+90) direction."
note:set_text(txt)
v:set_attrefmode(1)
oapi.set_panelblink(vec.add(adictrl_ref,{x=9,y=78,z=0}),vec.add(adictrl_ref,{x=34,y=78,z=0}),vec.add(adictrl_ref,{x=9,y=93,z=0}),vec.add(adictrl_ref,{x=34,y=93,z=0}))

proc.wait_simdt(20)

txt = "'OV/OM' stands for orbital velocity / orbital moment vector. Unlike the previous two frames, this is not an inertial frame. Instead it rotates as our spacecraft proceeds along its orbital trajectory. The direction of the orbital velocity vector (relative to the body being orbited) is represented by the (pitch=0,yaw=0) axis. The direction of the orbital moment vector (perpendicular to the orbital plane) is represented by the (pitch=0,yaw=270) axis. If the orbit is circular, then the (pitch=0) plane represents the local horizon plane. The OV/OM frame is equivalent to the 'Orbit' HUD mode, but with a 90 degree roll offset."
note:set_text(txt)
v:set_attrefmode(2)
oapi.set_panelblink(vec.add(adictrl_ref,{x=28,y=72,z=0}),vec.add(adictrl_ref,{x=59,y=72,z=0}),vec.add(adictrl_ref,{x=28,y=84,z=0}),vec.add(adictrl_ref,{x=59,y=84,z=0}))

proc.wait_simdt(30)

txt = "'LH/LN' stands for local horizon / local north. Like OV/OM, this is a rotating frame. The (pitch=0) plane represents the local horizon, and the (pitch=0,yaw=0) direction is the north direction in the horizon plane. The (pitch=-90) direction points towards the centre of the planet, and (pitch=+90) points away from it. This frame is equivalent to the 'Surface' HUD mode."
note:set_text(txt)
v:set_attrefmode(3)
oapi.set_panelblink(vec.add(adictrl_ref,{x=55,y=78,z=0}),vec.add(adictrl_ref,{x=83,y=78,z=0}),vec.add(adictrl_ref,{x=55,y=93,z=0}),vec.add(adictrl_ref,{x=83,y=93,z=0}))

proc.wait_simdt(20)

txt = "'NAV 1' and 'NAV 2' are special modes. They feed data from the Nav-1 and Nav-2 receivers to the ADI. The reference frame depends on the type of transmitter the radio is currently tuned to. For example, if the radio is tuned to an IDS (instrument docking system) transmitter, then the ADI ball is aligned with the docking approach direction."
note:set_text(txt)
v:set_attrefmode(4)
oapi.set_panelblink(vec.add(adictrl_ref,{x=57,y=93,z=0}),vec.add(adictrl_ref,{x=85,y=93,z=0}),vec.add(adictrl_ref,{x=57,y=123,z=0}),vec.add(adictrl_ref,{x=85,y=123,z=0}))

proc.wait_simdt(20)

txt = "All of the predefined frames can be modified by adding offsets to the roll, pitch and yaw angles. This will rotate the reference frame against the frame of the base mode."
note:set_text(txt)
v:set_attrefmode(3)
oapi.set_panelblink()

proc.wait_simdt(10)

txt = txt.."\n\nThe 'OFFSET' button group can be used to define the offset angles."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=9,y=125,z=0}),vec.add(adictrl_ref,{x=123,y=125,z=0}),vec.add(adictrl_ref,{x=9,y=177,z=0}),vec.add(adictrl_ref,{x=123,y=177,z=0}))

proc.wait_simdt(10)

txt = "Make sure that the 'FRM/TGT' selector switch is set to 'FRM'. This will add the offsets to the reference frame, rather than the target (which will be discussed later)."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=8,y=134,z=0}),vec.add(adictrl_ref,{x=30,y=134,z=0}),vec.add(adictrl_ref,{x=8,y=178,z=0}),vec.add(adictrl_ref,{x=30,y=178,z=0}))

proc.wait_simdt(10)

txt = "Use the 'R', 'P' and 'Y' toggles to add or subtract offsets in the roll, pitch and yaw angles."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=52,y=134,z=0}),vec.add(adictrl_ref,{x=121,y=134,z=0}),vec.add(adictrl_ref,{x=52,y=178,z=0}),vec.add(adictrl_ref,{x=121,y=178,z=0}))

proc.wait_simdt(5)
ofs = {x=0,y=0,z=0}
for i=1,30 do
  ofs.y = ofs.y + RAD
  v:set_attrefoffset(ofs)
  proc.wait_simdt(0.1)
end
for i=1,30 do
  ofs.z = ofs.z + RAD
  v:set_attrefoffset(ofs)
  proc.wait_simdt(0.1)
end
for i=1,30 do
  ofs.x = ofs.x + 2*RAD
  v:set_attrefoffset(ofs)
  proc.wait_simdt(0.1)
end

proc.wait_simdt(5)

txt = "The current offset angles (in degrees) are shown in the 'OFS' line of the digital display."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=8,y=20,z=0}),vec.add(adictrl_ref,{x=162,y=20,z=0}),vec.add(adictrl_ref,{x=8,y=32,z=0}),vec.add(adictrl_ref,{x=162,y=32,z=0}))

proc.wait_simdt(10)

txt = "To remove the frame offsets, press the 'RESET' button."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=29,y=134,z=0}),vec.add(adictrl_ref,{x=54,y=134,z=0}),vec.add(adictrl_ref,{x=29,y=178,z=0}),vec.add(adictrl_ref,{x=54,y=178,z=0}))

proc.wait_simdt(5)
ofs = {x=0,y=0,z=0}
v:set_attrefoffset(ofs)

proc.wait_simdt(10)

txt = "The ADI ball also contains two 'error needles' (the horizontal and vertical red bars) which can be used to designate a direction in the reference frame."
note:set_text(txt)
oapi.set_panelblink({x=adicnt.x-12,y=adicnt.y-40,z=0},{x=adicnt.x+12,y=adicnt.y-40,z=0},{x=adicnt.x-12,y=adicnt.y+70,z=0},{x=adicnt.x+12,y=adicnt.y+70,z=0})
proc.wait_simdt(5)
oapi.set_panelblink({x=adicnt.x-40,y=adicnt.y-12,z=0},{x=adicnt.x+70,y=adicnt.y-12,z=0},{x=adicnt.x-40,y=adicnt.y+12,z=0},{x=adicnt.x+70,y=adicnt.y+12,z=0})

proc.wait_simdt(5)

txt = "The target mode can be selected with the 'TARGET' dial."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=89,y=64,z=0}),vec.add(adictrl_ref,{x=161,y=64,z=0}),vec.add(adictrl_ref,{x=89,y=124,z=0}),vec.add(adictrl_ref,{x=161,y=124,z=0}))

proc.wait_simdt(10)

txt = "'NONE' simply disables the target mode and centers the error needles on the ball."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=88,y=78,z=0}),vec.add(adictrl_ref,{x=113,y=78,z=0}),vec.add(adictrl_ref,{x=88,y=93,z=0}),vec.add(adictrl_ref,{x=113,y=93,z=0}))

proc.wait_simdt(10)

txt = txt.."\n\nThe 'TGT' indicator on the ADI ball instrument displays 'OFF'."
note:set_text(txt)
oapi.set_panelblink(vec.add(adicnt,{x=-82,y=-64,z=0}),vec.add(adicnt,{x=-54,y=-64,z=0}),vec.add(adicnt,{x=-82,y=-39,z=0}),vec.add(adicnt,{x=-54,y=-39,z=0}))

proc.wait_simdt(10)

txt = "'FIXED' marks a fixed pitch/yaw point in the reference frame. By default, this is the (pitch=0,yaw=0) direction ..."
note:set_text(txt)
v:set_attreftgtmode(1)
oapi.set_panelblink(vec.add(adictrl_ref,{x=113,y=72,z=0}),vec.add(adictrl_ref,{x=139,y=72,z=0}),vec.add(adictrl_ref,{x=113,y=84,z=0}),vec.add(adictrl_ref,{x=139,y=84,z=0}))

proc.wait_simdt(10)

txt = txt.."\n\n... but you can select any pitch/yaw direction by adding offsets."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=9,y=125,z=0}),vec.add(adictrl_ref,{x=123,y=125,z=0}),vec.add(adictrl_ref,{x=9,y=177,z=0}),vec.add(adictrl_ref,{x=123,y=177,z=0}))

proc.wait_simdt(10)

txt = "Make sure that the 'FRM/TGT' selector switch is set to 'TGT'. This will add the offsets to the target direction, rather than the reference frame."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=8,y=134,z=0}),vec.add(adictrl_ref,{x=30,y=134,z=0}),vec.add(adictrl_ref,{x=8,y=178,z=0}),vec.add(adictrl_ref,{x=30,y=178,z=0}))

proc.wait_simdt(5)

v:set_attoffsetmode(1)

proc.wait_simdt(5)

txt = "Then use the 'P' and 'Y' toggles to add or subtract offsets to the target direction pitch and yaw angles. Note that the 'R' (roll) toggle is inactive in this mode."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=75,y=134,z=0}),vec.add(adictrl_ref,{x=121,y=134,z=0}),vec.add(adictrl_ref,{x=75,y=178,z=0}),vec.add(adictrl_ref,{x=121,y=178,z=0}))

proc.wait_simdt(5)
ofs = {x=0,y=0,z=0}
for i=1,30 do
  ofs.y = ofs.y - RAD
  v:set_atttgtoffset(ofs)
  proc.wait_simdt(0.1)
end
for i=1,30 do
  ofs.z = ofs.z - RAD
  v:set_atttgtoffset(ofs)
  proc.wait_simdt(0.1)
end

proc.wait_simdt(5)

txt = "The current target offset angles (in degrees) are shown in the 'OFS' line of the digital display."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=8,y=44,z=0}),vec.add(adictrl_ref,{x=162,y=44,z=0}),vec.add(adictrl_ref,{x=8,y=56,z=0}),vec.add(adictrl_ref,{x=162,y=56,z=0}))

proc.wait_simdt(10)

txt = "As with the reference frame, target offsets can be removed by pressing the 'RESET' button."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=29,y=134,z=0}),vec.add(adictrl_ref,{x=54,y=134,z=0}),vec.add(adictrl_ref,{x=29,y=178,z=0}),vec.add(adictrl_ref,{x=54,y=178,z=0}))

proc.wait_simdt(5)
ofs = {x=0,y=0,z=0}
v:set_atttgtoffset(ofs)

proc.wait_simdt(10)

txt = "Note that the error needles indicate not only the direction towards the target, but also the opposite direction, away from the target. To distinguish between the two, the 'TGT' indicator on the ADI shows 'TO' or 'FROM'."
note:set_text(txt)
oapi.set_panelblink(vec.add(adicnt,{x=-82,y=-64,z=0}),vec.add(adicnt,{x=-54,y=-64,z=0}),vec.add(adicnt,{x=-82,y=-39,z=0}),vec.add(adicnt,{x=-54,y=-39,z=0}))
v:set_thrustergrouplevel(THGROUP.ATT_YAWLEFT,1)

proc.wait_simdt(17)

v:set_thrustergrouplevel(THGROUP.ATT_YAWLEFT,0)

proc.wait_simdt(3)

v:set_navmode(NAVMODE.KILLROT)

proc.wait_simdt(15)

txt = "In addition to the fixed target direction, the ADI can also dynamically display the bearing of a NAV radio transmitter. To activate this mode, first set the 'FRAME' selector to NAV 1 or NAV 2, and make sure that the corresponding receiver is tuned to a transmitter in range."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=57,y=93,z=0}),vec.add(adictrl_ref,{x=85,y=93,z=0}),vec.add(adictrl_ref,{x=57,y=123,z=0}),vec.add(adictrl_ref,{x=85,y=123,z=0}))

proc.wait_simdt(5)
v:set_attrefmode(4)
txt = txt.."\n\nIn this case, NAV 1 has been tuned to the IDS (instrument docking system) frequency of the nearby ISS."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=8,y=13,z=0}),vec.add(adictrl_ref,{x=162,y=13,z=0}),vec.add(adictrl_ref,{x=8,y=25,z=0}),vec.add(adictrl_ref,{x=162,y=25,z=0}))

proc.wait_simdt(10)

txt = "Next, set the 'TARGET' dial to 'N. BRG' (NAV bearing)."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=138,y=78,z=0}),vec.add(adictrl_ref,{x=164,y=78,z=0}),vec.add(adictrl_ref,{x=138,y=93,z=0}),vec.add(adictrl_ref,{x=164,y=93,z=0}))

proc.wait_simdt(5)
v:set_attreftgtmode(2)
proc.wait_simdt(5)

txt = "When the ADI is slaved to a receiver tuned to an IDS signal, and the target mode is set to bearing information, the ADI indicator acts similarly to the 'Docking' MFD mode, where the ball takes the role of the MFD's attitude alignment indicator (x), and the error needles take the role of the lateral offset indicator (+)."
note:set_text(txt)
oapi.set_panelblink()

v:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT,0.5)
v:set_thrustergrouplevel(THGROUP.ATT_PITCHDOWN,1)
proc.wait_simdt(5)
v:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT,0)
v:set_thrustergrouplevel(THGROUP.ATT_PITCHDOWN,0)
proc.wait_simdt(8)
v:set_navmode(NAVMODE.KILLROT)

txt = "When you have the ball aligned at (R=0,P=0,Y=0), and the error needles are centered, you are located on the correct approach path to the dock, with the correct vessel attitude."
note:set_text(txt)

proc.wait_simdt(10)
txt = txt.."\n\n(You can try this later. First let's finish the description of the ADI functions.)"
note:set_text(txt)

proc.wait_simdt(7)

txt = "The remaining target mode is 'N. Vel'. Like, N. Brg, this is mode relies on data from a Nav receiver, so the frame mode must be set to Nav 1 or Nav 2, and the associated receiver must be tuned to a transmitter."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=144,y=93,z=0}),vec.add(adictrl_ref,{x=166,y=93,z=0}),vec.add(adictrl_ref,{x=144,y=110,z=0}),vec.add(adictrl_ref,{x=166,y=110,z=0}))

proc.wait_simdt(5)
v:set_attreftgtmode(3)
proc.wait_simdt(5)

txt = txt.."\n\nIn this mode however, the target direction is given by the relative velocity between the target transmitter and yourself, expressed in the reference frame."
note:set_text(txt)

proc.wait_simdt(10)

txt = "This mode is useful for judging your approach towards a dock, landing pad, VOR transmitter, etc."
note:set_text(txt)

proc.wait_simdt(5)

txt = "For example, if you are tuned to an IDS or XPDR transmitter, then the N. Vel direction is equivalent to the velocity markers in the Docking HUD mode."
note:set_text(txt)

proc.wait_simdt(10)
v:set_attreftgtmode(0)
v:set_attrefmode(0)

txt = "The final component of the ADI are the rate indicators. These are the scales at the top, bottom and right edges of the instrument."
note:set_text(txt)
oapi.set_panelblink(vec.add(adicnt,{x=-44,y=-84,z=0}),vec.add(adicnt,{x=44,y=-84,z=0}),vec.add(adicnt,{x=-44,y=-64,z=0}),vec.add(adicnt,{x=44,y=-64,z=0}))
proc.wait_simdt(5)
oapi.set_panelblink(vec.add(adicnt,{x=63,y=-44,z=0}),vec.add(adicnt,{x=83,y=-44,z=0}),vec.add(adicnt,{x=63,y=44,z=0}),vec.add(adicnt,{x=83,y=44,z=0}))
proc.wait_simdt(5)
oapi.set_panelblink(vec.add(adicnt,{x=-44,y=64,z=0}),vec.add(adicnt,{x=44,y=64,z=0}),vec.add(adicnt,{x=-44,y=84,z=0}),vec.add(adicnt,{x=44,y=84,z=0}))
proc.wait_simdt(5)

txt = "The top scale indicates the current roll rate..."
note:set_text(txt)
oapi.set_panelblink(vec.add(adicnt,{x=-44,y=-84,z=0}),vec.add(adicnt,{x=44,y=-84,z=0}),vec.add(adicnt,{x=-44,y=-64,z=0}),vec.add(adicnt,{x=44,y=-64,z=0}))
v:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT,1)
proc.wait_simdt(3)
v:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT,0)
v:set_navmode(NAVMODE.KILLROT)
proc.wait_simdt(5)

txt = txt.."\n\n...the scale on the right shows the current pitch rate..."
note:set_text(txt)
oapi.set_panelblink(vec.add(adicnt,{x=63,y=-44,z=0}),vec.add(adicnt,{x=83,y=-44,z=0}),vec.add(adicnt,{x=63,y=44,z=0}),vec.add(adicnt,{x=83,y=44,z=0}))
v:set_navmode(0)
v:set_thrustergrouplevel(THGROUP.ATT_PITCHUP,1)
proc.wait_simdt(5)
v:set_thrustergrouplevel(THGROUP.ATT_PITCHUP,0)
v:set_navmode(NAVMODE.KILLROT)
proc.wait_simdt(5)

txt = txt.."\n\n...and the bottom scale shows the current yaw rate."
note:set_text(txt)
oapi.set_panelblink(vec.add(adicnt,{x=-44,y=64,z=0}),vec.add(adicnt,{x=44,y=64,z=0}),vec.add(adicnt,{x=-44,y=84,z=0}),vec.add(adicnt,{x=44,y=84,z=0}))
v:set_navmode(0)
v:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT,1)
proc.wait_simdt(5)
v:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT,0)
v:set_navmode(NAVMODE.KILLROT)
proc.wait_simdt(5)

txt = txt.."\n\nEach scale tick mark represents a rate of 2deg./s with a range of +/- 10 degrees in each axis."
note:set_text(txt)
oapi.set_panelblink()
proc.wait_simdt(10)

txt = "The rates can be displayed in two different frames: Either in the local spacecraft frame, where the rates are directly related to the RCS thrusters for the corresponding axis..."
note:set_text(txt)
oapi.set_panelblink(vec.add(adictrl_ref,{x=133,y=134,z=0}),vec.add(adictrl_ref,{x=156,y=134,z=0}),vec.add(adictrl_ref,{x=133,y=178,z=0}),vec.add(adictrl_ref,{x=156,y=178,z=0}))
v:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT,1)
v:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT,0.2)
proc.wait_simdt(3)
v:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT,0)
v:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT,0)
proc.wait_simdt(7)

txt = txt.."\n\n...or the current ADI reference frame, where the rates correspond to the rotation of the ball.\n\nThe frames can be selected with the 'RATE' switch on the control panel."
note:set_text(txt)
v:set_atttgtframemode(0)
proc.wait_simdt(10)
oapi.set_panelblink()
v:set_navmode(NAVMODE.KILLROT)

txt = "You may come across the ADI ball in two different layouts. So far we have used a ball that uses the pitch=0 plane as the plane of reference, with poles at pitch = +/-90 degrees. The range of yaw angles is 0-360 degrees."
note:set_text(txt)
proc.wait_simdt(10)

txt = "An alternative layout uses the yaw = 0 plane as plane of reference, with poles at yaw = +/-90, and pitch range 0-360 degrees."
note:set_text(txt)
proc.wait_simdt(5)
v:set_adilayout(1)
oapi.set_panelblink(adiball_area[1],adiball_area[2],adiball_area[3],adiball_area[4])
proc.wait_simdt(5)
oapi.set_panelblink()

txt = txt.."\n\nThis is the layout used by the ADI instrument in Apollo and pre-glass cockpit Space Shuttle spacecraft."
note:set_text(txt)
proc.wait_simdt(7)

txt = "Note that the two layouts only differ in the markings printed on the ball, not in the way the ball behaves."
note:set_text(txt)
proc.wait_simdt(7)

txt = txt.."\n\nIt is important to remember that the definitions of the Euler angles (roll, pitch, yaw) in the two layouts are different."
note:set_text(txt)
proc.wait_simdt(7)

txt = txt.."\n\nTherefore, the same spacecraft orientation will result in two different sets of Euler angles depending on the chosen layout."
note:set_text(txt)
proc.wait_simdt(7)

txt = "The layout can be selected by adding an ADI_LAYOUT 0/1 entry in the Shuttle-A's scenario definition, or with the V:set_adilayout function in a terminal MFD."
note:set_text(txt)
proc.wait_simdt(10)

txt = "This concludes the introduction of the ADI. Got everything? If not, have a look at Doc\\ShuttleA.pdf, section 'Reference frame and ADI'."
note:set_text(txt)
proc.wait_simdt(7)
txt = txt.."\n\nOtherwise, let's have a little test to see if you have mastered the ADI yet..."
note:set_text(txt)
proc.wait_simdt(7)


txt = "I am going to turn off the MFDs and HUD. You should rely on the ADI only."
note:set_text(txt)
proc.wait_simdt(5)
v:set_adilayout(0)

note:set_pos (0.2,0.05,0.8,0.95);
note:set_colour ({r=0.7,g=0.8,b=1})

hEarth = oapi.get_objhandle('Earth')
hDock = v:get_dockhandle (0)
obl = 0.4090928023
Re = {m11=1,m12=0,m13=0,  m21=0,m22=math.cos(obl),m23=-math.sin(obl),  m31=0,m32=math.sin(obl),m33=math.cos(obl)}


-- The tests

function taskSkip()
  oapi.open_mfd(0,0)
  oapi.open_mfd(1,0)
  oapi.set_hudmode(0)
  proc.skip()
end

function celestialDir(ra,dec)
  d = {x=math.cos(dec)*math.cos(ra), y=math.sin(dec), z=-math.cos(dec)*math.sin(ra)}
  return mat.mul(Re,d)
end

function checkTask1()
  R=v:get_rotationmatrix()
  vz = vec.unit({x=R.m13, y=R.m23, z=R.m33})
  pg = vec.unit(v:get_relativevel(hEarth))
  phi = math.acos(vec.dotp(vz,pg))
  if phi < 2*RAD and vec.length(v:get_angvel()) < 0.005 then
    return true
  else
    return false
  end
end

function checkTask2()
  R=v:get_rotationmatrix()
  vz = vec.unit({x=R.m13, y=R.m23, z=R.m33})
  ve = {x=1,y=0,z=0}
  phi = math.acos(vec.dotp(vz,ve))
  if phi < 2*RAD and vec.length(v:get_angvel()) < 0.005 then
    return true
  else
    return false
  end
end

function checkTask3()
  R=v:get_rotationmatrix()
  vz = vec.unit({x=R.m13, y=R.m23, z=R.m33})
  cs = {x=-Re.m12, y=-Re.m22, z=-Re.m32}
  phi = math.acos(vec.dotp(vz,cs))
  if phi < 2*RAD and vec.length(v:get_angvel()) < 0.005 then
    return true
  else
    return false
  end
end

d_sirius = celestialDir(-(6+45/60+9/3600)/12*PI, -(16+42/60+58/3600)*RAD)

function checkTask4()
  R=v:get_rotationmatrix()
  vz = vec.unit({x=R.m13, y=R.m23, z=R.m33})
  cs = {x=-Re.m12,y=-Re.m22,z=-Re.m32}
  phi = math.acos(vec.dotp(vz,d_sirius))
  if phi < 2*RAD and vec.length(v:get_angvel()) < 0.005 then
    return true
  else
    return false
  end
end

function checkTask5()
  mate = v:get_dockstatus (hDock)
  if mate ~= nil then -- make sure we are docked to ISS
    v2 = vessel.get_interface(mate)
    if v2:get_name()~='ISS' then
      mate = nil
    else
      mmate = v2:get_dockstatus(v2:get_dockhandle(0))
      if mmate == nil or oapi.get_objname(mmate) ~= 'SH-15' then
        note:set_text("Wrong dock! You must dock at port 1 to pass this test.")
        mate = nil
      end
    end        
  end
  if mate ~= nil then
    return true
  else
    return false
  end
end


-- Test 1: turn prograde
note:set_text("Here is the first task: Turn your vessel to a prograde direction and hold.")
step_done = false
t0 = 0
ccount = 0
while not step_done do
  taskSkip()
  if v:get_navmode(NAVMODE.PROGRADE) == true then
     v:set_navmode(NAVMODE.PROGRADE,false)
     warning = {"That would be cheating. Please don't push this button again during this task.","Do you want to make me angry?","I can fail you for this, you know.","You seem to have serious attitude problems ...","That's it, test failed for obnoxious behaviour. If you want to retake it, you'll have to go through the tutorial again."}
     if ccount < 5 then
       ccount = ccount+1
     end
     note:set_text(warning[ccount])
     t0 = oapi.get_systime()
  end
  if t0 > 0 and oapi.get_systime()-t0 > 5 then
    if ccount == 5 then
      proc.wait_sysdt(5)
      oapi.del_annotation(note)
      error('Script terminated.')
    end
	note:set_text("Here is the first task: Turn your vessel to a prograde direction and hold.")
	t0 = 0
  end
  step_done = checkTask1()
end
note:set_text("Task 1 passed!")
proc.wait_simdt(5)

-- Test 2: turn to vernal equinox
note:set_text("The next task: Turn your vessel towards the vernal equinox and hold. (The vernal equinox is the ascending node of the ecliptic with respect to Earth's equatorial plane.)")
step_done = false
while not step_done do
  taskSkip()
  step_done = checkTask2()
end
note:set_text("Task 2 passed!")
proc.wait_simdt(5)

-- Test 3: turn to celestial south pole
note:set_text("The next task: Turn your vessel towards the celestial south pole and hold. (The celestial south pole is the negative of Earth's spin axis direction.)")
step_done = false
while not step_done do
  taskSkip()
  step_done = checkTask3()
end
note:set_text("Task 3 passed!")
proc.wait_simdt(5)

-- Test 4: turn towards Sirius
note:set_text("Now, turn your vessel towards Sirius. Sirius has the following celestial coordinates: Right ascension 6h 45m 9s, declination -16deg 42' 58''. (You may need pen and paper to convert these coordinates to decimal degrees.)\n\nTip: consider the direction in which right ascension coordinates are increasing.")
step_done = false
while not step_done do
  taskSkip()
  step_done = checkTask4()
end
note:set_text("Task 4 passed!")
proc.wait_simdt(5)

-- Test 5: Dock with ISS
note:set_text("All good so far. Here is the last test: Dock with the ISS, docking port 1. (NAV-1 has already been tuned to its IDS transmitter.)")
step_done = false
while not step_done do
  taskSkip()
  step_done = checkTask5()
end
note:set_text("Hard dock! You pass all tests and are now a certified ADI operator. Congratulations!")
proc.wait_simdt(10)
oapi.del_annotation(note)