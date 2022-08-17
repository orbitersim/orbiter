v1 = vessel.get_interface('srb1')
v2 = vessel.get_interface('srb2')

note = oapi.create_annotation()
note:set_pos (0.3,0.05,0.9,0.95)
note:set_colour ({r=1,g=0.6,b=0.2})

note2 = oapi.create_annotation()
note2:set_pos (0.3,0.8,0.9,1)
note2:set_colour ({r=1,g=0.7,b=0.3})

intro = 'Space physics: Spin stabilisation'

note:set_text (intro)
proc.wait_sysdt(5)

intro = intro..'\n\
The attitude of an object can be stabilised by spinning\
it rapidly around one of its axes of symmetry. In this\
scenario, two rockets are placed in a low orbit. One is\
spinning around its longitudinal axis at 1Hz, the other\
is not.'

note:set_text (intro)
proc.wait_sysdt(10)

intro = intro..'\n\
The spinning rocket maintains its global orientation,\
while the other starts to rotate under the influence of\
gravity gradient torque.'

note:set_text (intro)
proc.wait_sysdt(10)

note:set_text ('If the effect doesn\'t show, make sure that\
the Gravity gradient torque option is ticked in the Parameters\
tab of the Orbiter Launchpad.')
proc.wait_sysdt(10)

note:set_text ('Speeding up the simulation time ...')
oapi.set_tacc(100)

proc.wait_simtime(700)
note:set_text ('Note how the rocket in front starts to rotate\
under the influence of the gravity gradient, while the\
spinning rocket behind maintains its attitude with\
respect to the global (non-rotating) frame of reference.')

proc.wait_simtime(2000)
note2:set_text ('(Dipping into Earth\'s shadow ...)')
proc.wait_simtime(4200)
note2:set_text ('')

proc.wait_simtime(4500)
note:set_text ('I am now going to stop the spinning rocket.\
Watch how it also slowly starts to rotate in the inhomogeneous\
gravity field.')
v1:set_angvel({x=0,y=0,z=0})

proc.wait_simtime(5500)
note:set_text ('Now I am spinning up the other rocket.\
As a result, it will maintain the orientation of its\
longitudinal axis.')
v2:set_angvel({x=0,y=0,z=360*RAD})

proc.wait_sysdt(200)
oapi.del_annotation (note)