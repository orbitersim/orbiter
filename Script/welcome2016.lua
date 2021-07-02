menuvis = oapi.get_mainmenuvisibilitymode()
if menuvis == 0 then
	oapi.set_mainmenuvisibilitymode(2)
end

h = oapi.create_annotation()
h:set_pos(0.04,0.8,0.4,1)
h:set_size(1.0)
h:set_colour({r=0.9,g=1,b=0.9})

t1 = oapi.get_simtime()
t0 = t1
dt = 0

function tstep()
	proc.skip()
	t1 = oapi.get_simtime()
	dt = t1-t0
	t0 = t1
end

dst=38867.083

mjd = oapi.get_simmjd()
if mjd < 57457.66028 then

	oapi.set_cameramode({mode='track',trackmode='relative',reldist=dst,phi=0.226,tht=-0.743})
	oapi.set_cameraaperture(RAD*15)
	while t1 < 3 do
		tstep()
	end

	spd=0
	while dst > 20 do
		tstep()
		if spd < 1 then
			spd = spd+dt*0.05
			spd = math.min(spd,2)
		end
		dst = dst - spd*dt*dst;
		oapi.set_cameramode({mode='track',trackmode='relative',reldist=dst,phi=0.226,tht=-0.743})
	end

	camlng=-80.74391
	oapi.set_cameramode({mode='ground',ref='Earth',lng=camlng, lat=28.55112, alt=1188.91, alt_above_ground=0})
	while camlng < -80.55 do
		tstep()
		camlng = camlng + dt*0.02
		oapi.set_cameramode({mode='ground',ref='Earth',lng=camlng, lat=28.55112, alt=1188.91, alt_above_ground=0})
	end

	oapi.set_cameratarget(oapi.get_objhandle('STS-101'));
	camlat=28.50
	spd=0.02
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.618111, lat=camlat, alt=100, alt_above_ground=0})
	while camlat < 28.629145 do
		tstep()
		if camlat > 28.59 then
			spd = (28.6295-camlat)/(28.6295-28.59)*0.02
		end
		camlat=camlat + dt*spd
		oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.618111, lat=camlat, alt=100, alt_above_ground=0})
	end	

	while t1 < 43 do
		tstep()
	end
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.62087, lat=28.62712, alt=5.29, phi=1.59, tht=1.54, alt_above_ground=0})

	while t1 < 46 do
		tstep()
	end
	oapi.set_cameraaperture(RAD*20)
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.62111, lat=28.62718, alt=93.68, phi=0.02, tht=0.03, alt_above_ground=0})

	while t1 < 50 do
		tstep()
	end
	oapi.set_cameramode({mode='cockpit', cockpitmode='vc', pos=2, lean=1})

	while t1 < 56 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=25, phi=3.112, tht=-0.570})
	oapi.set_cameraaperture(RAD*5)

	while t1 < 61 do
		tstep()
	end
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.67809, lat=28.53594, alt=62.47, phi=1.09, tht=0.06, alt_above_ground=0})


	while t1 < 66 do
		tstep()
	end
	oapi.set_cameramode({mode='cockpit', cockpitmode='vc', pos=1, lean=3})
	oapi.set_cameraaperture(RAD*25)

	while t1 < 71 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=17.857, phi=-2.210, tht=-1.057})
	oapi.set_cameraaperture(RAD*15)
	-- oapi.set_cameramode({mode='track',trackmode='relative',reldist=14.468, phi=0.001, tht=-0.168})
	h:set_text('>>> Check out the latest news and updates at the Orbiter home page orbit.medphys.ucl.ac.uk')
	
	while t1 < 78 do
		tstep()
	end
	camlat=29.05168
	while t1 < 90 do
		oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.61916, lat=camlat, alt=11790.00, alt_above_ground=0})
		tstep()
		camlat=camlat-dt*0.05
	end
	h:set_text('')
	
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=4.016, phi=-0.008, tht=3.093})
	while t1 < 100 do
		tstep()
	end
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.57007, lat=28.68031, alt=13330.00, alt_above_ground=0})

	while t1 < 163 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=9.491, phi=-0.404, tht=0.480})

	while t1 < 390 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=9.491, phi=1.697, tht=0.730})

	while t1 < 404 do
		tstep()
	end
	oapi.set_cameramode({mode='cockpit', cockpitmode='vc', pos=1, lean=3})

	while t1 < 560 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=9.491, phi=1.697, tht=0.730})
	h:set_text('>>> Join the Orbiter community at orbiter-forum.com!')
	while t1 < 600 do
		tstep()
	end
	h:set_text('')
	oapi.set_cameramode({mode='track',trackmode='global',reldist=10.449, phi=3.099, tht=-0.272})
	oapi.set_cameraaperture(RAD*10)

	while t1 < 700 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=4.351, phi=1.235, tht=-1.600})
	oapi.set_cameraaperture(RAD*20)

	while t1 < 800 do
		tstep()
	end
	oapi.set_cameratarget(oapi.get_objhandle('ISS'),1)
	oapi.set_cameramode({mode='track',trackmode='global',reldist=1.784, phi=-2.661, tht=-1.761})
	oapi.set_cameraaperture(RAD*20)

	distinit=1.784
	distfinal=1.007
	phiinit=-2.661
	phifinal=-2.721
	thtinit=-1.761
	thtfinal=-2.508
	while t1 < 830 do
		tstep()
		step = (t1-800)/30
		d = distinit + step*(distfinal-distinit)
		p = phiinit + step*(phifinal-phiinit)
		t = thtinit + step*(thtfinal-thtinit)
		oapi.set_cameramode({mode='track',trackmode='global',reldist=d, phi=p, tht=t})
	end

	distinit=13.199
	distfinal=2.265
	phiinit=-1.424
	phifinal=-0.241
	thtinit=-0.953
	thtfinal=-1.416
	oapi.set_cameratarget(oapi.get_objhandle('STS-101'),1)
	while t1 < 857 do
		tstep()
		step = (t1-830)/27
		d = distinit + step*(distfinal-distinit)
		p = phiinit + step*(phifinal-phiinit)
		t = thtinit + step*(thtfinal-thtinit)
		oapi.set_cameramode({mode='track',trackmode='global',reldist=d, phi=p, tht=t})
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=1.975, phi=1.315, tht=0.764})
	oapi.set_cameraaperture(RAD*10)
	h:set_text('>>> Find free downloads at the Orbiter addon repository orbithangar.com')

	while t1 < 871 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=15.804, phi=2.955, tht=0.661})
	oapi.set_cameraaperture(RAD*10)

	while t1 < 875 do
		tstep()
	end
	h:set_text('')

	distinit=27.018
	distfinal=27.018
	phiinit=1.086
	phifinal=-1.389
	thtinit=0.049
	thtfinal=-1.491
	oapi.set_cameramode({mode='track',trackmode='global',reldist=distinit, phi=phiinit, tht=thtinit})
	oapi.set_cameraaperture(RAD*15)
	while t1 < 893 do
		tstep()
		step = (t1-875)/18
		d = distinit + step*(distfinal-distinit)
		p = phiinit + step*(phifinal-phiinit)
		t = thtinit + step*(thtfinal-thtinit)
		oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=p,tht=t})
	end

	distinit=3.794
	distfinal=4.376
	phiinit=1.249
	phifinal=0.372
	thtinit=0.064
	thtfinal=-0.191
	oapi.set_cameramode({mode='track',trackmode='global',reldist=distinit,phi=phiinit,tht=thtinit})
	oapi.set_cameraaperture(RAD*20)
	while t1 < 971 do
		tstep()
		step = (t1-893)/78
		d = distinit + step*(distfinal-distinit)
		p = phiinit + step*(phifinal-phiinit)
		t = thtinit + step*(thtfinal-thtinit)
		oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=p,tht=t})
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=5.894, phi=-1.916, tht=-0.856})

	h:set_text(">>> Look for essential addons like Dan Steph's OrbiterSound and Jarmonik's D3D9 GraphicsClient.")
	while t1 < 1030 do
		tstep()
	end
	h:set_text('')
	oapi.set_cameramode({mode='track',trackmode='global',reldist=7.698, phi=1.412, tht=0.376})

	while t1 < 1050 do
		tstep()
	end
	oapi.set_cameramode({mode='cockpit', cockpitmode='vc', pos=0, lean=0})
	h:set_text('>>> Check out the orbitersim channel on youtube!')
	while t1 < 1067 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=11.486, phi=-1.850, tht=-1.104})
	h:set_text('')
	
	while t1 < 1077 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=10.357, phi=1.364, tht=-0.587})
	
	while t1 < 1087 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=4.724, phi=0.972, tht=0.668})

	while t1 < 1105 do
		tstep()
	end
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.67949, lat=28.59217, alt=7.07, alt_above_ground=0})

	while t1 < 1114 do
		tstep()
	end
	oapi.set_cameramode({mode='track',trackmode='global',reldist=19.469, phi=-0.037, tht=-0.072})

	while t1 < 1120 do
		tstep()
	end
	oapi.set_cameramode({mode='ground',ref='Earth',lng=-80.68838, lat=28.61201, alt=15.79, alt_above_ground=0})
	oapi.set_cameraaperture(RAD*5)

end

oapi.set_mainmenuvisibilitymode(menuvis)
