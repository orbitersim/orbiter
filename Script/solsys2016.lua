h = oapi.create_annotation()
h:set_size(1.1)
h:set_colour({r=0.8,g=0.8,b=1})
h:set_pos(0.05,0.2,0.5,0.9)

par1 = oapi.create_annotation()
par1:set_size(0.7)
par1:set_colour({r=1,g=1,b=1})
par1:set_pos(0.05,0.25,0.5,0.9)

t1 = oapi.get_simtime()
t0 = t1
dt = 0

function tstep()
	proc.skip()
	t1 = oapi.get_simtime()
	dt = t1-t0
	t0 = t1
end

d0 = 33.480
d1 = 1.822
ph0 = -0.361
ph1 = -2.631
th0 = -0.301
th1 = -0.342
h:set_text('Mercury')
par1:set_text('Radius: 2,440km  Mass: 3.285×10^23kg  Mean orbital radius: 57.91×10^6km (0.387AU)  Orbital period: 7.601×10^6s (88.0 days)')
oapi.set_cameratarget(oapi.get_objhandle('Mercury'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
while t1 < 20 do
	tstep()
	step = math.sin(t1/20*math.pi/2)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = 27.91808
lng1 = 117.39881
lat0 = 9.95286
lat1 = 21.03918
alt0 = 2006000.00
alt1 = 937630.89
ph0 = 0.13
ph1 = 0.14
th0 = -1.05
th1 = -1.02
oapi.set_cameraaperture(15*RAD)
oapi.set_cameramode({mode='ground',ref='Mercury',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 40 do
	tstep()
	step = (t1-20)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Mercury',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = -102.61807
lng1 = -130.00000
lat0 = 1.31802
lat1 = 1.76824
alt0 = 32630000.00
alt1 = 15000000.00
ph0 = 0.95
ph1 = 2.60
th0 = -1.56
th1 = -1.48
h:set_text('Venus')
par1:set_text('Radius: 6,052km  Mass: 4.867×10^24kg  Mean orbital radius: 108.2×10^6km (0.723AU)  Orbital period: 1.941×10^7s (224.7 days)')
oapi.set_cameramode({mode='ground',ref='Venus',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 60 do
	tstep()
	step = (t1-40)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Venus',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 7.918
d1 = 1.570
ph0 = 3.124
ph1 = 0.326
th0 = -0.340
th1 = 0.714
h:set_text("Earth")
par1:set_text('Radius: 6,371km  Mass: 5.972×10^24kg  Mean orbital radius: 149.6×10^6km (1AU)  Orbital period: 3.156×10^7s (365.24 days)')
oapi.set_cameratarget(oapi.get_objhandle('Earth'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(25*RAD)
while t1 < 80 do
	tstep()
	step = (t1-60)/20
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = -79.21589
lng1 = -80.63926
lat0 = 25.61459
lat1 = 28.58158
alt0 = 329422.77
alt1 = 311.74
ph0 = 2.65
ph1 = 2.93
th0 = -1.02
th1 = -0.30
par1:set_text('Florida and Kennedy Space Center')
oapi.set_cameramode({mode='ground',ref='Earth',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 100 do
	tstep()
	stepr = (t1-80)/20
	step = math.sin(stepr*math.pi/2)
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*stepr
	th = th0 + (th1-th0)*stepr
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = -71.40311
lng1 = -71.75295
lat0 = -40.93990
lat1 = -36.38502
a = 4500
ph = 1.63
th = -0.32
par1:set_text('South America: Andes')
oapi.set_cameramode({mode='ground',ref='Earth',lng=lng0, lat=lat0, alt=a, phi=ph, tht=th, alt_above_ground=0})
while t1 < 120 do
	tstep()
	step = (t1-100)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = 172.94102
lng1 = 177.32896
lat0 = -3.90938
lat1 = -4.20036
alt0 = 9318645.85
ph0 = 3.04
ph1 = 2.62
th0 = -1.30
th1 = -1.51
h:set_text('Moon')
par1:set_text('Natural satellite of Earth  Radius: 1,737km  Mass: 7.348×10^22kg  Mean orbital radius: 3.8×10^5km  Orbital period: 2.317×10^6s (27.32 days)')
oapi.set_cameraaperture(5*RAD)
oapi.set_cameramode({mode='ground',ref='Moon',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 140 do
	tstep()
	step = (t1-120)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Moon',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d = 2.66
ph0 = 1.453
ph1 = 1.965
th0 = -0.344
th1 = 0.785
par1:set_text('Mare Crisium to Mare Australe')
oapi.set_cameratarget(oapi.get_objhandle('Moon'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph0,tht=th0})
while t1 < 160 do
	tstep()
	step = (t1-140)/20
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

lng0 = 158.96822
lng1 = 144.89152
lat0 = 20.20244
lat1 = 38.10981
alt0 = 107911.57
alt1 = 49191.03
ph0 = -3.86
ph1 = -2.06
th0 = -0.54
th1 = -0.52
par1:set_text('Mare Moscoviense and Titov crater')
oapi.set_cameraaperture(15*RAD)
oapi.set_cameramode({mode='ground',ref='Moon',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 180 do
	tstep()
	step = (t1-160)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Moon',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d = 4
ph0 = -0.732
ph1 = 2.442
th0 = -0.272
th1 = -0.360
h:set_text('Mars')
par1:set_text('Radius: 3,390km  Mass: 6.39×10^23kg  Mean orbital radius: 227.9×10^6km (1.52AU)  Orbital period: 5.94×10^7s (687.0 days)')
oapi.set_cameratarget(oapi.get_objhandle('Mars'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph0,tht=th0})
while t1 < 200 do
	tstep()
	step = (t1-180)/20
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = -104.32048
lng1 = -87.95089
lat0 = -12.00130
lat1 = -6.95669
alt0 = 2000235.89
alt1 = 34145.76
ph0 = 0.96
ph1 = -0.11
th0 = -1.37
th1 = -0.35
par1:set_text('Noctis Labyrinthus and Valles Marineris')
oapi.set_cameramode({mode='ground',ref='Mars',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 220 do
	tstep()
	step = (t1-200)/20
	stepq = step*step
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*stepq
	oapi.set_cameramode({mode='ground',ref='Mars',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = -135.43852
lng1 = -133.63974
lat0 = 12.72653
lat1 = 17.61664
alt0 = -772
alt1 = 484396.42
ph0 = -5.20
ph1 = -4.21
th0 = -0.23
th1 = -1.44
par1:set_text('Olympus Base and Olympus Mons')
oapi.set_cameraaperture(20*RAD)
oapi.set_cameramode({mode='ground',ref='Mars',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
f0 = math.atan(-15)
f1 = math.atan(3)
while t1 < 240 do
	tstep()
	stepr = (t1-220)/20
	-- step = (math.atan(stepr*18-15)-f0)/(f1-f0)
	step = math.pow(stepr, 4)
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Mars',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d = 7.632
ph0 = 4.442
ph1 = 1.583
th0 = -0.391
th1 = 0.168
h:set_text('Phobos')
par1:set_text('Natural satellite of Mars    Mean radius: 11.27km  Mass: 1.066×10^16kg  Mean orbital radius: 9,374km  Orbital period: 2.755×10^4s (0.32 days)')
oapi.set_cameratarget(oapi.get_objhandle('Phobos'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 260 do
	tstep()
	step = (t1-240)/20
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

d = 7.632
ph0 = 1.482
ph1 = 4.443
th0 = 0.774
th1 = -0.373
h:set_text('Deimos')
par1:set_text('Natural satellite of Mars    Mean radius: 6.2km  Mass: 1.48×10^15kg  Mean orbital radius: 23,463km  Orbital period: 1.091×10^5s (1.263 days)')
oapi.set_cameratarget(oapi.get_objhandle('Deimos'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 280 do
	tstep()
	step = (t1-260)/20
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

lng0 = -12.60043
lng1 = 62.09722
lat0 = -25.91577
lat1 = 23.14283
alt0 = 1043827.72
alt1 = 673654.03
ph0 = -0.97
ph1 = 0.64
th0 = -1.60
th1 = -1.56
h:set_text('Vesta')
par1:set_text('Asteriod belt object  Mean radius: 262.5km  Mass: 2.589×10^20kg  Mean orbital radius: 3.533×10^8km (2.3618AU)  Orbital period: 1.145×10^8s (3.63 years)')
oapi.set_cameramode({mode='ground',ref='Vesta',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 300 do
	tstep()
	step = (t1-280)/20
	stepq = step*step
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*stepq
	oapi.set_cameramode({mode='ground',ref='Vesta',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 72.404
d1 = 4.258
ph0 = -0.633
ph1 = 2.483
th0 = 1.482
th1 = 3.048
h:set_text('Jupiter')
par1:set_text('Radius: 69,911km  Mass: 1.898×10^27kg  Mean orbital radius: 778.5×10^6km (5.20AU)  Orbital period: 3.743×10^8s (11.86 years)')
oapi.set_cameratarget(oapi.get_objhandle('Jupiter'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 320 do
	tstep()
	stepr = (t1-300)/20
	step = math.sin(stepr*math.pi/2)
	-- step = math.sqrt((t1-300)/20)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = 155.45761
lng1 = 152.29362
lat0 = 1.71224
lat1 = 0.41304
alt0 = 7442000.00
alt1 = 2079717.54
ph0 = 4.80
ph1 = 3.43
th0 = -1.58
th1 = -1.18
h:set_text('Io')
par1:set_text('Natural satellite of Jupiter  Radius: 1,821.6km  Mass: 8.932×10^22kg  Mean orbital radius: 421,700km  Orbital period: 1.529×10^5s (1.769 days)')
oapi.set_cameratarget(oapi.get_objhandle('Io'))
oapi.set_cameramode({mode='ground',ref='Io',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 340 do
	tstep()
	step = (t1-320)/20
	stepq = step*step
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Io',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 4.258
d1 = 6.358
ph0 = 1.764
ph1 = 5.212
th0 = 3.140
th1 = 3.105
h:set_text('Europa')
par1:set_text('Natural satellite of Jupiter  Radius: 1,560.8km  Mass: 4.800×10^22kg  Mean orbital radius: 670,900km  Orbital period: 3.068×10^5s (3.551 days)')
oapi.set_cameratarget(oapi.get_objhandle('Europa'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 360 do
	tstep()
	step = (t1-340)/20
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = 120.45467
lng1 = 133.42313
lat0 = 4.42135
lat1 = -20.09059
alt0 = 14110000.00
alt1 = 8013990.31
ph0 = -1.79
ph1 = -0.93
th0 = -1.59
th1 = -1.47
h:set_text('Ganymede')
par1:set_text('Natural satellite of Jupiter  Radius: 2,634.1km  Mass: 1.482×10^23kg  Mean orbital radius: 1.0703×10^6km  Orbital period: 6.182×10^5s (7.155 days)')
oapi.set_cameratarget(oapi.get_objhandle('Ganymede'))
oapi.set_cameramode({mode='ground',ref='Ganymede',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 380 do
	tstep()
	step = (t1-360)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Ganymede',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = -144.34554
lng1 = -46.80991
lat0 = 1.58300
lat1 = 10.48157
alt0 = 25650000.00
alt1 = 8366465.73
ph0 = 1.16
ph1 = -2.81
th0 = -1.57
th1 = -1.41
h:set_text('Callisto')
par1:set_text('Natural satellite of Jupiter  Radius: 2,410.3km  Mass: 1.076×10^23kg  Mean orbital radius: 1.883×10^6km  Orbital period: 1.442×10^6s (16.690 days)')
oapi.set_cameratarget(oapi.get_objhandle('Callisto'))
oapi.set_cameramode({mode='ground',ref='Callisto',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(10*RAD)
while t1 < 400 do
	tstep()
	step = (t1-380)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Callisto',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 374.272
d1 = 8.070
ph0 = -3.115
ph1 = -2.770
th0 = -1.082
th1 = 0.467
h:set_text('Saturn')
par1:set_text('Radius: 58,232km  Mass:  5.683×10^26kg  Mean orbital radius: 1.433×10^9km (9.58AU)  Orbital period: 9.2960×10^8s (29.46 years)')
oapi.set_cameratarget(oapi.get_objhandle('Saturn'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 420 do
	tstep()
	stepr = (t1-400)/20
	step = math.sin(stepr*math.pi/2)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

d0 = 296.345
d1 = 5.363
ph0 = 3.564
ph1 = 2.341
th0 = 0.317
th1 = 0.623
h:set_text('Mimas')
par1:set_text('Natural satellite of Saturn  Radius: 198.2km  Mass:  3.7493×10^19kg  Mean orbital radius: 185,320km  Orbital period: 81389s (0.942 days)')
oapi.set_cameratarget(oapi.get_objhandle('Mimas'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 440 do
	tstep()
	stepr = (t1-420)/20
	step = math.sin(stepr*math.pi/2)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

d0 = 5.363
d1 = 2.929
ph0 = 1.217
ph1 = 3.063
th0 = -0.162
th1 = 0.922
h:set_text('Enceladus')
par1:set_text('Natural satellite of Saturn  Radius: 252.1km  Mass: 1.0802×10^20kg  Mean orbital radius: 237,912km  Orbital period: 1.184×10^5s (1.370 days)')
oapi.set_cameratarget(oapi.get_objhandle('Enceladus'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(20*RAD)
while t1 < 460 do
	tstep()
	step = (t1-440)/20
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

lng0 = -173.44246
lng1 = -80.90251
lat0 = 7.19863
lat1 = 61.50696
alt0 = 1327000.00
alt1 = 309943.94
ph0 = -4.76
ph1 = -6.42
th0 = -1.52
th1 = -1.28
h:set_text('Tethys')
par1:set_text('Natural satellite of Saturn  Radius: 531.1km  Mass: 6.1745×10^20kg  Mean orbital radius: 294,572km  Orbital period: 1.631×10^5s (1.888 days)')
oapi.set_cameratarget(oapi.get_objhandle('Tethys'))
oapi.set_cameramode({mode='ground',ref='Tethys',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
while t1 < 480 do
	tstep()
	step = (t1-460)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Tethys',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 52.585
d1 = 2.928
ph0 = 3.019
ph1 = 2.295
th0 = 0.304
th1 = 0.276
h:set_text('Dione')
par1:set_text('Natural satellite of Saturn  Radius: 561.4km  Mass: 1.0955×10^21kg  Mean orbital radius: 377,336km  Orbital period: 2.365×10^5s (2.737 days)')
oapi.set_cameratarget(oapi.get_objhandle('Dione'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(20*RAD)
while t1 < 500 do
	tstep()
	stepr = (t1-480)/20
	step = math.sin(stepr*math.pi/2)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

lng0 = -163.81138
lng1 = -160.46787
lat0 = -4.17910
lat1 = 42.54257
alt0 = 2236000.00
alt1 = 435863.71
ph0 = 1.54
ph1 = 1.50
th0 = -1.63
th1 = -1.10
h:set_text('Rhea')
par1:set_text('Natural satellite of Saturn  Radius: 763.8km  Mass: 2.306518×10^21kg  Mean orbital radius: 527,067km  Orbital period: 3.904×10^5s (4.518 days)')
oapi.set_cameratarget(oapi.get_objhandle('Rhea'))
oapi.set_cameramode({mode='ground',ref='Rhea',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(15*RAD)
while t1 < 520 do
	tstep()
	step = (t1-500)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Rhea',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = 43.69265
lng1 = 72.05031
lat0 = -24.09029
lat1 = 7.13501
alt0 = 8272000.00
alt1 = 168666.16
ph0 = 1.53
ph1 = 3.77
th0 = -1.56
th1 = -0.60
h:set_text('Titan')
par1:set_text('Natural satellite of Saturn  Radius: 2575.5km  Mass: 1.3452×10^23kg  Mean orbital radius: 1.2217×10^6km  Orbital period: 1.378×10^6s (15.945 days)')
oapi.set_cameratarget(oapi.get_objhandle('Titan'))
oapi.set_cameramode({mode='ground',ref='Titan',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(15*RAD)
while t1 < 540 do
	tstep()
	stepr = (t1-520)/20
	step = math.sin(stepr*math.pi/2)
	stepq = step*step
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*stepq
	oapi.set_cameramode({mode='ground',ref='Titan',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 7.539
d1 = 2.618
ph0 = 2.168
ph1 = 4.9212
th0 = 0.318
th1 = 0.376
h:set_text('Iapetus')
par1:set_text('Natural satellite of Saturn  Radius: 734.5km  Mass: 1.8056×10^21kg  Mean orbital radius: 3.5601×10^6km  Orbital period: 6.853×10^6s (79.322 days)')
oapi.set_cameratarget(oapi.get_objhandle('Iapetus'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(25*RAD)
while t1 < 560 do
	tstep()
	stepr = (t1-540)/20
	step = math.sin(stepr*math.pi/2)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

d0 = 3.471
d1 = 9.597
ph0 = -1.005
ph1 = 0.164
th0 = 0.494
th1 = 0.534
h:set_text('Uranus')
par1:set_text('Radius: 25,362km  Mass: 8.681×10^25kg  Mean orbital radius: 2.877×10^9km (19.23AU)  Orbital period: 2.6515×10^9s (84.0205 years)')
oapi.set_cameratarget(oapi.get_objhandle('Uranus'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(20*RAD)
while t1 < 580 do
	tstep()
	step = (t1-560)/20
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

d0 = 15.844
d1 = 7.726
ph0 = -0.419
ph1 = -1.656
th0 = 0.671
th1 = -0.617
h:set_text('Miranda')
par1:set_text('Natural satellite of Uranus  Radius: 235.8km  Mass: 6.59×10^19kg  Mean orbital radius: 129,840km  Orbital period: 1.2212×10^5s (1.413 days)')
oapi.set_cameratarget(oapi.get_objhandle('Miranda'))
oapi.set_cameramode({mode='track',trackmode='global',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(10*RAD)
while t1 < 600 do
	tstep()
	step = (t1-580)/20
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
end

d0 = 29.963
d1 = 8.348
ph0 = -2.055
ph1 = 2.494
th0 = -0.765
th1 = -0.021
h:set_text('Ariel')
par1:set_text('Natural satellite of Uranus  Radius: 578.9km  Mass: 1.353×10^21kg  Mean orbital radius: 190,900km  Orbital period: 2.17728×10^5s (2.520 days)')
oapi.set_cameratarget(oapi.get_objhandle('Ariel'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(10*RAD)
while t1 < 620 do
	tstep()
	step = (t1-600)/20
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

d0 = 36.878
d1 = 23.763
ph0 = 2.927
ph1 = 4.247
th0 = -0.163
th1 = 0.622
h:set_text('Umbriel')
par1:set_text('Natural satellite of Uranus  Radius: 584.7km  Mass: 1.172×10^21kg  Mean orbital radius: 265,967km  Orbital period: 3.5804×10^5s (4.144 days)')
oapi.set_cameratarget(oapi.get_objhandle('Umbriel'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(5*RAD)
while t1 < 630 do
	tstep()
	step = (t1-620)/10
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = 172.67632
lng1 = 170.97385
lat0 = -9.65432
lat1 = -19.02012
alt0 = 7e6
alt1 = 2998518.37
ph0 = 4.65
ph1 = 5.07
th0 = -1.55
th1 = -1.50
h:set_text('Titania')
par1:set_text('Natural satellite of Uranus  Radius: 788.4km  Mass: 3.527×10^21kg  Mean orbital radius: 436,282km  Orbital period: 7.522×10^5s (8.706 days)')
oapi.set_cameratarget(oapi.get_objhandle('Titania'))
oapi.set_cameramode({mode='ground',ref='Titania',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(20*RAD)
while t1 < 640 do
	tstep()
	step = (t1-630)/10
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Titania',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = 169.76020
lng1 = 174.02587
lat0 = 4.93857
lat1 = 8.42862
alt0 = 6849000.00
alt1 = 6219112.51
ph0 = 1.59
ph1 = 1.29
th0 = -1.59
th1 = -1.56
h:set_text('Oberon')
par1:set_text('Natural satellite of Uranus  Radius: 761.4km  Mass: 3.014×10^21kg  Mean orbital radius: 583,416km  Orbital period: 1.163×10^6s (13.463 days)')
oapi.set_cameratarget(oapi.get_objhandle('Oberon'))
oapi.set_cameramode({mode='ground',ref='Oberon',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(10*RAD)
while t1 < 650 do
	tstep()
	step = (t1-640)/10
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Oberon',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

lng0 = -167.77946
lng1 = -180.00000
lat0 = -19.58675
lat1 = -19.58680
alt0 = 355200000.00
alt1 = 80000000.00
ph0 = -4.75
ph1 = -4.02
th0 = -1.58
th1 = -1.58
h:set_text('Neptune')
par1:set_text('Radius: 24,622km  Mass: 1.024×10^26kg  Mean orbital radius: 4.498×10^9km (30.07AU)  Orbital period: 5.1997×10^9s (164.8 years)')
oapi.set_cameratarget(oapi.get_objhandle('Neptune'))
oapi.set_cameramode({mode='ground',ref='Neptune',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(15*RAD)
while t1 < 670 do
	tstep()
	step = (t1-650)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Neptune',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

d0 = 5.538
d1 = 8.131
ph0 = 1.802
ph1 = 3.543
th0 = 0.988
th1 = 0.617
h:set_text('Triton')
par1:set_text('Natural satellite of Neptune  Radius: 1353.4km  Mass: 2.14×10^22kg  Mean orbital radius: 354,757km  Orbital period: -5.0776×10^5s (-5.877 days) - retrograde')
oapi.set_cameratarget(oapi.get_objhandle('Triton'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 680 do
	tstep()
	step = (t1-670)/10
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

d0 = 15.079
d1 = 8.133
ph0 = -3.059
ph1 = -2.101
th0 = -0.015
th1 = 0.001
h:set_text('Proteus')
par1:set_text('Natural satellite of Neptune  Radius: 210km  Mass: 4.4×10^19kg  Mean orbital radius: 117,647km  Orbital period: 9.6968×10^4s (1.12231477 days)')
oapi.set_cameratarget(oapi.get_objhandle('Proteus'))
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
oapi.set_cameraaperture(15*RAD)
while t1 < 690 do
	tstep()
	step = (t1-680)/10
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end

lng0 = 143.68625
lng1 = 169.51579
lat0 = -23.19917
lat1 = -18.34516
alt0 = 3653000.00
alt1 = 2942934.32
ph0 = -2.95
ph1 = -2.41
th0 = -1.58
th1 = -1.60
h:set_text('Nereid')
par1:set_text('Natural satellite of Neptune  Radius: 170km  Mean orbital radius: 5.5141×10^6km  Orbital period: 3.1115×10^7s (360.1362 days)')
oapi.set_cameratarget(oapi.get_objhandle('Nereid'))
oapi.set_cameramode({mode='ground',ref='Nereid',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(10*RAD)
while t1 < 700 do
	tstep()
	step = (t1-690)/10
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Nereid',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end

--[[
lng0 = -50.00000
lng1 = -18.39718
lat0 = 0.00000
lat1 = 12.44589
alt0 = 500000000.00
alt1 = 89120000.00
ph0 = 0.93
ph1 = 0.93
th0 = -1.57
th1 = -1.57
h:set_text('Earth')
par1:set_text("This concludes the journey through Orbiter's solar system.    Back to the home planet.")
oapi.set_cameratarget(oapi.get_objhandle('Earth'))
oapi.set_cameramode({mode='ground',ref='Earth',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(10*RAD)
while t1 < 720 do
	tstep()
	stepr = (t1-700)/20
	step = math.sin(stepr*math.pi/2)
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end
--]]

lng0 = 155.82910
lng1 = 151.00734
lat0 = -3.34807
lat1 = -3.14795
alt0 = 1434963.42
alt1 = 1435128.11
ph0 = 3.10
ph1 = 3.10
th0 = -1.00
th1 = -0.96
h:set_text('Earthrise')
par1:set_text("This concludes the journey through Orbiter's solar system.    Let's finish with a view towards the home planet.")
oapi.set_cameratarget(oapi.get_objhandle('Moon'))
oapi.set_cameramode({mode='ground',ref='Moon',lng=lng0, lat=lat0, alt=alt0, phi=ph0, tht=th0, alt_above_ground=0})
oapi.set_cameraaperture(3*RAD)
while t1 < 720 do
	tstep()
	stepr = (t1-700)/20
	step = math.sin(stepr*math.pi/2)
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	a  = alt0 + (alt1-alt0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Moon',lng=lg, lat=lt, alt=a, phi=ph, tht=th, alt_above_ground=0})
end
oapi.del_annotation(h)