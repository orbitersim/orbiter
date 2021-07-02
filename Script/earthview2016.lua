h = oapi.create_annotation()
t1 = oapi.get_simtime()
t0 = t1
tofs = 0
dt = 0
mjd0 = 57451.2987058334

function tstep()
	proc.skip()
	t1 = oapi.get_simtime()-tofs
	dt = t1-t0
	t0 = t1
end

oapi.set_simmjd(mjd0)
d1 = 1068066.609
d0 = 2.702
ph = 0.219
th = -0.091
d = d1
oapi.set_cameramode({mode='track',trackmode='global',reldist=d1,phi=ph,tht=th})
oapi.set_tacc(10000)
while t1 < 200000 do
	tstep()
	step = (d-d0)*dt/10000
	d = d-step
	oapi.set_cameramode({mode='track',trackmode='global',reldist=d,phi=ph,tht=th})
	-- oapi.set_tacc(t1/20*10000)
end
oapi.set_tacc(1)
oapi.set_simmjd(mjd0)
tofs = oapi.get_simtime()
t1 = 0
alt0 = 4218948.88
alt1 = 280714.82
lng0 = 114.93673
lng1 = 95.16855
lat0 = 29.82409
lat1 = 30.61197
ph0 = 1.75
ph1 = 2.98
th0 = -1.65
th1 = -0.74
oapi.set_cameramode({mode='ground',ref='Earth',lng=lng0,lat=lat0,alt=alt0,phi=ph0,tht=th0, alt_above_ground=0})

while t1 < 20 do
	tstep()
	step = (t1-0)/20
	al = alt0 + (alt1-alt0)*step
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg,lat=lt,alt=al,phi=ph,tht=th, alt_above_ground=0})
end

lng0 = 92.35555
lng1 = 79.02076
lat0 = 27.99852
lat1 = 30.64634
alt0 = 60869.08
alt1 = 63696.16
ph0 = 3.14
ph1 = 2.67
th0 = -0.59
th1 = -0.53
oapi.set_cameramode({mode='ground',ref='Earth',lng=lng0,lat=lat0,alt=alt0,phi=ph0,tht=th0, alt_above_ground=0})

while t1 < 40 do
	tstep()
	step = (t1-20)/20
	al = alt0 + (alt1-alt0)*step
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg,lat=lt,alt=al,phi=ph,tht=th, alt_above_ground=0})
end

oapi.set_simmjd(57451.621815)
tofs = oapi.get_simtime()
t1 = 0
oapi.set_cameramode({mode='ground',ref='Earth',lng=-11.67576,lat=20.74645,alt=30473.91,phi=0.93,tht=-0.68, alt_above_ground=0})
oapi.set_cameraaperture(RAD*20)
while t1 < 10 do
	tstep()
end

ph0 = -0.01
ph1 = -4.33
while t1 < 30 do
	tstep()
	step = (t1-10)/20
	ph = ph0 + (ph1-ph0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=8.96919,lat=46.39945,alt=5243.89,phi=ph,tht=-0.22, alt_above_ground=0})
end

ph0 = 0.363
ph1 = 0.375
th0 = 0.409
th1 = 0.345
while t1 < 50 do
	tstep()
	step = (t1-30)/20
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=1.008,phi=ph,tht=th})
end

lng0 = -82.35611
lng1 = -80.57625
lat0 = 31.05713
lat1 = 24.73577
al = 3056.42
ph = -1.33
th = -0.26
while t1 < 70 do
	tstep()
	step = (t1-50)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg,lat=lt,alt=al,phi=ph,tht=th, alt_above_ground=0})
end

oapi.set_simmjd(57451.705953)
tofs = oapi.get_simtime()
t1 = 0
lng0 = -106.76529
lng1 = -106.43868
lat0 = 33.05009
lat1 = 32.88831
al = 2300
ph0 = -0.67
ph1 = -2.42
th0 = -0.21
th1 = -0.29
while t1 < 20 do
	tstep()
	step = (t1-0)/20
	lg = lng0 + (lng1-lng0)*step
	lt = lat0 + (lat1-lat0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='ground',ref='Earth',lng=lg,lat=lt,alt=al,phi=ph,tht=th, alt_above_ground=0})
end