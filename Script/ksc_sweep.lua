t1 = oapi.get_simtime()
t0 = t1
dt = 0

function tstep()
	proc.skip()
	t1 = oapi.get_simtime()
	dt = t1-t0
	t0 = t1
end

d0 = 2215.664
ph0 = 0.853
th0 = -0.361
d1 = 54.110
ph1 = -1.264
th1 = -0.233
oapi.set_cameramode({mode='track',trackmode='relative',reldist=d0,phi=ph0,tht=th0})
while t1 < 20 do
	tstep()
	step = math.sin(t1/20*3.1415/2)
	d = d0 + (d1-d0)*step
	ph = ph0 + (ph1-ph0)*step
	th = th0 + (th1-th0)*step
	oapi.set_cameramode({mode='track',trackmode='relative',reldist=d,phi=ph,tht=th})
end
