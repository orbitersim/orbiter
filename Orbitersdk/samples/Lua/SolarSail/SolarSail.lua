-- Copyright (c) Martin Schweiger
-- Licensed under the MIT License

-- ==============================================================
--                 ORBITER MODULE: SolarSail
--                  Part of the ORBITER SDK
--
-- SolarSail.lua
-- Control module for SolarSail vessel class
-- ==============================================================

local SolarSail = VesselClass()

-- ==============================================================
-- Some vessel parameters
-- ==============================================================
local SAIL_RADIUS = 500.0
local GRP_sail1 = 0
local GRP_paddle1 = 4
local GRP_paddle2 = 5
local GRP_paddle3 = 6
local GRP_paddle4 = 7
local MAXNBHR = 6 -- max node neighbours

-- distance between two vertices
function Dst (v1, v2)
	local dx = v1.x - v2.x
	local dy = v1.y - v2.y
	local dz = v1.z - v2.z
	return math.sqrt (dx*dx + dy*dy + dz*dz)
end

function Nml (v1, v2, v3)
	local dx1 = v2.x - v1.x
	local dx2 = v3.x - v1.x
	local dy1 = v2.y - v1.y
	local dy2 = v3.y - v1.y
	local dz1 = v2.z - v1.z
	local dz2 = v3.z - v1.z

	return _V(dy1*dz2 - dy2*dz1, dz1*dx2 - dz2*dx1, dx1*dy2 - dx2*dy1)
end

function crossp (v1, v2)
	return _V(v1.y*v2.z - v2.y*v1.z, v1.z*v2.x - v2.z*v1.x, v1.x*v2.y - v2.x*v1.y)
end

-- --------------------------------------------------------------
-- Set up the dynamic elastic sail deformation code
-- --------------------------------------------------------------
function SolarSail:SetupElasticity (hMesh)
	local sail = oapi.mesh_group (hMesh, GRP_sail1)
	-- all sail segments have the same mesh structure, so segment 1 represents all 4
	local nvtx = #sail.Vtx/2 -- scan front side only
	local nidx = #sail.Idx/2 -- scan front side only
	local ntri = nidx/3
	local idx = sail.Idx
	local vtx = sail.Vtx
	-- generate node neighbour graph
	self.sail_vbuf = {}
	self.nbhr = {}
	for i = 1, nvtx do
		self.nbhr[i] = {}
		self.nbhr[i].nnd = 0
		self.nbhr[i].nd = {}
		self.nbhr[i].dst0 = {}
		self.nbhr[i].fix = (vtx[i].x == 0 or vtx[i].y == 0)
	end

	for i = 1, ntri do
		local tri_offset = (i-1)*3
		for j = 1, 3 do
			local nj = idx[tri_offset + j] + 1
			for k = 1, 3 do
				if j ~= k then
					local nk = idx[tri_offset + k] + 1
					local ignore = false
					for m = 1, self.nbhr[nj].nnd do
						if self.nbhr[nj].nd[m] == nk then -- already in neighbour list
							ignore = true
						end
					end
					if not ignore then
						if self.nbhr[nj].nnd == MAXNBHR then 
							oapi.dbg_out("Problems!")
						else
							self.nbhr[nj].nnd = self.nbhr[nj].nnd + 1
							self.nbhr[nj].nd[self.nbhr[nj].nnd] = nk
							self.nbhr[nj].dst0[self.nbhr[nj].nnd] = Dst (vtx[nj], vtx[nk])
						end
					end
				end
			end
		end
	end
	self.sail_nvtx = nvtx
	self.sail_ntri = ntri

	self.sail_idx = {}
	for i = 1, 4 do
		local mg = oapi.mesh_group (hMesh, i - 1)
		self.sail_idx[i] = mg.Idx
	end
end

-- --------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------
function SolarSail:new(fmodel)
	self.hMeshTpl = oapi.load_meshglobal ("SolarSail")
	oapi.set_meshproperty (self.hMeshTpl, MESHPROPERTY.MODULATEMATALPHA, 1)
	self:SetupElasticity (self.hMeshTpl)
	self.hMesh = nil
	self.mf = _V(0,0,0)
	self:DefineAnimations()
	self.paddle_rot = {}
	self.paddle_vis = {}
	self.sail_vtx = {}
	for i = 1, 4 do
		self.paddle_rot[i] = 0.5
		self.paddle_vis[i] = 0.5
		local sail = oapi.mesh_group (self.hMeshTpl, i - 1)
		self.sail_vtx[i] = sail.Vtx:copy()
	end

end

-- --------------------------------------------------------------
-- Define animation sequences for moving parts
-- --------------------------------------------------------------
function SolarSail:DefineAnimations ()
	-- Steering paddle animations
	local Paddle = {}
	Paddle[1] = MGROUP_ROTATE(0, GRP_paddle1, _V(0,0,0), _V(0,1,0), 180.0*RAD)
	Paddle[2] = MGROUP_ROTATE(0, GRP_paddle2, _V(0,0,0), _V(1,0,0), 180.0*RAD)
	Paddle[3] = MGROUP_ROTATE(0, GRP_paddle3, _V(0,0,0), _V(0,1,0), 180.0*RAD)
	Paddle[4] = MGROUP_ROTATE(0, GRP_paddle4, _V(0,0,0), _V(1,0,0), 180.0*RAD)

	self.anim_paddle = {}
	for i = 1, 4 do
		self.anim_paddle[i] = self:create_animation(0.5)
		self:add_animationcomponent(self.anim_paddle[i], 0, 1, Paddle[i])
	end
end

-- --------------------------------------------------------------
-- Update sail nodal displacements
-- --------------------------------------------------------------
local sailidx = 0
function SolarSail:UpdateSail (rpressure)
	-- A kind of poor man's finite element code. Should eventually be done properly!

	local elast = 1e-1
	local pscale = 1e3
	local nml = {}
	local nsd = {}

	sailidx = (sailidx+1) % 4

	local vtx = self.sail_vtx[sailidx + 1]
	local idx = self.sail_idx[sailidx + 1]

	local Finit = _V(0,0,rpressure.z*pscale)
	local V0 = _V(0,0,0)
	for i = 1, self.sail_nvtx do
		local F = Finit -- note - should be calculated for LOCAL normal
		local vi = vtx[i].pos
		local nb = self.nbhr[i]
		if nb.fix then
			self.sail_vbuf[i] = V0
		else
			local dv = {}
			for j = 1, nb.nnd do
				local vj = vtx[nb.nd[j]]
				dv.x = vj.x - vi.x
				dv.y = vj.y - vi.y
				dv.z = vj.z - vi.z
				local dst = vec.length(dv)
				if dst > nb.dst0[j] then -- is stretched
					F = vec.add(F, vec.mul(dv,(elast/nb.dst0[j])))
				end
			end
			self.sail_vbuf[i] = F
		end
	end

	for i = 1, self.sail_nvtx do
		local VTXfront = vtx[i]
		local VBUF = self.sail_vbuf[i]
		VTXfront.x = VTXfront.x + VBUF.x
		VTXfront.y = VTXfront.y + VBUF.y
		VTXfront.z = VTXfront.z + VBUF.z

		local VTXback = vtx[i+self.sail_nvtx]
		VTXback.x = VTXback.x + VBUF.x
		VTXback.y = VTXback.y + VBUF.y
		VTXback.z = VTXback.z + VBUF.z
	end

	-- calculate smooth normals - surely this could be done more efficiently!
	for i = 1, self.sail_ntri do
		local idx_offset = (i-1)*3+1
		local nm = vec.unit(Nml(vtx[1+idx[idx_offset]], vtx[1+idx[idx_offset+1]], vtx[1+idx[idx_offset+2]]))
		for j = 0,2 do
			local offset = 1+idx[idx_offset+j]
			nml[offset] = vec.add((nml[offset] or V0), nm)
			nsd[offset] = (nsd[offset] or 0) + 1
		end
	end

	for i = 1, self.sail_nvtx do
		local N = vec.div(nml[i], nsd[i])
		vtx[i].normal = N
		vtx[i+self.sail_nvtx].normal = vec.sub(V0, N)
	end

	local ges = {}
	ges.flags = GRPEDIT.VTXCRD + GRPEDIT.VTXNML
	ges.Vtx = vtx
	oapi.edit_meshgroup (self.hMesh, sailidx, ges)
end

-- --------------------------------------------------------------
-- Low-level steering: adjust the position of a paddle
-- --------------------------------------------------------------
function SolarSail:SetPaddle (p, pos)
	self.paddle_rot[p] = pos
end

-- ==============================================================
-- Overloaded callback functions
-- ==============================================================

-- --------------------------------------------------------------
-- Set the capabilities of the vessel class
-- --------------------------------------------------------------
function SolarSail:clbkSetClassCaps (cfg)
	-- physical specs
	self:set_size (SAIL_RADIUS*2.0)
	self:set_emptymass (100.0)
	self:set_cw ({z=0.3, zn=0.3, x=0.6, y=0.9})

	self:set_wingaspect (0.7);
	self:set_wingeffectiveness (2.5)
	self:set_crosssections (_V(10.5,15.0,5.8))
	self:set_rotdrag (_V(0.6,0.6,0.35))
	if self:get_flightmodel() >= 1 then
		self:set_pitchmomentscale (1e-4)
		self:set_yawmomentscale (1e-4)
	end
	self:set_pmi (_V(3e3,3e3,6e3))
	self:set_trimscale (0.05)
	self:set_cameraoffset (_V(0,0.8,0))
	self:set_dockparams (_V(0,1.3,-1), _V(0,1,0), _V(0,0,-1))
	self:set_touchdownpoints (_V(0,-1.5,2), _V(-1,-1.5,-1.5), _V(1,-1.5,-1.5))

	-- visual specs
	self:add_mesh (self.hMeshTpl)
end

-- --------------------------------------------------------------
-- Frame pre-update
-- --------------------------------------------------------------
function SolarSail:clbkPreStep (simt, simdt, mjd)
	-- calculate solar pressure on steering paddles
	local paddle_area = 7812.5
	local albedo = 2.0
	local ppos = {
		_V(0,-550,0), _V(-550,0,0), _V(0,550,0), _V(550,0,0)
	}
	local nml
	for i = 1, 4 do
		local phi = (self.paddle_rot[i]-0.5)*PI
		local sphi = math.sin(phi)
		local cphi = math.cos(phi)

		if i == 1 or i == 3 then
			nml = _V(-sphi,0,cphi);
		else
			nml = _V(0,sphi,cphi);
		end

		local f = vec.dotp (self.mf, nml)
		if f < 0 then
			nml = vec.sub(0,nml)
			f = -f
		end
		f = f * paddle_area*albedo
		self:add_force (vec.mul(nml,f), ppos[i])
	end
end

-- --------------------------------------------------------------
-- Frame update
-- --------------------------------------------------------------
function SolarSail:clbkPostStep (simt, simdt, mjd)
	if self.hMesh then
		self:UpdateSail (self.mf)
	end

	for i = 1, 4 do
		if self.paddle_vis[i] ~= self.paddle_rot[i] then
			self.paddle_vis[i] = self.paddle_rot[i]
			self:set_animation (self.anim_paddle[i], self.paddle_vis[i])
		end
	end
end

-- --------------------------------------------------------------
-- Implement effects of radiation pressure
-- --------------------------------------------------------------
function SolarSail:clbkGetRadiationForce (mflux)
	self.mf = mflux       -- store flux value
	local albedo = 2.0    -- fully reflective
	local area = SAIL_RADIUS*SAIL_RADIUS*PI

	-- The sail is oriented normal to the vessel z-axis.
	-- Therefore only the z-component of the radiation momentum flux contributes
	-- to change the sail's momentum (Fresnel reflection)
	local mom = mflux.z * albedo * area * 0
	local F = _V(0,0,mom)
	local pos = _V(0,0,0)        -- don't induce torque

	return F, pos
end

-- --------------------------------------------------------------
-- Create SolarSail visual
-- --------------------------------------------------------------
function SolarSail:clbkVisualCreated (vis, refcount)
	self.hMesh = self:get_devmesh (vis, 0)
end

-- --------------------------------------------------------------
-- Destroy SolarSail visual
-- --------------------------------------------------------------
function SolarSail:clbkVisualDestroyed (vis, refcount)
	self.hMesh = nil
end

register_vesselclass(SolarSail, true)
