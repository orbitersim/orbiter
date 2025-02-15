-- Copyright (c) 2021 Jarmo Nikkanen
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
-- class CameraMFD: Port of D3D9 GenericCamera
-- ==============================================================

local CameraMFD = Class()

-- ==============================================================
-- new: called automatically when the MFD instance is created.
--        This is your one chance to save the vessel handle
--        and the MFD width and height
-- ==============================================================
function CameraMFD:new(mfd, w, h, vessel)
	self.hVessel = vessel
	self.w = w
	self.h = h
	self.font = oapi.create_font (w/20, true, "Arial", FONT.BOLD + FONT.ITALIC, 450)
	self.hTexture = oapi.load_texture("samples/Crosshairs.dds")

	-- Create 3D render target
	self.hRenderSrf = oapi.create_surface(w, h, OAPISURFACE.RENDER3D + OAPISURFACE.TEXTURE +
		                                    OAPISURFACE.RENDERTARGET + OAPISURFACE.NOMIPMAPS)

	-- Clear the surface
	oapi.clear_surface(self.hRenderSrf)	
	
	self.type = "DOCK"
	self.index = 0
	self.bParent = false
	self.bNightVis = false
	self.bCross = true
	self.fov = 30.0
	self.offset = -0.2

	self:SelectVessel(vessel, "DOCK")
end


function CameraMFD:destroy()
	oapi.release_font(self.font)

	-- Attention, Always delete the camera before the surface !!!
	if self.hCamera then oapi.delete_customcamera(self.hCamera) end
	if self.hRenderSrf then oapi.destroy_surface(self.hRenderSrf) end
	if self.hTexture then oapi.release_texture(self.hTexture) end
end

function CameraMFD:SelectVessel(hVes, type)
	local pos = _V(0, 0, 0)
	local dir = _V(1, 0, 0)
	local rot = _V(0, 1, 0)

	self.type = type

	if hVes ~= self.hVessel then
		-- New Vessel Selected
		self.offset = -0.2
		self.index = 0
		self.type = "DOCK"
	end

	self.hVessel = hVes

	local nDock = self.hVessel:get_dockcount()
	local nAtch = self.hVessel:get_attachmentcount(self.bParent)

	if nDock == 0 and nAtch == 0 then return end

	if self.type == "ATCH" and nAtch == 0 then self.type = "DOCK" end
	if self.type == "DOCK" and nDock == 0 then self.type = "ATCH" end

	if self.fov < 5.0 then self.fov = 5.0 end
	if self.fov > 70.0 then self.fov = 70.0 end

	-- Attachemnts
	if self.type == "ATCH" then

		if self.index < 0 then self.index = nAtch - 1 end
		if self.index >= nAtch then self.index = 0 end

		self.hAttach = self.hVessel:get_attachmenthandle(self.bParent, self.index)

		if self.hAttach then
			pos, dir, rot = self.hVessel:get_attachmentparams(self.hAttach)
			pos = vec.add(pos, vec.mul(dir, self.offset))
		else
			return
		end
	end

	-- Docking ports
	if self.type == "DOCK" then

		if self.index < 0 then self.index = nDock - 1 end
		if self.index >= nDock then self.index = 0 end

		self.hDock = self.hVessel:get_dockhandle(self.index)

		if self.hDock then
			pos, dir, rot = self.hVessel:get_dockparams(self.hDock)
			pos = vec.add(pos, vec.mul(dir, self.offset))
		else
			return
		end
	end

	-- Actual rendering of the camera view into hRenderSrf will occur when the client is ready for it and 
	-- a lagg of a few frames may occur depending about graphics/performance options.
	-- Update will continue until the camera is turned off via oapi.customcamera_onoff() or deleted via oapi.delete_customcamera()
	-- Camera orientation can be changed by calling this function again with an existing camera handle instead of nil.

	self.hCamera = oapi.setup_customcamera(self.hCamera, self.hVessel:get_handle(), pos, dir, rot, self.fov*PI / 180.0, self.hRenderSrf)

	-- Register camera overlay render proc to draw into the actual rendering surface
	--oapi.customcamera_overlay(self.hCamera, function(skp) self:DrawOverlay(skp) end)
end

function CameraMFD:NextAttachment()
	if self.pMask then
		local nAtch = self.hVessel:get_attachmentcount(self.bParent)
		for i = 1, nAtch - 1 do
			local h = self.hVessel:get_attachmenthandle(self.bParent, (self.index+i)%nAtch)
			if h then
				local attachmentId = self.hVessel:get_attachmentid(h)
				if attachmentId == self.pMask then
					self.index = i
					return
				end
			end
		end
	else
		self.index = self.index + 1
	end
end

function CameraMFD:PreviousAttachment()
	if self.pMask then
		local nAtch = self.hVessel:get_attachmentcount(self.bParent)
		for i = 1, nAtch - 1 do
			local h = self.hVessel:get_attachmenthandle(self.bParent, (self.index + nAtch - i)%nAtch)
			if h then
				local attachmentId = self.hVessel:get_attachmentid(h)
				if attachmentId == self.pMask then
					self.index = i
					return
				end
			end
		end
	else
		self.index = self.index - 1
	end
end

-- ==============================================================
-- buttonlabel : return the label for an MFD button
-- ==============================================================
function CameraMFD:buttonlabel(bt)
	-- The labels for the buttons used by our MFD mode
	local label = {"NA", "PA", "ND", "PD", "FWD", "BWD", "VES", "NV", "ZM+", "ZM-", "PAR", "CRS"}
	return label[bt+1]
end

-- ==============================================================
-- buttonmenu : return the menu entries for the MFD
-- ==============================================================
function CameraMFD:buttonmenu()
	local mnu = {
		{ l1 = "Next attachment", sel = '1' },
		{ l1 = "Prev attachment", sel = '2' },
		{ l1 = "Next dockport",   sel = '3' },
		{ l1 = "Prev dockport",   sel = '4' },
		{ l1 = "Move Forward",    sel = '5' },
		{ l1 = "Move Backwards",  sel = '6' },
		{ l1 = "Select Vessel",   sel = '7' },
		{ l1 = "Night Vision",    sel = '8' },
		{ l1 = "Zoom In",         sel = '9' },
		{ l1 = "Zoom Out",        sel = '0' },
		{ l1 = "Parent Mode",     sel = 'B' },
		{ l1 = "Crosshairs",      sel = 'C' }
	}

	return mnu, 12
end

function CameraMFD:update(skp)
	self:invalidate_display()
	
	-- Call to update attachments
	self:SelectVessel(self.hVessel, self.type)

	local nDock = self.hVessel:get_dockcount()
	local nAtch = self.hVessel:get_attachmentcount(self.bParent)

	local tbgh = 27			-- Text backgound height
	local edge = tbgh + 2	-- Minimum spacing between cross endpoints and MFD screen edge

	local W, H = self:get_size()
	local sr = _R(0, 0, W - 2, H - 3)
	
	skp:set_textalign(SKP.CENTER)

	if self.hRenderSrf then
		if nDock ~= 0 or nAtch ~= 0 then
			if self.bNightVis then
				skp:set_brightness(_COLOUR4(0.0, 4.0, 0.0, 1.0))
				skp:set_renderparam(PRM.GAMMA, _COLOUR4(0.5, 0.5, 0.5, 1.0))
				skp:set_renderparam(PRM.NOISE, _COLOUR4(0.0, 0.3, 0.0, 0.0))
			end

			-- Blit the camera view into the sketchpad.
			skp:copy_rect(self.hRenderSrf, sr, 1, 1)

			if self.bNightVis then
				skp:set_brightness()
				skp:set_renderparam(PRM.GAMMA)
				skp:set_renderparam(PRM.NOISE)
			end
		end


		-- Draw the cross-hairs
		if self.bCross then
			local rc = _V(W / 2, H / 2)

			local y = H / 2 - 2
			local x = W / 2 + 1

			local nseg = ((H / 2) - edge) / 16	 -- Number of segments
			local len = math.min(248, (nseg * 16) - 8) -- Length (start and end to a white segment)

			local ch = _R( 0, 0, len, 4 )
		
			skp:copy_rect(self.hTexture, ch, x - len, y)
			skp:copy_rect(self.hTexture, ch, x, y)

			skp:set_worldtransform2d(1.0, PI05, rc)

			skp:copy_rect(self.hTexture, ch, x - len, y)
			skp:copy_rect(self.hTexture, ch, x, y)

			-- Back to defaults
			skp:set_worldtransform2d()
		end
	else
		skp:text(W / 2, H / 2, "No Graphics API")
		return true
	end
	

	if not self.hCamera then
		skp:text(W / 2, H / 2, "Custom Cameras Disabled")
		return true
	end

	if nDock == 0 and nAtch == 0 then
		skp:text(W / 2, H / 2, "No Dock/Attachment points")
		return true
	end


	skp:set_textalign(SKP.LEFT, SKP.TOP)

	local atchId = self.type == "ATCH" and (" [ID:"..self.hVessel:get_attachmentid(self.hVessel:get_attachmenthandle(self.bParent, self.index)).."]")
										or ""
	local text = string.format("Viewing %s (%s%d)%s", self.hVessel:get_name(), self.type, self.index, atchId)
	
	skp:quick_brush(0xA0000000)
	skp:quick_pen(0)
	skp:rectangle(1, 1, W - 1, tbgh)
	skp:rectangle(1, H - tbgh, W - 1, H - 1)
	
	self:set_title(skp, text)

	text = string.format("[%s] FOV=%0.0f° Ofs=%2.2f[m]", self.bParent and "Parent" or "Child", self.fov*2.0, self.offset)

	skp:text(10, H - tbgh, text)
	
	return true
end

function CameraMFD:DrawOverlay(skp)
	-- Must identify the surface, no pre-filtering exists in a caller application
	-- This callback function may receive "render overlay" calls not intended for this CameraMFD
	if skp:get_surface() == self.hRenderSrf then
		local W, H = self:get_size()
		skp:quick_pen(0xFF0000FF, 3.0)
		skp:line(0, 0, W, H)
		skp:line(0, H, W, 0)
	end
end

-- Note: we use '.' and not ':' since self is explicitly passed last
function CameraMFD.cbEnter(str, self)
	local hObj = vessel.get_handle(str)

	if hObj then
		self:SelectVessel(vessel.get_interface(hObj), self.type)
		return true
	end

	return false
end

-- ==============================================================
-- consumekeybuffered : handle key events
-- ==============================================================
function CameraMFD:consumekeybuffered(key)
	local commands = {
		[OAPI_KEY.KEY1] = function() self:NextAttachment(); self:SelectVessel(self.hVessel, "ATCH") end,
		[OAPI_KEY.KEY2] = function() self:PreviousAttachment(); self:SelectVessel(self.hVessel, "ATCH") end,
		[OAPI_KEY.KEY3] = function() self.index = self.index + 1; self:SelectVessel(self.hVessel, "DOCK") end,
		[OAPI_KEY.KEY4] = function() self.index = self.index - 1; self:SelectVessel(self.hVessel, "DOCK") end,
		[OAPI_KEY.KEY5] = function() self.offset = self.offset + 0.1; self:SelectVessel(self.hVessel, self.type) end,
		[OAPI_KEY.KEY6] = function() self.offset = self.offset - 0.1; self:SelectVessel(self.hVessel, self.type) end,
		[OAPI_KEY.KEY7] = function() oapi.open_inputboxex("Enter vessel name", self.cbEnter, nil, "", 20, self) end,
		[OAPI_KEY.KEY8] = function() self.bNightVis = not self.bNightVis end,
		[OAPI_KEY.KEY9] = function() self.fov = self.fov - 5.0; self:SelectVessel(self.hVessel, self.type) end,
		[OAPI_KEY.KEY0] = function() self.fov = self.fov + 5.0; self:SelectVessel(self.hVessel, self.type) end,
		[OAPI_KEY.B] = function() self.bParent = not self.bParent; self:SelectVessel(self.hVessel, self.type) end,
		[OAPI_KEY.C] = function() self.bCross = not self.bCross end,
	}
	if commands[key] then
		return commands[key](self)
	end

	return false
end

-- ==============================================================
-- consumebutton : handle button events
-- ==============================================================
function CameraMFD:consumebutton(bt, event)
	local btkey = { OAPI_KEY.KEY1, OAPI_KEY.KEY2, OAPI_KEY.KEY3, OAPI_KEY.KEY4, OAPI_KEY.KEY5, OAPI_KEY.KEY6,
		OAPI_KEY.KEY7, OAPI_KEY.KEY8, OAPI_KEY.KEY9, OAPI_KEY.KEY0, OAPI_KEY.B, OAPI_KEY.C }

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then					
		return self:consumekeybuffered(btkey[bt+1])
	end

	return false
end

-- ==============================================================
-- Return a module created from CameraMFD
-- ==============================================================
return MFDModule(CameraMFD)
