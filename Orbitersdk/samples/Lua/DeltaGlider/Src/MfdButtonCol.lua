-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local MfdButtonGrp = require("MfdButtonGrp")
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local MfdButtonCol = Class(MfdButtonGrp)

local CHY = 22
local CHH = 12
local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height

local texw = PANEL2D_TEXW -- texture width
local texh = PANEL2D_TEXH -- texture height

local lblx = {{185,533},{748,1095}}
local lbly = {texh-467,texh-426,texh-385,texh-344,texh-303,texh-262}

local CHX = { -- MFD label font: character x-offsets
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,642--[[+]],0,661--[[-]],0,596--[[/]],
	492--[[0]],501--[[1]],510--[[2]],520--[[3]],529--[[4]],538--[[5]],547--[[6]],556--[[7]],565--[[8]],575--[[9]],627--[[:]],621--[[;]],602--[[<]],652--[[=]],612--[[>]],0,
	0,1--[[A]],11--[[B]],21--[[C]],32--[[D]],43--[[E]],54--[[F]],63--[[G]],75--[[H]],86--[[I]],92--[[J]],101--[[K]],111--[[L]],120--[[M]],132--[[N]],143--[[O]],
	155--[[P]],165--[[Q]],176--[[R]],187--[[S]],198--[[T]],207--[[U]],218--[[V]],229--[[W]],242--[[X]],253--[[Y]],263--[[Z]],0,0,0,0,0,
	0,273--[[a]],282--[[b]],291--[[c]],299--[[d]],309--[[e]],318--[[f]],324--[[g]],333--[[h]],342--[[i]],347--[[j]],353--[[k]],362--[[l]],367--[[m]],380--[[n]],389--[[o]],
	398--[[p]],407--[[q]],416--[[r]],423--[[s]],431--[[t]],438--[[u]],447--[[v]],456--[[w]],466--[[x]],475--[[y]],483--[[z]],0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
}


local CHW = { -- MFD label font: character widths
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,6--[[+]],0,4--[[-]],0,4--[[/]],
	6--[[0]],6--[[1]],6--[[2]],6--[[3]],6--[[4]],6--[[5]],6--[[6]],6--[[7]],6--[[8]],6--[[9]],2--[[:]],2--[[;]],6--[[<]],6--[[=]],6--[[>]],0,
	0,8--[[A]],7--[[B]],7--[[C]],7--[[D]],6--[[E]],6--[[F]],8--[[G]],7--[[H]],2--[[I]],5--[[J]],7--[[K]],6--[[L]],8--[[M]],7--[[N]],8--[[O]],
	6--[[P]],8--[[Q]],7--[[R]],7--[[S]],6--[[T]],7--[[U]],8--[[V]],11--[[W]],7--[[X]],8--[[Y]],7--[[Z]],0,0,0,0,0,
	0,6--[[a]],6--[[b]],6--[[c]],6--[[d]],6--[[e]],4--[[f]],6--[[g]],6--[[h]],2--[[i]],3--[[j]],5--[[k]],2--[[l]],8--[[m]],6--[[n]],6--[[o]],
	6--[[p]],6--[[q]],4--[[r]],6--[[s]],4--[[t]],6--[[u]],6--[[v]],9--[[w]],6--[[x]],6--[[y]],6--[[z]],0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
}


function MfdButtonCol:new (_subsys, side)
	MfdButtonGrp.new(self, _subsys, 6)
	self.sd = side
	self.xcnt = lblx[self.subsys:MfdId()+1][self.sd+1]
	self.curbtn = -1
end

--------------------------------------------------------------

function MfdButtonCol:Redraw2D (surf)
	for btn=1,self.nbtn do
		oapi.blt (surf, surf, self.xcnt-14, lbly[btn], 773, 22, 28, CHH) -- blank label
	end
	for btn = 1, 6 do
		local label = oapi.mfd_buttonlabel (self.subsys:MfdId(), btn+self.sd*6 - 1)
		if label then
			local len = #label
			local w = 0
			for i = 1, len do
				local c = label:sub(i,i)
				local ascii = string.byte(c)
				w = w + CHW[ascii + 1]
			end

			local x = self.xcnt-w/2
			for i = 1, len do
				local c = label:sub(i,i)
				local ascii = string.byte(c)
				local w = CHW[ascii + 1]
				if w > 0 then
					oapi.blt (surf, surf, x, lbly[btn], CHX[ascii + 1], CHY, w, CHH)
					x = x + w
				end
			end
		else
			return false
		end
	end
    return false
end

--------------------------------------------------------------

function MfdButtonCol:RedrawVC (hMesh, surf)
	if self.pending_action ~= 0 then -- process button push
		local grpid = {{GRP_VC.LMFD_LBUTTONS,GRP_VC.LMFD_RBUTTONS},
                       {GRP_VC.RMFD_LBUTTONS,GRP_VC.RMFD_RBUTTONS}}
		self:PushButtonVC (hMesh, grpid[self.subsys:MfdId()+1][self.sd+1], self.pending_btn, self.pending_action==1)
		self.pending_action = 0
		return false

	else -- process label change
		local CHX = { -- MFD label font: character x-offsets
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,333--[[+]],0,342--[[-]],0,359--[[ / ]],
			492--[[0]],501--[[1]],510--[[2]],520--[[3]],529--[[4]],538--[[5]],547--[[6]],556--[[7]],565--[[8]],575--[[9]],627--[[:]],621--[[;]],373--[[<]],652--[[=]],381--[[>]],0,
			0,81--[[A]],90--[[B]],100--[[C]],110--[[D]],120--[[E]],129--[[F]],138--[[G]],150--[[H]],159--[[I]],164--[[J]],173--[[K]],183--[[L]],192--[[M]],203--[[N]],213--[[O]],
			224--[[P]],233--[[Q]],243--[[R]],253--[[S]],263--[[T]],271--[[U]],281--[[V]],291--[[W]],305--[[X]],314--[[Y]],324--[[Z]],0,0,0,0,0,
			0,273--[[a]],282--[[b]],291--[[c]],299--[[d]],309--[[e]],318--[[f]],324--[[g]],333--[[h]],342--[[i]],347--[[j]],353--[[k]],362--[[l]],367--[[m]],380--[[n]],389--[[o]],
			398--[[p]],407--[[q]],416--[[r]],423--[[s]],431--[[t]],438--[[u]],447--[[v]],456--[[w]],466--[[x]],475--[[y]],483--[[z]],0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		}

		local CHW = { -- MFD label font: character widths
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,6--[[+]],0,6--[[-]],0,5--[[ / ]],
			6--[[0]],6--[[1]],6--[[2]],6--[[3]],6--[[4]],6--[[5]],6--[[6]],6--[[7]],6--[[8]],6--[[9]],2--[[:]],2--[[;]],8--[[<]],6--[[=]],8--[[>]],0,
			0,9--[[A]],9--[[B]],9--[[C]],9--[[D]],8--[[E]],8--[[F]],10--[[G]],8--[[H]],4--[[I]],7--[[J]],8--[[K]],7--[[L]],10--[[M]],9--[[N]],10--[[O]],
			8--[[P]],9--[[Q]],9--[[R]],9--[[S]],8--[[T]],9--[[U]],8--[[V]],12--[[W]],8--[[X]],9--[[Y]],8--[[Z]],0,0,0,0,0,
			0,6--[[a]],6--[[b]],6--[[c]],6--[[d]],6--[[e]],4--[[f]],6--[[g]],6--[[h]],2--[[i]],3--[[j]],5--[[k]],2--[[l]],8--[[m]],6--[[n]],6--[[o]],
			6--[[p]],6--[[q]],4--[[r]],6--[[s]],4--[[t]],6--[[u]],6--[[v]],9--[[w]],6--[[x]],6--[[y]],6--[[z]],0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		}
		local CHY = 1012

		surf = self.subsys:VcTex()

		local xcnt0 = 148
		local dx = 40
		local wlbl = 32
		local hlbl = 12
		local y = 14+self.sd*41+self.subsys:MfdId()*82

		for btn=0,5 do
			oapi.blt (surf, surf, xcnt0-wlbl/2+btn*dx, y, 0, 128, wlbl, hlbl) -- blank label
		end

		for btn=0,5 do
			local label = oapi.mfd_buttonlabel (self.subsys:MfdId(), btn+self.sd*6)
			if label then
				local len = #label
				local w = 0
				for i = 1, len do
					local c = label:sub(i,i)
					local ascii = string.byte(c)
					w =w + CHW[ascii + 1]
				end
				local xcnt = xcnt0 + btn*dx
				local x = xcnt-w/2
				for i = 1, len do
					local c = label:sub(i,i)
					local ascii = string.byte(c)
					local w = CHW[ascii + 1]
					if w > 0 then
						oapi.blt (surf, surf, x, y, CHX[ascii + 1], CHY, w, hlbl)
						x = x + w
					end
				end
			else
				return true
			end
		end
		return true
	end
end

--------------------------------------------------------------

function MfdButtonCol:ProcessMouse2D (event, mx, my)
	local process_btn = -1

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if my%41 < 18 then
			process_btn = my/41 + self.sd*6
			self.curbtn = process_btn
		end
	elseif self.curbtn >= 0 then
		process_btn = self.curbtn
		if bit.anyset(event, PANEL_MOUSE.LBUP) then
			self.curbtn = -1
		end
	end
	if process_btn >= 0 then
		oapi.process_mfdbutton (self.subsys:MfdId(), process_btn, event)
		return true
	end
	return false
end

--------------------------------------------------------------

function MfdButtonCol:ProcessMouseVC (event, p)
	local pushed = false
	local anim_btn = -1
	local process_btn = -1

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		local int, frac = math.modf(p.y*23.0/4.0)

		if frac < 0.75 then
			process_btn = int
			anim_btn = process_btn
			self.curbtn = anim_btn
			pushed = true
--			oapi.dbg_out(string.format("MfdButtonCol:ProcessMouseVC LBDOWN %d %d", process_btn, self.sd))
		end
	elseif self.curbtn >= 0 then
		process_btn = self.curbtn
		if bit.anyset(event, PANEL_MOUSE.LBUP) then
			anim_btn = self.curbtn
			self.curbtn = -1
			pushed = false
		end
	end
	if process_btn >= 0 then
		oapi.process_mfdbutton (self.subsys:MfdId(), process_btn + self.sd*6, event)
	end
	-- animate button pushes
	if anim_btn >= 0 then
		self.pending_btn = anim_btn
		self.pending_action = pushed and 1 or 2
		return true
	end
	return false
end

function MfdButtonCol:MfdId()
	return self.mfdid
end

function MfdButtonCol:VcTex()
	return self.vctex
end

return MfdButtonCol
