local PanelElement = require("PanelElement")

local PressureIndicator = Class(PanelElement)

function PressureIndicator:new (_subsys, blitsrc)
	PanelElement.new(self, _subsys:DG())
	self.subsys = _subsys
	self.bsrc = blitsrc
	self.btgt = nil
end

--------------------------------------------------------------

function PressureIndicator:ResetVC (hMesh)
	self.upt = 0.0
	self.btgt = oapi.get_texturehandle (self.subsys:DG().vcmesh_tpl, 14)
end

--------------------------------------------------------------

function PressureIndicator:Redraw ()
	if not self.btgt then return false end

	local t = oapi.get_simtime()
	if t < self.upt and t > self.upt-1.0 then return false end
	self.upt = t + 0.5

	local cbuf
	cbuf = self:ValStr (self.subsys:PExtHatch())
	self:BlitReadout (0, cbuf)
	cbuf = self:ValStr (self.subsys:PCabin())
	self:BlitReadout (1, cbuf)
	cbuf = self:ValStr (self.subsys:PAirlock())
	self:BlitReadout (2, cbuf)
	cbuf = self:ValStr (self.subsys:PExtLock())
	self:BlitReadout (3, cbuf)

	return false
end

--------------------------------------------------------------

function PressureIndicator:Redraw2D (surf)
	self.btgt = surf
	return self:Redraw()
end

--------------------------------------------------------------

function PressureIndicator:RedrawVC (hMesh, surf)
	return self:Redraw()
end

--------------------------------------------------------------

function PressureIndicator:ValStr (p)
	p = p * 1e-3 -- convert to kPa
	if p > 300.0 then
		return "-----"
	else
		return string.format("%5.1f", p)
	end
end

--------------------------------------------------------------

local lut_srcx = {
	['0'] = 0,
	['1'] = 8,
	['2'] = 16,
	['3'] = 24,
	['4'] = 32,
	['5'] = 40,
	['6'] = 48,
	['7'] = 56,
	['8'] = 64,
	['9'] = 72,
	['.'] = 10*8+2,
	['+'] = 11*8,
	['-'] = 12*8,
	['k'] = 13*8,
	['M'] = 14*8,
	['G'] = 15*8,
}

function PressureIndicator:BlitReadout (which, str)
	local tgtx = 65 + which*61
	local tgty = 17
	local srcx = 0
	local srcy = 0
	local w = 8;
	local h = 11;

	local len = #str
	for i = 1, len do
		local c = str:sub(i,i)
		local srcx = lut_srcx[c] or 16*8
		oapi.blt (self.btgt, self.bsrc, tgtx, tgty, srcx, srcy, w, h)
		tgtx = tgtx + (c == '.' and 4 or w)
	end
end

return PressureIndicator
