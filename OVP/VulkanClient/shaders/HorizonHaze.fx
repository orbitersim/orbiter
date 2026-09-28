// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

HazeVS HazeTechVS(HZVERTEX vrt)
{
	// Zero output.
	HazeVS outVS = HazeVS(vec4(0), vec4(0), vec2(0));

	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	outVS.tex0  = vrt.tex0;
	outVS.color = vrt.color;
	return outVS;
}

#ifdef VS_HazeTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 3) in vec4 iColor;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out HazeVS oVS;
void main() { oVS = HazeTechVS(HZVERTEX(iPosL, iColor, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif


// Horizon haze pixel-shader frg.tex0.y is the altitude. 0.0 = Horizon (ground level) 1.0 = top of atmosphere
//
vec4 HazeTechPS(HazeVS frg)
{
	return frg.color * texture(ClampS, frg.tex0);

	//return float4(frg.color.rgb, frg.color.a*frg.tex0.y*frg.tex0.y);
	//return float4(frg.color.rgb*(frg.tex0.y+0.30), frg.color.a*frg.tex0.y*frg.tex0.y);
}

#ifdef PS_HazeTechPS
layout(location = 0) in HazeVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = HazeTechPS(frg PS_ARGS); }
#endif


technique HazeTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 HazeTechVS();
		pixelShader  = compile ps_3_0 HazeTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}
