// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

HazeVS HazeTechVS(HZVERTEX vrt)
{
	// Zero output.
	HazeVS outVS = (HazeVS)0;

	float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), gVP);
	outVS.tex0  = vrt.tex0;
	outVS.color = vrt.color;
	return outVS;
}


// Horizon haze pixel-shader frg.tex0.y is the altitude. 0.0 = Horizon (ground level) 1.0 = top of atmosphere
//
float4 HazeTechPS(HazeVS frg) : COLOR
{
	return frg.color * tex2D(ClampS, frg.tex0);

	//return float4(frg.color.rgb, frg.color.a*frg.tex0.y*frg.tex0.y);
	//return float4(frg.color.rgb*(frg.tex0.y+0.30), frg.color.a*frg.tex0.y*frg.tex0.y);
}


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