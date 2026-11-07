// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// Metal port of Planet.fx (included by D3D9Client.metal)
// ==============================================================

struct TileVS
{
	float4 posH    [[position]];
	float2 tex0;
	float3 normalW;
	float3 toCamW;
	float3 posW;
	float4 aux;
	float4 diffuse;
	float4 atten;
	float4 insca;
};



vertex TileVS PlanetTechVS(TILEVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	TileVS outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	float3 nrmW = normalize((g.gW * float4(vrt.normalL, 0.0f)).xyz);

	outVS.posH = g.gVP * float4(posW, 1.0f);

	float3 tocam  = normalize(-posW);
	float3 sundir = g.gSun.Dir;

	float diff    = saturate(dot(-sundir, nrmW));
	float dotr    = max(dot(reflect(sundir, nrmW), tocam), 0.0f);
	float spec    = pow(diff,0.25f) * pow(dotr, g.gWater.specPower);
	float nigh    = 0.0f;
	float ambi    = 0.0f;

	outVS.tex0    = float2(vrt.tex0.x*g.gTexOff[0] + g.gTexOff[1], vrt.tex0.y*g.gTexOff[2] + g.gTexOff[3]);
	outVS.toCamW  = tocam;
	outVS.normalW = nrmW;
	outVS.posW    = g.gCameraPos*g.gRadius[2] + posW*g.gDistScale;

	LegacySunColor(g, outVS.diffuse, ambi, nigh, nrmW);

	outVS.aux     = float4(spec, diff, ambi, nigh);

	AtmosphericHaze(g, outVS.atten, outVS.insca, outVS.posH.z, posW);

	outVS.insca *= (outVS.diffuse+ambi);

	return outVS;
}



fragment float4 PlanetTechPS(TileVS frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]], texture2d<float> gTex1 [[texture(1)]])
{

	float4 diff  = frg.aux.g*(g.gMat.diffuse*frg.diffuse) + (g.gMat.ambient*frg.aux.b);
	float4 vSpe = frg.aux.r * (g.gWater.specular*frg.diffuse);
	float4 vEff = gTex1.sample(Planet1S, frg.tex0);

	if (g.gSpecMode==2) vSpe *= 1.0f - vEff.a;
	if (g.gSpecMode==0) vSpe = 0.0f;

	float3 cTex = gTex0.sample(Planet0S, frg.tex0).rgb;
	float3 color = diff.rgb * cTex.rgb + frg.aux.a*vEff.rgb + vSpe.rgb;

	return float4(color*frg.atten.rgb+g.gColor.rgb+frg.insca.rgb, 1.0f);
}



fragment float4 CloudTechPS(TileVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{

	float4 data  = (g.gMat.ambient*frg.aux.b);
	float4 color = gTex0.sample(Planet0S, frg.tex0);
	float  alpha = color.a;

	if (dot(frg.normalW, frg.toCamW)<0) {
		float4 diff = (min(1.0f,frg.aux.g*2.0f) * frg.diffuse) * g.gMat.diffuse + data;
		return float4(color.rgb*diff.rgb, alpha);
	}

	else {
		float4 diff = (min(1.0f,frg.aux.g*1.5f) * frg.diffuse) * g.gMat.diffuse + data;
		return float4(color.rgb*diff.rgb, alpha);
	}
}









// -----------------------------------------------------------------------------
// Cloud Shadow Techs
// -----------------------------------------------------------------------------


struct ShadowVS
{
	float4 posH    [[position]];
	float2 tex0;
	float4 atten;
};

vertex ShadowVS CloudShadowTechVS(TILEVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	ShadowVS outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	outVS.tex0  = float2(vrt.tex0.x*g.gTexOff[0] + g.gTexOff[1], vrt.tex0.y*g.gTexOff[2] + g.gTexOff[3]);

	float4 none;

	AtmosphericHaze(g, outVS.atten, none, outVS.posH.z, posW);

	return outVS;
}

fragment float4 CloudShadowPS(ShadowVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	return float4(0.0f, 0.0f, 0.0f, gTex0.sample(Planet0S, frg.tex0).a * frg.atten.b);
}





// @technique PlanetTech
// @pass P0 vs=PlanetTechVS ps=PlanetTechPS AlphaBlendEnable=false ZEnable=false ZWriteEnable=false

// @technique PlanetCloudTech
// @pass P0 vs=PlanetTechVS ps=CloudTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false

// @technique PlanetCloudShadowTech
// @pass P0 vs=CloudShadowTechVS ps=CloudShadowPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false
