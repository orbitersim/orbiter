// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

struct TileVS
{
	vec4 posH;     // POSITION0
	vec2 tex0;     // TEXCOORD0
	vec3 normalW;  // TEXCOORD1
	vec3 toCamW;   // TEXCOORD2  // Vector to the camera
	vec3 posW;     // TEXCOORD3  // World space vertex position
	vec4 aux;      // TEXCOORD4  // Specular, Diffuse, Twilight, Night Texture Intensity,
	vec4 diffuse;  // TEXCOORD5  // Sun light
	vec4 atten;    // COLOR0     // Attennuate incoming fragment color
	vec4 insca;    // COLOR1     // "Inscatter" Add to incoming fragment color
};



TileVS PlanetTechVS(TILEVERTEX vrt)
{
	// Zero output.
	TileVS outVS = TileVS(vec4(0), vec2(0), vec3(0), vec3(0), vec3(0), vec4(0), vec4(0), vec4(0), vec4(0));

	// Apply a mesh group transformation matrix
	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;
	vec3 nrmW = normalize((vec4(vrt.normalL, 0.0f) * gW).xyz);

	// Convert transformed vertex position into a "screen" space using a combined (World, View and Projection) Matrix
	outVS.posH = vec4(posW, 1.0f) * gVP;

	// A vector from the vertex to the camera
	vec3 tocam  = normalize(-posW);
	vec3 sundir = gSun.Dir;

	float diff    = clamp(dot(-sundir, nrmW), 0.0, 1.0);
	float dotr    = max(dot(reflect(sundir, nrmW), tocam), 0.0f);
	float spec    = pow(diff,0.25f) * pow(dotr, gWater.specPower);
	float nigh    = 0.0f;
	float ambi    = 0.0f;

	outVS.tex0    = vec2(vrt.tex0.x*gTexOff[0] + gTexOff[1], vrt.tex0.y*gTexOff[2] + gTexOff[3]);
	outVS.toCamW  = tocam;
	outVS.normalW = nrmW;
	outVS.posW    = gCameraPos*gRadius[2] + posW*gDistScale;

	LegacySunColor(outVS.diffuse, ambi, nigh, nrmW);

	outVS.aux     = vec4(spec, diff, ambi, nigh);

	AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

	outVS.insca *= (outVS.diffuse+ambi);

	return outVS;
}

#ifdef VS_PlanetTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 6) in float iElev;
layout(location = 0) out TileVS oVS;
void main() { oVS = PlanetTechVS(TILEVERTEX(iPosL, iNrmL, iTex0, iElev) VS_ARGS); gl_Position = oVS.posH; }
#endif



vec4 PlanetTechPS(TileVS frg)
{

	vec4 diff  = frg.aux.g*(gMat.diffuse*frg.diffuse) + (gMat.ambient*frg.aux.b);
	vec4 vSpe = frg.aux.r * (gWater.specular*frg.diffuse);
	vec4 vEff = texture(Planet1S, frg.tex0);

	if (gSpecMode==2) vSpe *= 1.0f - vEff.a;
	if (gSpecMode==0) vSpe = vec4(0);

	vec3 cTex = texture(Planet0S, frg.tex0).rgb;
	vec3 color = diff.rgb * cTex.rgb + frg.aux.a*vEff.rgb + vSpe.rgb;

	return vec4(color*frg.atten.rgb+gColor.rgb+frg.insca.rgb, 1.0f);
}



vec4 CloudTechPS(TileVS frg)
{

	vec4 data  = (gMat.ambient*frg.aux.b);
	vec4 color = texture(Planet0S, frg.tex0);
	float  alpha = color.a;

	if (dot(frg.normalW, frg.toCamW)<0) {    // Render cloud layer from below
		vec4 diff = (min(1,frg.aux.g*2) * frg.diffuse) * gMat.diffuse + data;
		return vec4(color.rgb*diff.rgb, alpha);
	}

	else { // Render cloud layer from above
		vec4 diff = (min(1,frg.aux.g*1.5) * frg.diffuse) * gMat.diffuse + data;
		return vec4(color.rgb*diff.rgb, alpha);
	}
}

#if defined(PS_PlanetTechPS) || defined(PS_CloudTechPS)
layout(location = 0) in TileVS frg;
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_PlanetTechPS
void main() { oColor = PlanetTechPS(frg PS_ARGS); }
#endif
#ifdef PS_CloudTechPS
void main() { oColor = CloudTechPS(frg PS_ARGS); }
#endif









// -----------------------------------------------------------------------------
// Cloud Shadow Techs
// -----------------------------------------------------------------------------


struct ShadowVS
{
	vec4 posH;     // POSITION0
	vec2 tex0;     // TEXCOORD0
	vec4 atten;    // TEXCOORD2
};

ShadowVS CloudShadowTechVS(TILEVERTEX vrt)
{
	// Zero output.
	ShadowVS outVS = ShadowVS(vec4(0), vec2(0), vec4(0));

	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	outVS.tex0  = vec2(vrt.tex0.x*gTexOff[0] + gTexOff[1], vrt.tex0.y*gTexOff[2] + gTexOff[3]);

	vec4 none;

	AtmosphericHaze(outVS.atten, none, outVS.posH.z, posW);

	return outVS;
}

#ifdef VS_CloudShadowTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 6) in float iElev;
layout(location = 0) out ShadowVS oVS;
void main() { oVS = CloudShadowTechVS(TILEVERTEX(iPosL, iNrmL, iTex0, iElev) VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 CloudShadowPS(ShadowVS frg)
{
	return vec4(0,0,0, texture(Planet0S, frg.tex0).a * frg.atten.b);
}

#ifdef PS_CloudShadowPS
layout(location = 0) in ShadowVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = CloudShadowPS(frg PS_ARGS); }
#endif





// This is used for high resolution base tiles ---------------------------------
//
technique PlanetTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 PlanetTechVS();
		pixelShader  = compile ps_3_0 PlanetTechPS();

		AlphaBlendEnable = false;
		ZEnable = false;
		ZWriteEnable = false;
	}
}

technique PlanetCloudTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 PlanetTechVS();
		pixelShader  = compile ps_3_0 CloudTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}

technique PlanetCloudShadowTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 CloudShadowTechVS();
		pixelShader  = compile ps_3_0 CloudShadowPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}
