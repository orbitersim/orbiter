// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2016 Jarmo Nikkanen
// Metal port of Mesh.fx (included by D3D9Client.metal)
// ==============================================================


struct TileMeshVS
{
	float4 posH     [[position]];
	float3 CamW;
	float2 tex0;
	float3 nrmW;
	float4 atten;
	float4 insca;
};

struct MeshVS
{
	float4 posH     [[position]];
	float3 CamW;
	float2 tex0;
	float3 nrmW;
};

struct TileMeshNMVS
{
	float4 posH     [[position]];
	float3 camW;
	float4 atten;
	float4 insca;
	float2 tex0;
	float3 nrmT;
	float3 tanT;
};

vertex MeshVS TinyMeshTechVS(MESH_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	MeshVS outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	float3 nrmW = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;
	outVS.nrmW  = normalize(nrmW);
	outVS.CamW  = -posW;
	outVS.tex0  = vrt.tex0.xy;

	return outVS;
}


fragment float4 TinyMeshTechPS(MeshVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	return float4(0.0f, 1.0f, 0.0f, 1.0f);

	float3 nrmW = normalize(frg.nrmW);
	float3 CamW = normalize(frg.CamW);
	float4 cSpec = g.gMtrl.specular;
	float4 cTex = 1.0f;

	if (g.gTextured) {
		if (g.gNoColor) cTex.a = gTex0.sample(WrapS, frg.tex0.xy).a;
		else cTex = gTex0.sample(WrapS, frg.tex0.xy);
	}

	if (g.gFullyLit) return float4(cTex.rgb*saturate(g.gMtrl.diffuse.rgb + g.gMtrl.emissive.rgb), cTex.a);

	cTex.a *= g.gMtrlAlpha;

	float  d = saturate(-dot(g.gSun.Dir, nrmW));
	float  s = pow(saturate(dot(reflect(g.gSun.Dir, nrmW), CamW)), cSpec.a) * saturate(cSpec.a);

	if (d == 0) s = 0.0f;

	float3 diff = g.gMtrl.diffuse.rgb * (d * saturate(g.gSun.Color));
	diff += (g.gMtrl.ambient.rgb*g.gSun.Ambient) + (g.gMtrl.emissive.rgb);

	float3 cTot = cSpec.rgb * (s * g.gSun.Color);

	cTex.rgb *= saturate(diff);

#if defined(_GLASS)
	cTex.a = saturate(cTex.a + max(max(cTot.r, cTot.g), cTot.b));
#endif

	cTex.rgb += cTot.rgb;

	return cTex;
}



// ============================================================================
// Planet Rings Technique
// ============================================================================

fragment float4 RingTechPS(MeshVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 color = gTex0.sample(RingS, frg.tex0);

	float3 pp = g.gCameraPos*g.gRadius[2] - frg.CamW*g.gDistScale;

	float  da = dot(normalize(pp), g.gSun.Dir);
	float  r  = sqrt(dot(pp,pp) * (1.0f-da*da));

	float sh  = max(0.05f, smoothstep(g.gRadius[0], g.gRadius[1], r));

	if (da<0) sh = 1.0f;

	if ((dot(frg.nrmW, frg.CamW)*dot(frg.nrmW, g.gSun.Dir))>0) return float4(color.rgb*0.35f*sh, color.a);
	return float4(color.rgb*sh, color.a);
}

fragment float4 RingTech2PS(MeshVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float3 pp  = g.gCameraPos*g.gRadius[2] - frg.CamW*g.gDistScale;
	float  dpp = dot(pp,pp);
	float  len = sqrt(dpp);

	len = saturate(smoothstep(g.gTexOff.x, g.gTexOff.y, len));

	float4 color = gTex0.sample(RingS, float2(len, 0.5f));
	color.a = color.r*0.75f;

	float  da = dot(normalize(pp), g.gSun.Dir);
	float  r  = sqrt(dpp*(1.0f-da*da));

	float sh  = max(0.05f, smoothstep(g.gRadius[0], g.gRadius[1], r));

	if (da<0) sh = 1.0f;

	color.rgb *= sh;

	if ((dot(frg.nrmW, frg.CamW)*dot(frg.nrmW, g.gSun.Dir))>0) return float4(color.rgb*0.35f, color.a);
	return float4(color.rgb, color.a);
}


// ============================================================================
// Base Tile Rendering Technique
// ============================================================================

vertex TileMeshVS BaseTileVS(NTVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	TileMeshVS outVS = {};

	float3 posW  = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH   = g.gVP * float4(posW, 1.0f);
	outVS.nrmW   = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;
	outVS.tex0   = vrt.tex0;
	outVS.CamW   = -posW;

	AtmosphericHaze(g, outVS.atten, outVS.insca, outVS.posH.z, posW);

	float4 diffuse;
	float ambi, nigh;

	LegacySunColor(g, diffuse, ambi, nigh, outVS.nrmW);

	outVS.insca *= (diffuse+ambi);
	outVS.insca.a = nigh;

	return outVS;
}


fragment float4 BaseTilePS(TileMeshVS frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]], texture2d<float> gTex1 [[texture(1)]])
{
	float3 nrmW = normalize(frg.nrmW);
	float3 CamW = normalize(frg.CamW);

	float4 cTex = gTex0.sample(ClampS, frg.tex0);

	float3 r = reflect(g.gSun.Dir, nrmW);
	float  s = pow(saturate(dot(r, CamW)), 20.0f) * (1.0f-cTex.a);
	float  d = saturate(dot(-g.gSun.Dir, nrmW));

	if (d<=0) s = 0.0f;

	float3 clr = cTex.rgb * saturate(d * g.gSun.Color + s * g.gSun.Color + g.gSun.Ambient);

	if (g.gNight) clr += gTex1.sample(Tex1S, frg.tex0).rgb;

	return float4(clr.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);
}


// ============================================================================
// Vessel Axis vector technique
// ============================================================================

vertex MeshVS AxisTechVS(MESH_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	MeshVS outVS = {};
	float  stretch = vrt.tex0.x * g.gMix;
	float3 posX = vrt.posL + float3(0.0f, stretch, 0.0f);
	float3 posW = (g.gW * float4(posX, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	float3 nrmW = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;

	outVS.nrmW  = normalize(nrmW);
	outVS.CamW  = -posW;

	return outVS;
}


fragment float4 AxisTechPS(MeshVS frg [[stage_in]], constant Globals &g [[buffer(0)]])
{
	float3 nrmW = normalize(frg.nrmW);
	float  d = saturate(dot(-g.gSun.Dir, nrmW));
	float3 clr = g.gColor.rgb * saturate(max(d,0.0f) + 0.5f);
	return float4(clr, g.gColor.a);
}

// @technique AxisTech
// @pass P0 vs=AxisTechVS ps=AxisTechPS AlphaBlendEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=true ZWriteEnable=true



// ============================================================================
// Mesh Shadow Technique
// ============================================================================

vertex ShadowTexVS ShadowMeshTechVS(POSTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	ShadowTexVS outVS = {};
	float3 posW = (g.gW * float4(vrt.posL.xyz, 1.0f)).xyz;
	float alpha = dot(vrt.posL.xyz, g.gInScatter.xyz) + g.gInScatter.w;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	outVS.tex0  = float3(vrt.tex0.xy, alpha);
	outVS.dstW  = outVS.posH.zw;
	return outVS;
}

vertex ShadowTexVS ShadowMeshTechExVS(POSTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	ShadowTexVS outVS = {};
	float alpha = dot(vrt.posL.xyz, g.gColor.xyz) + g.gColor.w;
	float3 posX = (g.gGrpT * float4(vrt.posL.xyz, 1.0f)).xyz;
	float3 posW = (g.gW * float4(posX, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	outVS.tex0  = float3(vrt.tex0.xy, alpha);
	outVS.dstW  = outVS.posH.zw;
	return outVS;
}

fragment float4 ShadowTechPS(ShadowTexVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	if (frg.tex0.b < 0) discard_fragment();
	if (g.gOITEnable) {
		float4 alpha = gTex0.sample(WrapS, frg.tex0.xy);
		if (alpha.a < 0.5f) discard_fragment();
	}
	return float4(0.0f, 0.0f, 0.0f, g.gMix);
}


// -----------------------------------------------------------------------------------
// Shadow Map rendering with plain geometry (without texture)
//
vertex BShadowVS ShadowMapVS(SHADOW_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	BShadowVS outVS = {};
	float3 posW = (g.gW * float4(vrt.posL.xyz, 1.0f)).xyz;
	outVS.posH = g.gLVP * float4(posW, 1.0f);
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

fragment float4 ShadowMapPS(BShadowVS frg [[stage_in]], constant Globals &g [[buffer(0)]])
{
	return float4(1.0f - (frg.dstW.x / frg.dstW.y));
}


// -----------------------------------------------------------------------------------
// Shadow Map rendering with texture alpha included
//
vertex ShadowTexVS ShadowMapOIT_VS(POSTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	ShadowTexVS outVS = {};
	float3 posW = (g.gW * float4(vrt.posL.xyz, 1.0f)).xyz;
	outVS.posH = g.gLVP * float4(posW, 1.0f);
	outVS.tex0 = float3(vrt.tex0.xy, 0.0f);
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

fragment float4 ShadowMapOIT_PS(ShadowTexVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	if (g.gOITEnable) {
		float alpha = gTex0.sample(WrapS, frg.tex0.xy).a;
		if (alpha < 0.5f) return float4(1.0f);
	}
	return float4(1.0f - (frg.dstW.x / frg.dstW.y));
}

// -----------------------------------------------------------------------------------

// @technique GeometryTech
// @pass P0 vs=ShadowMapVS ps=ShadowMapPS AlphaBlendEnable=false ZEnable=true ZWriteEnable=true StencilEnable=false
// @pass P1 vs=ShadowMapOIT_VS ps=ShadowMapOIT_PS AlphaBlendEnable=false ZEnable=true ZWriteEnable=true StencilEnable=false

// @technique ShadowTech
// @pass P0 vs=ShadowMeshTechVS ps=ShadowTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false StencilEnable=true StencilRef=1 StencilMask=1 StencilFunc=NotEqual StencilPass=Replace
// @pass P1 vs=ShadowMeshTechExVS ps=ShadowTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false StencilEnable=true StencilRef=1 StencilMask=1 StencilFunc=NotEqual StencilPass=Replace



// =============================================================================
// Mesh Bounding Box Technique
// =============================================================================

vertex BShadowVS BoundingBoxVS(POSONLY vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	BShadowVS outVS = {};
	float3 posL = vrt.posL;
	float3 pos;
	pos.x = g.gAttennuate.x * posL.x + g.gInScatter.x * (1.0f-posL.x);
	pos.y = g.gAttennuate.y * posL.y + g.gInScatter.y * (1.0f-posL.y);
	pos.z = g.gAttennuate.z * posL.z + g.gInScatter.z * (1.0f-posL.z);

	float3 posX = (g.gGrpT * float4(pos, 1.0f)).xyz;
	float3 posW = (g.gW * float4(posX, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	return outVS;
}

vertex BShadowVS BoundingSphereVS(POSONLY vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	BShadowVS outVS = {};
	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	return outVS;
}

fragment float4 BoundingBoxPS(BShadowVS frg [[stage_in]], constant Globals &g [[buffer(0)]])
{
	return g.gColor;
}

// @technique TileBoxTech
// @pass P0 vs=BoundingSphereVS ps=BoundingBoxPS AlphaBlendEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=true ZWriteEnable=true

// @technique BoundingBoxTech
// @pass P0 vs=BoundingBoxVS ps=BoundingBoxPS AlphaBlendEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=true ZWriteEnable=true

// @technique BoundingSphereTech
// @pass P0 vs=BoundingSphereVS ps=BoundingBoxPS AlphaBlendEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=true ZWriteEnable=true

// @technique BaseTileTech
// @pass P0 vs=BaseTileVS ps=BaseTilePS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false CullMode=CCW

// @technique RingTech
// @pass P0 vs=TinyMeshTechVS ps=RingTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true ZEnable=false CullMode=None

// @technique RingTech2
// @pass P0 vs=TinyMeshTechVS ps=RingTech2PS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true ZEnable=false CullMode=None

// @technique SimplifiedTech
// @pass P0 vs=TinyMeshTechVS ps=TinyMeshTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true ZEnable=true
