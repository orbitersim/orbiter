// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2016 Jarmo Nikkanen
// ==============================================================


struct TileMeshVS
{
	vec4 posH;     // POSITION0
	vec3 CamW;     // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
	vec3 nrmW;     // TEXCOORD2
	vec4 atten;    // COLOR0			// (Atmospheric haze) Attennuate incoming fragment color
	vec4 insca;    // COLOR1			// (Atmospheric haze) "Inscatter" Add to incoming fragment color
};

struct MeshVS
{
	vec4 posH;     // POSITION0
	vec3 CamW;     // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
	vec3 nrmW;     // TEXCOORD2
};

struct TileMeshNMVS
{
	vec4 posH;     // POSITION0
	vec3 camW;     // TEXCOORD0
	vec4 atten;    // TEXCOORD1
	vec4 insca;    // TEXCOORD2
	vec2 tex0;     // TEXCOORD3
	vec3 nrmT;     // TEXCOORD4
	vec3 tanT;     // TEXCOORD5

};

MeshVS TinyMeshTechVS(MESH_VERTEX vrt)
{
	// Zero output.
	MeshVS outVS = MeshVS(vec4(0), vec3(0), vec2(0), vec3(0));

	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;	// Apply world transformation matrix
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	vec3 nrmW = (vec4(vrt.nrmL, 0.0f) * gW).xyz;	// Apply world transformation matri
	outVS.nrmW  = normalize(nrmW);
	outVS.CamW  = -posW;
	outVS.tex0  = vrt.tex0.xy;

	return outVS;
}

#ifdef VS_TinyMeshTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 2) in vec3 iTanL;
layout(location = 5) in vec3 iTex0;
layout(location = 0) out MeshVS oVS;
void main() { oVS = TinyMeshTechVS(MESH_VERTEX(iPosL, iNrmL, iTanL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif


vec4 TinyMeshTechPS(MeshVS frg)
{
	return vec4(0,1,0,1);

	// Normalize input
	vec3 nrmW = normalize(frg.nrmW);
	vec3 CamW = normalize(frg.CamW);
	vec4 cSpec = gMtrl.specular;
	vec4 cTex = vec4(1);

	if (gTextured) {
		if (gNoColor) cTex.a = texture(WrapS, frg.tex0.xy).a;
		else cTex = texture(WrapS, frg.tex0.xy);
	}

	if (gFullyLit) return vec4(cTex.rgb*clamp(gMtrl.diffuse.rgb + gMtrl.emissive.rgb, 0.0, 1.0), cTex.a);

	cTex.a *= gMtrlAlpha;

	// Sunlight calculations. Saturate with cSpec.a to gain an ability to disable specular light
	float  d = clamp(-dot(gSun.Dir, nrmW), 0.0, 1.0);
	float  s = pow(clamp(dot(reflect(gSun.Dir, nrmW), CamW), 0.0, 1.0), cSpec.a) * clamp(cSpec.a, 0.0, 1.0);

	if (d == 0) s = 0;

	vec3 diff = gMtrl.diffuse.rgb * (d * clamp(gSun.Color, 0.0, 1.0)); // Compute total diffuse light
	diff += (gMtrl.ambient.rgb*gSun.Ambient) + (gMtrl.emissive.rgb);

	vec3 cTot = cSpec.rgb * (s * gSun.Color);	// Compute total specular light

	cTex.rgb *= clamp(diff, 0.0, 1.0);	// Lit the diffuse texture

#if defined(_GLASS)
	cTex.a = clamp(cTex.a + max(max(cTot.r, cTot.g), cTot.b), 0.0, 1.0);	// Re-compute output alpha for alpha blending stage
#endif

	cTex.rgb += cTot.rgb;											// Apply reflections to output color

	return cTex;
}



// ============================================================================
// Planet Rings Technique
// ============================================================================

vec4 RingTechPS(MeshVS frg)
{
	vec4 color = texture(RingS, frg.tex0);

	vec3 pp = gCameraPos*gRadius[2] - frg.CamW*gDistScale;

	float  da = dot(normalize(pp), gSun.Dir);
	float  r  = sqrt(dot(pp,pp) * (1.0-da*da));

	float sh  = max(0.05, smoothstep(gRadius[0], gRadius[1], r));

	if (da<0) sh = 1.0f;

	if ((dot(frg.nrmW, frg.CamW)*dot(frg.nrmW, gSun.Dir))>0) return vec4(color.rgb*0.35f*sh, color.a);
	return vec4(color.rgb*sh, color.a);
}

vec4 RingTech2PS(MeshVS frg)
{
	vec3 pp  = gCameraPos*gRadius[2] - frg.CamW*gDistScale;
	float  dpp = dot(pp,pp);
	float  len = sqrt(dpp);

	len = clamp(smoothstep(gTexOff.x, gTexOff.y, len), 0.0, 1.0);

	vec4 color = texture(RingS, vec2(len, 0.5));
	color.a = color.r*0.75;

	float  da = dot(normalize(pp), gSun.Dir);
	float  r  = sqrt(dpp*(1.0-da*da));

	float sh  = max(0.05, smoothstep(gRadius[0], gRadius[1], r));

	if (da<0) sh = 1.0f;

	color.rgb *= sh;

	if ((dot(frg.nrmW, frg.CamW)*dot(frg.nrmW, gSun.Dir))>0) return vec4(color.rgb*0.35f, color.a);
	return vec4(color.rgb, color.a);
}

#if defined(PS_TinyMeshTechPS) || defined(PS_RingTechPS) || defined(PS_RingTech2PS) || defined(PS_AxisTechPS)
layout(location = 0) in MeshVS frg;
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_TinyMeshTechPS
void main() { oColor = TinyMeshTechPS(frg PS_ARGS); }
#endif
#ifdef PS_RingTechPS
void main() { oColor = RingTechPS(frg PS_ARGS); }
#endif
#ifdef PS_RingTech2PS
void main() { oColor = RingTech2PS(frg PS_ARGS); }
#endif


// ============================================================================
// Base Tile Rendering Technique
// ============================================================================

TileMeshVS BaseTileVS(NTVERTEX vrt)
{
	// Null the output
	TileMeshVS outVS = TileMeshVS(vec4(0), vec3(0), vec2(0), vec3(0), vec4(0), vec4(0));

	vec3 posW  = (vec4(vrt.posL, 1.0f) * gW).xyz;
	outVS.posH   = vec4(posW, 1.0f) * gVP;
	outVS.nrmW   = (vec4(vrt.nrmL, 0.0f) * gW).xyz;
	outVS.tex0   = vrt.tex0;
	outVS.CamW   = -posW;

	// Atmospheric haze -------------------------------------------------------

	AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

	vec4 diffuse;
	float ambi, nigh;

	LegacySunColor(diffuse, ambi, nigh, outVS.nrmW);

	outVS.insca *= (diffuse+ambi);
	outVS.insca.a = nigh;

	return outVS;
}

#ifdef VS_BaseTileVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out TileMeshVS oVS;
void main() { oVS = BaseTileVS(NTVERTEX(iPosL, iNrmL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif


vec4 BaseTilePS(TileMeshVS frg)
{
	// Normalize input
	vec3 nrmW = normalize(frg.nrmW);
	vec3 CamW = normalize(frg.CamW);

	vec4 cTex = texture(ClampS, frg.tex0);

	vec3 r = reflect(gSun.Dir, nrmW);
	float  s = pow(clamp(dot(r, CamW), 0.0, 1.0), 20.0f) * (1.0f-cTex.a);
	float  d = clamp(dot(-gSun.Dir, nrmW), 0.0, 1.0);

	if (d<=0) s = 0;

	vec3 clr = cTex.rgb * clamp(d * gSun.Color + s * gSun.Color + gSun.Ambient, 0.0, 1.0);

	if (gNight) clr += texture(Tex1S, frg.tex0).rgb;

	return vec4(clr.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);
	//return float4(clr.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a*(1-frg.insca.a));	// Make basetiles transparent during night
}

#ifdef PS_BaseTilePS
layout(location = 0) in TileMeshVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = BaseTilePS(frg PS_ARGS); }
#endif


// ============================================================================
// Vessel Axis vector technique
// ============================================================================

MeshVS AxisTechVS(MESH_VERTEX vrt)
{
	// Zero output.
	MeshVS outVS = MeshVS(vec4(0), vec3(0), vec2(0), vec3(0));
	float  stretch = vrt.tex0.x * gMix;
	vec3 posX = vrt.posL + vec3(0.0, stretch, 0.0);
	vec3 posW = (vec4(posX, 1.0f) * gW).xyz;			// Apply world transformation matrix
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	vec3 nrmW = (vec4(vrt.nrmL, 0.0f) * gW).xyz;		// Apply world transformation matrix

	outVS.nrmW  = normalize(nrmW);
	outVS.CamW  = -posW;

	return outVS;
}

#ifdef VS_AxisTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 2) in vec3 iTanL;
layout(location = 5) in vec3 iTex0;
layout(location = 0) out MeshVS oVS;
void main() { oVS = AxisTechVS(MESH_VERTEX(iPosL, iNrmL, iTanL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif


vec4 AxisTechPS(MeshVS frg)
{
	vec3 nrmW = normalize(frg.nrmW);
	float  d = clamp(dot(-gSun.Dir, nrmW), 0.0, 1.0);
	vec3 clr = gColor.rgb * clamp(max(d,0) + 0.5, 0.0, 1.0);
	return vec4(clr, gColor.a);
}

#ifdef PS_AxisTechPS
void main() { oColor = AxisTechPS(frg PS_ARGS); }
#endif

technique AxisTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 AxisTechVS();
		pixelShader  = compile ps_3_0 AxisTechPS();

		AlphaBlendEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = true;
		ZWriteEnable = true;
	}
}



// ============================================================================
// Mesh Shadow Technique
// ============================================================================

ShadowTexVS ShadowMeshTechVS(POSTEX vrt)
{
	// Zero output.
	ShadowTexVS outVS = ShadowTexVS(vec4(0), vec2(0), vec3(0));
	vec3 posW = (vec4(vrt.posL.xyz, 1.0f) * gW).xyz;
	float alpha = dot(vrt.posL.xyz, gInScatter.xyz) + gInScatter.w;
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	outVS.tex0  = vec3(vrt.tex0.xy, alpha);
	outVS.dstW  = outVS.posH.zw;
	return outVS;
}

ShadowTexVS ShadowMeshTechExVS(POSTEX vrt)
{
	// Zero output.
	ShadowTexVS outVS = ShadowTexVS(vec4(0), vec2(0), vec3(0));
	float alpha = dot(vrt.posL.xyz, gColor.xyz) + gColor.w;
	vec3 posX = (vec4(vrt.posL.xyz, 1.0f) * gGrpT).xyz;
	vec3 posW = (vec4(posX, 1.0f) * gW).xyz;
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	outVS.tex0  = vec3(vrt.tex0.xy, alpha);
	outVS.dstW  = outVS.posH.zw;
	return outVS;
}

#if defined(VS_ShadowMeshTechVS) || defined(VS_ShadowMeshTechExVS) || defined(VS_ShadowMapOIT_VS)
layout(location = 0) in vec3 iPosL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out ShadowTexVS oVS;
#endif
#ifdef VS_ShadowMeshTechVS
void main() { oVS = ShadowMeshTechVS(POSTEX(iPosL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif
#ifdef VS_ShadowMeshTechExVS
void main() { oVS = ShadowMeshTechExVS(POSTEX(iPosL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif

#ifdef STAGE_PS
vec4 ShadowTechPS(ShadowTexVS frg)
{
	if (frg.tex0.b < 0) discard; // clip(-1)
	if (gOITEnable) {
		vec4 alpha = texture(WrapS, frg.tex0.xy);
		if (alpha.a < 0.5f) discard;
	}
	return vec4(0.0f, 0.0f, 0.0f, gMix);
}
#endif

#ifdef PS_ShadowTechPS
layout(location = 0) in ShadowTexVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = ShadowTechPS(frg PS_ARGS); }
#endif


// -----------------------------------------------------------------------------------
// Shadow Map rendering with plain geometry (without texture) 
//
BShadowVS ShadowMapVS(SHADOW_VERTEX vrt)
{
	// Zero output.
	BShadowVS outVS = BShadowVS(vec4(0), vec2(0), 0.0);
	vec3 posW = (vec4(vrt.posL.xyz, 1.0f) * gW).xyz;
	outVS.posH = vec4(posW, 1.0f) * gLVP;
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

#ifdef VS_ShadowMapVS
layout(location = 0) in vec4 iPosL;
layout(location = 0) out BShadowVS oVS;
void main() { oVS = ShadowMapVS(SHADOW_VERTEX(iPosL) VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 ShadowMapPS(BShadowVS frg)
{
	return vec4(1 - (frg.dstW.x / frg.dstW.y));
}

#ifdef PS_ShadowMapPS
layout(location = 0) in BShadowVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = ShadowMapPS(frg PS_ARGS); }
#endif


// -----------------------------------------------------------------------------------
// Shadow Map rendering with texture alpha included
//
ShadowTexVS ShadowMapOIT_VS(POSTEX vrt)
{
	// Zero output.
	ShadowTexVS outVS = ShadowTexVS(vec4(0), vec2(0), vec3(0));
	vec3 posW = (vec4(vrt.posL.xyz, 1.0f) * gW).xyz;
	outVS.posH = vec4(posW, 1.0f) * gLVP;
	outVS.tex0 = vec3(vrt.tex0.xy, 0);
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

#ifdef VS_ShadowMapOIT_VS
void main() { oVS = ShadowMapOIT_VS(POSTEX(iPosL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 ShadowMapOIT_PS(ShadowTexVS frg)
{
	if (gOITEnable) {
		float alpha = texture(WrapS, frg.tex0.xy).a;
		if (alpha < 0.5f) return vec4(1.0f);
	}
	return vec4(1 - (frg.dstW.x / frg.dstW.y));
}

#ifdef PS_ShadowMapOIT_PS
layout(location = 0) in ShadowTexVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = ShadowMapOIT_PS(frg PS_ARGS); }
#endif

// -----------------------------------------------------------------------------------

technique GeometryTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 ShadowMapVS();
		pixelShader = compile ps_3_0 ShadowMapPS();

		AlphaBlendEnable = false;
		ZEnable = true;
		ZWriteEnable = true;
		StencilEnable = false;
	}

	pass P1
	{
		vertexShader = compile vs_3_0 ShadowMapOIT_VS();
		pixelShader = compile ps_3_0 ShadowMapOIT_PS();

		AlphaBlendEnable = false;
		ZEnable = true;
		ZWriteEnable = true;
		StencilEnable = false;
	}
}

technique ShadowTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 ShadowMeshTechVS();
		pixelShader  = compile ps_3_0 ShadowTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;

		StencilEnable = true;
		StencilRef    = 1;
		StencilMask   = 1;
		StencilFunc   = NotEqual;
		StencilPass   = Replace;
	}

	pass P1
	{
		vertexShader = compile vs_3_0 ShadowMeshTechExVS();
		pixelShader  = compile ps_3_0 ShadowTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;

		StencilEnable = true;
		StencilRef    = 1;
		StencilMask   = 1;
		StencilFunc   = NotEqual;
		StencilPass   = Replace;
	}
}



// =============================================================================
// Mesh Bounding Box Technique
// =============================================================================

BShadowVS BoundingBoxVS(vec3 posL)	// posL : POSITION0
{
	// Zero output.
	BShadowVS outVS = BShadowVS(vec4(0), vec2(0), 0.0);
	vec3 pos;
	pos.x = gAttennuate.x * posL.x + gInScatter.x * (1-posL.x);
	pos.y = gAttennuate.y * posL.y + gInScatter.y * (1-posL.y);
	pos.z = gAttennuate.z * posL.z + gInScatter.z * (1-posL.z);

	vec3 posX = (vec4(pos, 1.0f) * gGrpT).xyz;		// Apply meshgroup specific transformation
	vec3 posW = (vec4(posX, 1.0f) * gW).xyz;			// Apply world transformation matrix
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	return outVS;
}

BShadowVS BoundingSphereVS(vec3 posL)	// posL : POSITION0
{
	// Zero output.
	BShadowVS outVS = BShadowVS(vec4(0), vec2(0), 0.0);
	vec3 posW = (vec4(posL, 1.0f) * gW).xyz;			// Apply world transformation matrix
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	return outVS;
}

#if defined(VS_BoundingBoxVS) || defined(VS_BoundingSphereVS)
layout(location = 0) in vec3 iPosL;
layout(location = 0) out BShadowVS oVS;
#endif
#ifdef VS_BoundingBoxVS
void main() { oVS = BoundingBoxVS(iPosL VS_ARGS); gl_Position = oVS.posH; }
#endif
#ifdef VS_BoundingSphereVS
void main() { oVS = BoundingSphereVS(iPosL VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 BoundingBoxPS(BShadowVS frg)
{
	return gColor;
}

#ifdef PS_BoundingBoxPS
layout(location = 0) in BShadowVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = BoundingBoxPS(frg PS_ARGS); }
#endif

technique TileBoxTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BoundingSphereVS();
		pixelShader  = compile ps_3_0 BoundingBoxPS();

		AlphaBlendEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = true;
		ZWriteEnable = true;
	}
}

technique BoundingBoxTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BoundingBoxVS();
		pixelShader  = compile ps_3_0 BoundingBoxPS();

		AlphaBlendEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = true;
		ZWriteEnable = true;
	}
}

technique BoundingSphereTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BoundingSphereVS();
		pixelShader  = compile ps_3_0 BoundingBoxPS();

		AlphaBlendEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = true;
		ZWriteEnable = true;
	}
}


technique BaseTileTech
{
	/*pass P0
	{
		vertexShader = compile VS_MOD BaseTileNMVS();
		pixelShader  = compile PS_MOD BaseTileNMPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
		CullMode = CCW;
	}*/

	pass P0
	{
		vertexShader = compile vs_3_0 BaseTileVS();
		pixelShader  = compile ps_3_0 BaseTilePS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
		CullMode = CCW;
	}
}

technique RingTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 TinyMeshTechVS();
		pixelShader  = compile ps_3_0 RingTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
		ZEnable = false;
		CullMode = NONE;
	}
}

technique RingTech2
{
	pass P0
	{
		vertexShader = compile vs_3_0 TinyMeshTechVS();
		pixelShader  = compile ps_3_0 RingTech2PS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
		ZEnable = false;
		CullMode = NONE;
	}
}

technique SimplifiedTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 TinyMeshTechVS();
		pixelShader = compile ps_3_0 TinyMeshTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
		ZEnable = true;
	}
}
