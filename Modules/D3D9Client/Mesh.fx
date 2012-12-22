

// -------------------------------------------------------------------------------------------------------------
// Vertex shader implementations with per vertex fog
// -------------------------------------------------------------------------------------------------------------

struct AdvancedVS
{
    float4 posH     : POSITION0;
    float3 CamW     : TEXCOORD0;     
    half3  nrmW     : TEXCOORD1;
    half4  diffuse  : COLOR0;           // (Local Light) Diffuse color
    half4  spec     : COLOR1;           // (Local Light) Specular color
    half4  atten    : TEXCOORD2;        // (Atmospheric haze) Attennuate incoming fragment color
    half4  insca    : TEXCOORD3;        // (Atmospheric haze) "Inscatter" Add to incoming fragment color
    half2  tex0     : TEXCOORD4;
};

struct TileMeshVS
{
    float4 posH     : POSITION0;
    float3 CamW     : TEXCOORD0;
    half2  tex0     : TEXCOORD1;
    half3  nrmW     : TEXCOORD2;
    half4  atten    : COLOR0;           // (Atmospheric haze) Attennuate incoming fragment color
    half4  insca    : COLOR1;           // (Atmospheric haze) "Inscatter" Add to incoming fragment color
};

struct MeshVS
{
    float4 posH     : POSITION0;
    float3 CamW     : TEXCOORD0;
    half2  tex0     : TEXCOORD1;
    half3  nrmW     : TEXCOORD2;
};


#include "NormalMap.fx"


AdvancedVS MeshTechVS(MESH_VERTEX vrt)
{
    AdvancedVS outVS = (AdvancedVS)0;

    float3 posX = mul(float4(vrt.posL, 1.0f), gGrpT).xyz;       // Apply meshgroup specific transformation
    float3 posW = mul(float4(posX, 1.0f), gW).xyz;              // Apply world transformation matrix
    float3 nrmX = mul(float4(vrt.nrmL.xyz, 0.0f), gGrpT).xyz;
    float3 nrmW = mul(float4(nrmX, 0.0f), gW).xyz;

    nrmW = normalize(nrmW);

	// A vector from the vertex to the camera
	outVS.CamW  = -posW;
	outVS.tex0  = vrt.tex0;
    outVS.nrmW  = nrmW;
	outVS.posH  = mul(float4(posW, 1.0f), gVP);

    half4 locW;
    LocalVertexLight(outVS.diffuse, outVS.spec, locW, nrmW, posW);
   
    // Atmospheric haze --------------------------------------------------------

    AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

    outVS.insca *= (gSun.diffuse+gSun.ambient);

    return outVS;
}



float4 MeshTechPS(AdvancedVS frg) : COLOR
{
	// Normalize input
	float3 CamW = normalize(frg.CamW);
    float3 nrmW = normalize(frg.nrmW);
    half4 cTex = 1;
    half4 cSpe = float4(gMat.specular.rgb, gMat.specPower);

    if (gTextured) {
        cTex = tex2D(WrapS, frg.tex0);
        if (gModAlpha) cTex.a *= gMat.diffuse.a;	
    }
    else cTex.a = gMat.diffuse.a;
   
    if (gFullyLit) {
		if (gDebugHL) cTex.rgb = cTex.rgb*0.5 + gColor.rgb;
		return float4(cTex.rgb*gMat.diffuse.rgb, cTex.a);
    }

	if (gUseSpec) {
		cSpe = tex2D(SpecS, frg.tex0);
		cSpe.a *= 80.0f;
	}

    float3 r = reflect(gSun.direction, nrmW);
    float  d = max(0,dot(-gSun.direction, nrmW));
	float  s = pow(max(dot(r, CamW), 0.0f), cSpe.a); 

    if (cSpe.a<2.0 || d<=0) s = 0;

    half3 diff = gMat.diffuse.rgb  * (frg.diffuse.rgb + d * gSun.diffuse.rgb) + (gMat.ambient.rgb*gSun.ambient.rgb) + (gMat.emissive.rgb);
	half3 spec = cSpe.rgb * (frg.spec.rgb + s * gSun.specular.rgb);

	if (gUseEmis) diff += tex2D(EmisS, frg.tex0).rgb;

    // -------------------------------------------------------------------------
	half3 color  = cTex.rgb * saturate(diff) + saturate(spec);
    //float3 color  = dayTex.rgb * diff + spec;
    //float3 color = 1.0f - exp(-1.0f*(dayTex.rgb*diff+spec));
    // -------------------------------------------------------------------------

    if (gNight && gTextured) color.rgb += tex2D(NightS, frg.tex0).rgb; 
    
    if (gDebugHL) color = color*0.5 + gColor.rgb;

    return float4(color.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);
}

MeshVS SimpleMeshTechVS(MESH_VERTEX vrt)
{
    // Zero output.
	MeshVS outVS = (MeshVS)0;
	
	float3 posX = mul(float4(vrt.posL, 1.0f), gGrpT).xyz;       // Apply meshgroup specific transformation
    float3 posW = mul(float4(posX, 1.0f), gW).xyz;              // Apply world transformation matrix
    outVS.posH  = mul(float4(posW, 1.0f), gVP);
   
    float3 nrmX = mul(float4(vrt.nrmL.xyz, 0.0f), gGrpT).xyz;       // Apply meshgroup specific transformation
    float3 nrmW = mul(float4(posX, 0.0f), gW).xyz;              // Apply world transformation matrix

    outVS.nrmW  = normalize(nrmW);
    outVS.CamW  = -posW;
    outVS.tex0  = vrt.tex0;

    return outVS;
}


MeshVS TinyMeshTechVS(MESH_VERTEX vrt)
{
    // Zero output.
	MeshVS outVS = (MeshVS)0;
	
    float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;              // Apply world transformation matrix
    outVS.posH  = mul(float4(posW, 1.0f), gVP);
    float3 nrmW = mul(float4(vrt.nrmL.xyz, 0.0f), gW).xyz;          // Apply world transformation matrix
    outVS.nrmW  = normalize(nrmW);
    outVS.CamW  = -posW;
    outVS.tex0  = vrt.tex0;

    return outVS;
}



float4 VCTechPS(MeshVS frg) : COLOR
{
    // Normalize input
	float3 nrmW = normalize(frg.nrmW);
	float3 CamW = normalize(frg.CamW);
    half4 cTex = 1;

    if (gTextured) {
        cTex = tex2D(WrapS, frg.tex0);
    }

    if (gModAlpha || !gTextured) cTex.a *= gMat.diffuse.a;	
    if (gFullyLit) {
		if (gDebugHL) cTex.rgb = cTex.rgb*0.5 + gColor.rgb;
		return float4(cTex.rgb*saturate(gMat.diffuse.rgb+gMat.emissive.rgb), cTex.a);
	}
   
    float3 r = reflect(gSun.direction, nrmW);
    float  d = max(0,dot(-gSun.direction, nrmW));
	float  s = pow(max(dot(r, CamW), 0.0f), gMat.specPower); 

    if (gMat.specPower<2.0 || d<=0) s = 0;

    half3 diff = gMat.diffuse.rgb  * (d * gSun.diffuse.rgb) + (gMat.ambient.rgb*gSun.ambient.rgb) + (gMat.emissive.rgb);
    half3 spec = gMat.specular.rgb * (s * gSun.specular.rgb);
    half3 colr = cTex.rgb * saturate(diff) + saturate(spec);
    
    if (gDebugHL) colr = colr*0.5 + gColor.rgb;

    return float4(colr.rgb, cTex.a);
}


float4 HUDTechPS(MeshVS frg) : COLOR
{
    return tex2D(SimpleS, frg.tex0);
}


float4 MFDTechPS(MeshVS frg) : COLOR
{
	// Normalize input
    
	float3 nrmW = normalize(frg.nrmW);
	float3 CamW = normalize(frg.CamW);
	
	float3 r = reflect(gSun.direction, nrmW);
	float  s = 0.0f;

    if (gMat.specPower>2.0) s = pow(max(dot(r, CamW), 0.0f), gMat.specPower);

    float4 color = tex2D(MFDSamp, frg.tex0) + saturate(s * gMat.specular);
	
    return float4(color.rgb, 1.0f);
}



// =============================================================================
// Planet Rings Technique
// =============================================================================

float4 RingTechPS(MeshVS frg) : COLOR
{
    float4 color = tex2D(RingS, frg.tex0);
    
    float3 pp = gCameraPos*gRadius[2] - frg.CamW*gDistScale;
    
    float  da = dot(normalize(pp), gSun.direction);
    float  r  = sqrt(dot(pp,pp) * (1.0-da*da));
    
    float sh  = max(0.05, smoothstep(gRadius[0], gRadius[1], r));
    
    if (da<0) sh = 1.0f;
    
    if (dot(frg.nrmW, frg.CamW)>0) return float4(color.rgb*0.35f*sh, color.a);
    return float4(color.rgb*sh, color.a);
}


// =============================================================================
// Base Tile Rendering Technique
// =============================================================================

TileMeshVS BaseTileVS(NTVERTEX vrt)
{
    // Null the output
	TileMeshVS outVS = (TileMeshVS)0;

	float3 posW  = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posH   = mul(float4(posW, 1.0f), gVP);
    outVS.nrmW   = mul(float4(vrt.nrmL, 0.0f), gW).xyz;
	outVS.tex0   = vrt.tex0;
    outVS.CamW   = -posW;

    // Atmospheric haze --------------------------------------------------------

    AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

    half4 diffuse;
    float ambi, nigh;

    LegacySunColor(diffuse, ambi, nigh, outVS.nrmW);

    outVS.insca *= (diffuse+ambi);
    outVS.insca.a = nigh;

    return outVS;
}


float4 BaseTilePS(TileMeshVS frg) : COLOR
{
   
    // Normalize input
	float3 nrmW = normalize(frg.nrmW);
    float3 CamW = normalize(frg.CamW);
	
	float4 cTex = tex2D(ClampS, frg.tex0);
	
	float3 r = reflect(gSun.direction, nrmW);
	float  s = pow(max(dot(r, CamW), 0.0f), 20.0f) * (1.0f-cTex.a);
	float  d = saturate(dot(-gSun.direction, nrmW));
  
    if (d<=0) s = 0;
       
    half3 clr = cTex.rgb * (max(d,0) * gSun.diffuse.rgb + s * gSun.specular.rgb + gSun.ambient.rgb);

    if (gNight) clr += tex2D(NightS, frg.tex0).rgb;     

    return float4(clr.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);
    //return float4(clr.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a*(1-frg.insca.a));  // Make basetiles transparent during night
}

// =============================================================================
// Vessel Axis vector technique 
// =============================================================================

MeshVS AxisTechVS(MESH_VERTEX vrt)
{
    // Zero output.
	MeshVS outVS = (MeshVS)0;
	float  stretch = vrt.tex0.x * gMix;
	float3 posX = vrt.posL + float3(0.0, stretch, 0.0);
    float3 posW = mul(float4(posX, 1.0f), gW).xyz;              // Apply world transformation matrix
    outVS.posH  = mul(float4(posW, 1.0f), gVP);
    float3 nrmW = mul(float4(vrt.nrmL, 0.0f), gW).xyz;              // Apply world transformation matrix

    outVS.nrmW  = normalize(nrmW);
    outVS.CamW  = -posW;
   
    return outVS;
}


float4 AxisTechPS(MeshVS frg) : COLOR
{
	float3 nrmW = normalize(frg.nrmW);
	float  d = saturate(dot(-gSun.direction, nrmW));
    float3 clr = gColor.rgb * saturate(max(d,0) + 0.5);
    return float4(clr, gColor.a);
}	

technique AxisTech
{
    pass P0
    {
        vertexShader = compile VS_MOD AxisTechVS();
        pixelShader  = compile PS_MOD AxisTechPS();

        AlphaBlendEnable = true;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha; 
        ZEnable = true; 
        ZWriteEnable = true;  
    }
}



// =============================================================================
// Mesh Shadow Technique
// =============================================================================

BShadowVS ShadowMeshTechVS(SHADOW_VERTEX vrt)
{
    // Zero output.
	BShadowVS outVS = (BShadowVS)0;
	float3 posX = mul(float4(vrt.posL, 1.0f), gGrpT).xyz;
    float3 posW = mul(float4(posX, 1.0f), gW).xyz;
    outVS.posH  = mul(float4(posW, 1.0f), gVP);

    return outVS;
}

BShadowVS ShadowMeshTechExVS(SHADOW_VERTEX vrt)
{
    // Zero output.
	BShadowVS outVS = (BShadowVS)0;
	float d = dot(vrt.posL,vrt.posL);
    float3 posX = mul(float4(vrt.posL, 1.0f), gGrpT).xyz;
    float3 posW = mul(float4(posX-gColor.xyz*(gTexOff.x*d+gTexOff.y*d*d), 1.0f), gW).xyz;
    outVS.posH  = mul(float4(posW, 1.0f), gVP);

    return outVS;
}


float4 ShadowTechPS(BShadowVS frg) : COLOR
{
    return float4(0.0f, 0.0f, 0.0f, gMix);
}


technique ShadowTech
{
    pass P0
    {
        vertexShader = compile VS_MOD ShadowMeshTechVS();
        pixelShader  = compile PS_MOD ShadowTechPS();

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
        vertexShader = compile VS_MOD ShadowMeshTechExVS();
        pixelShader  = compile PS_MOD ShadowTechPS();

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

BShadowVS BoundingBoxVS(float3 posL : POSITION0)
{
    // Zero output.
	BShadowVS outVS = (BShadowVS)0;
    float3 pos;
    pos.x = gAttennuate.x * posL.x + gInScatter.x * (1-posL.x);
    pos.y = gAttennuate.y * posL.y + gInScatter.y * (1-posL.y);
    pos.z = gAttennuate.z * posL.z + gInScatter.z * (1-posL.z);
   
    float3 posX = mul(float4(pos, 1.0f), gGrpT).xyz;       // Apply meshgroup specific transformation
    float3 posW = mul(float4(posX, 1.0f), gW).xyz;         // Apply world transformation matrix
    outVS.posH  = mul(float4(posW, 1.0f), gVP);
    return outVS;
}

BShadowVS BoundingSphereVS(float3 posL : POSITION0)
{
    // Zero output.
    BShadowVS outVS = (BShadowVS)0;
    float3 posW = mul(float4(posL, 1.0f), gW).xyz;         // Apply world transformation matrix
    outVS.posH  = mul(float4(posW, 1.0f), gVP);
    return outVS;
}

float4 BoundingBoxPS(BShadowVS frg) : COLOR
{
    return gColor;
}


technique BoundingBoxTech
{
    pass P0
    {
        vertexShader = compile VS_MOD BoundingBoxVS();
        pixelShader  = compile PS_MOD BoundingBoxPS();

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
        vertexShader = compile VS_MOD BoundingSphereVS();
        pixelShader  = compile PS_MOD BoundingBoxPS();

        AlphaBlendEnable = true;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha; 
        ZEnable = true; 
        ZWriteEnable = true;  
    }
}




// This is the default mesh rendering technique --------------------------------
//

technique VesselTech
{
    pass P0
    {
        vertexShader = compile VS_MOD MeshTechNMVS();
        pixelShader  = compile PS_MOD MeshTechNMPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = true;
    }

    pass P1
    {
        vertexShader = compile VS_MOD MeshTechVS();
        pixelShader  = compile PS_MOD MeshTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = true;
    }
}


technique BuildingTech
{
    pass P0
    {
        vertexShader = compile VS_MOD MeshTechNMVS();
        pixelShader  = compile PS_MOD MeshTechNMPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = true;
    }

    pass P1
    {
        vertexShader = compile VS_MOD MeshTechVS();
        pixelShader  = compile PS_MOD MeshTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = true;
    }
}


technique BaseTileTech
{
    pass P0
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
    }   

    pass P1
    {
        vertexShader = compile VS_MOD BaseTileVS();
        pixelShader  = compile PS_MOD BaseTilePS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
        CullMode = CCW;
    }   
}


technique VCTech
{
    pass P0
    {
        vertexShader = compile VS_MOD SimpleMeshTechVS();
        pixelShader  = compile PS_MOD VCTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = true;
        ZWriteEnable = true;
    }
}


technique VCMFDTech
{
    pass P0
    {
        vertexShader = compile VS_MOD SimpleMeshTechVS();
        pixelShader  = compile PS_MOD MFDTechPS();

        AlphaBlendEnable = false;
        ZEnable = true;
        ZWriteEnable = true;
    }
}


technique VCHudTech
{
    pass P0
    {
        vertexShader = compile VS_MOD SimpleMeshTechVS();
        pixelShader  = compile PS_MOD HUDTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = One;
        DestBlend = One;
        ZEnable = false;
    }
}


// This is used for rendering beacons ------------------------------------------
//
technique RingTech
{
    pass P0
    {
        vertexShader = compile VS_MOD TinyMeshTechVS();
        pixelShader  = compile PS_MOD RingTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZWriteEnable = true;
        ZEnable = false;
        CullMode = NONE;
    }   
}