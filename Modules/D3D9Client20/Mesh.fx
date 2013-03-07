// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

struct AdvancedVS
{
    float4 posH     : POSITION0;
    float3 CamW     : TEXCOORD0;     
    float3 nrmW     : TEXCOORD1;
    float2 tex0     : TEXCOORD2;
    float2 aux		: TEXCOORD3; 
    half4  diffuse  : COLOR0;           // (Local Light) Diffuse color
    half4  spec     : COLOR1;           // (Local Light) Specular color
    half4  atten    : TEXCOORD4;        // (Atmospheric haze) Attennuate incoming fragment color
    half4  insca    : TEXCOORD5;        // (Atmospheric haze) "Inscatter" Add to incoming fragment color
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

	float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	float3 nrmW = mul(float4(vrt.nrmL, 0.0f), gW).xyz;

    nrmW = normalize(nrmW);

	// A vector from the vertex to the camera
	
	outVS.CamW  = -posW * gDistScale + gCamOff;   
	outVS.tex0  = vrt.tex0;
    outVS.nrmW  = nrmW;
	outVS.posH  = mul(float4(posW, 1.0f), gVP);
	
	float3 CamW = normalize(outVS.CamW);
	
    half4 locW;
    LocalVertexLight(outVS.diffuse, outVS.spec, locW, nrmW, posW);
   
    // Atmospheric haze --------------------------------------------------------

    AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

    outVS.insca *= (gSun.diffuse+gSun.ambient);
    
    // Earth "glow" ------------------------------------------------------------
    float dotb = saturate(-dot(gCameraPos, gSun.direction));
    float dota = -dot(gCameraPos, nrmW);
	float angl = saturate((dota-gProxySize)/(1.0f-gProxySize));
	outVS.diffuse += gAtmColor * pow(angl*dotb, 0.5);
	
	// Add constanst -----------------------------------------------------------
	outVS.diffuse.rgb += (gMtrl.ambient.rgb*gSun.ambient.rgb) + (gMtrl.emissive.rgb);
	
	// Pre-compute fresnel term ------------------------------------------------

#if defined(_ENVMAP)
	outVS.aux[0] = gMtrl.fresnel.x + gMtrl.fresnel.y * pow(1.0f-saturate(dot(CamW, nrmW)), gMtrl.fresnel.z);
#endif
	
    return outVS;
}


float4 MeshTechPS(AdvancedVS frg) : COLOR
{

    // Normalize input
    float3 CamW = normalize(frg.CamW);
    float3 nrmW = normalize(frg.nrmW);
    
	float4 cTex;
    float4 cSpec;
    float4 cRefl;
    
	if (gTextured) {													// Sample the main diffuse texture
        cTex = tex2D(WrapS, frg.tex0);
        if (gModAlpha) cTex.a *= gMtrl.diffuse.a;    
    }
    else cTex = gMtrl.diffuse;
    
   
    if (gUseSpec) {														// Get specular color and power
		cSpec = tex2D(SpecS, frg.tex0);		
		cSpec.a *= 255.0;
	}																		
    else cSpec = gMtrl.specular;	
    	
	// Sunlight calculations. Saturate with cSpec.a to gain an ability to disable specular light
    float  d = saturate(-dot(gSun.direction, nrmW));
    float  s = pow(saturate(dot(reflect(gSun.direction, nrmW), CamW)), cSpec.a) * saturate(cSpec.a);					
    
    if (d==0) s = 0;	
    																					
    float3 diff = frg.diffuse.rgb + d * gSun.diffuse.rgb;				// Compute total diffuse light
   
    if (gUseEmis) diff += tex2D(EmisS, frg.tex0).rgb;					// Add emissive textures

    cTex.rgb *= saturate(diff);											// Lit the diffuse texture

    float3 cTot = cSpec.rgb * (frg.spec.rgb + s * gSun.specular.rgb);	// Compute total specular light
    
#if defined(_ENVMAP)

	if (gEnvMapEnable) {
    
		if (gUseRefl) cRefl = tex2D(SpecS, frg.tex0);					// Get a reflection color for non fresnel refl. (Pre-computed intensity in alpha)
		else 		  cRefl = gMtrl.reflect;
		
		cRefl.rgb = cRefl.rgb * (1.0f - frg.aux[0]) + frg.aux[0];		// Multiply with refraction term and add reflection
		
        float3 v = reflect(-CamW, nrmW);								// Reflection vector
		
		// Apply noise/blur effects in reflections
        if (gUseDisl) v += (tex2D(DislMapS, frg.tex0*gMtrl.dislscale)-0.5f) * gMtrl.dislmag;
		
		cTex.rgb *= (1.0f - cRefl.a); 									// Attennuate diffuse texture
		cTot.rgb += cRefl.rgb * texCUBE(EnvMapS, v).rgb;				// Add reflections into a specular light
    }
    
#endif 

#if defined(_ENVMAP) || defined(_GLASS)
	cTex.a = saturate(cTex.a + max(max(cTot.r, cTot.g), cTot.b));		// Re-compute output alpha for alpha blending stage
#endif

	cTex.rgb += cTot.rgb;												// Apply reflections to output color
																		
    if (gNight) cTex.rgb += tex2D(NightS, frg.tex0).rgb; 				// Apply building nightlights
    
#if defined(_DEBUG)
    if (gDebugHL) cTex.rgb = cTex.rgb*0.5f + gColor.rgb;				// Apply mesh debugger highlighting
#endif

    return float4(cTex.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);		// Apply fog and light inscattering
} 



MeshVS SimpleMeshTechVS(MESH_VERTEX vrt)
{
    // Zero output.
	MeshVS outVS = (MeshVS)0;
	
	float3 posX = mul(float4(vrt.posL, 1.0f), gGrpT).xyz;       // Apply meshgroup specific transformation
    float3 posW = mul(float4(posX, 1.0f), gW).xyz;              // Apply world transformation matrix
    outVS.posH  = mul(float4(posW, 1.0f), gVP);
   
    float3 nrmX = mul(float4(vrt.nrmL.xyz, 0.0f), gGrpT).xyz;   // Apply meshgroup specific transformation
    float3 nrmW = mul(float4(nrmX, 0.0f), gW).xyz;              // Apply world transformation matrix

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

    if (gModAlpha || !gTextured) cTex.a *= gMtrl.diffuse.a;	
    
    if (gFullyLit) {
		if (gDebugHL) cTex.rgb = cTex.rgb*0.5 + gColor.rgb;
		return float4(cTex.rgb*saturate(gMtrl.diffuse.rgb+gMtrl.emissive.rgb), cTex.a);
	}
   
    float3 r = reflect(gSun.direction, nrmW);
    float  d = saturate(-dot(gSun.direction, nrmW));
	float  s = pow(saturate(dot(r, CamW)), gMtrl.specular.a) * saturate(gMtrl.specular.a);

    if (d==0) s = 0;

    half3 diff = gMtrl.diffuse.rgb  * (d * gSun.diffuse.rgb) + (gMtrl.ambient.rgb*gSun.ambient.rgb) + (gMtrl.emissive.rgb);
    half3 spec = gMtrl.specular.rgb * (s * gSun.specular.rgb);
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
	float  s = pow(max(dot(r, CamW), 0.0f), gMtrl.specular.a) * saturate(gMtrl.specular.a);

    float3 color = tex2D(MFDSamp, frg.tex0).rgb + saturate(s * gMtrl.specular.rgb);
	
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

float4 RingTech2PS(MeshVS frg) : COLOR
{
    float3 pp  = gCameraPos*gRadius[2] - frg.CamW*gDistScale;
    float  dpp = dot(pp,pp);
    float  len = sqrt(dpp);
    
    len = saturate(smoothstep(gTexOff.x, gTexOff.y, len));
   
    float4 color = tex2D(RingS, float2(len, 0.5));
    color.a = color.r*0.75;
     
    float  da = dot(normalize(pp), gSun.direction);
    float  r  = sqrt(dpp*(1.0-da*da));
    
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
	float  s = pow(saturate(dot(r, CamW)), 20.0f) * (1.0f-cTex.a);
	float  d = saturate(dot(-gSun.direction, nrmW));
  
    if (d<=0) s = 0;
       
    half3 clr = cTex.rgb * saturate(d * gSun.diffuse.rgb + s * gSun.specular.rgb + gSun.ambient.rgb);

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

technique RingTech2
{
    pass P0
    {
        vertexShader = compile VS_MOD TinyMeshTechVS();
        pixelShader  = compile PS_MOD RingTech2PS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZWriteEnable = true;
        ZEnable = false;
        CullMode = NONE;
    }   
}