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

// -------------------------------------------------------------------------------------------------------------
// Vertex shader implementations with per vertex fog
// -------------------------------------------------------------------------------------------------------------

struct AdvancedNMVS
{
    float4 posH     : POSITION0;
    float3 camW     : TEXCOORD0;
    float3 locW     : TEXCOORD1;	 // Local light source average dir
    half4  diff		: COLOR0;		 // Diffuse color
    half4  spec     : COLOR1;        // Specular color
    half3  ambi		: TEXCOORD2;
    half4  atten    : TEXCOORD3;     // Attennuate incoming fragment color
    half4  insca    : TEXCOORD4;     // "Inscatter" Add to incoming fragment color
    half2  tex0     : TEXCOORD5;
    float3 nrmT     : TEXCOORD6;
    float3 tanT     : TEXCOORD7;
};

struct TileMeshNMVS
{
    float4 posH     : POSITION0;
    half2  tex0     : TEXCOORD0;
    half3  LigT     : TEXCOORD1;
    half3  CamT     : TEXCOORD2;
    half4  atten    : TEXCOORD3;     
    half4  insca    : TEXCOORD4;  
};

AdvancedNMVS MeshTechNMVS(MESH_VERTEX vrt)
{
    // Zero output.
	AdvancedNMVS outVS = (AdvancedNMVS)0;

    float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
    float3 nrmW = mul(float4(vrt.nrmL, 0.0f), gW).xyz; 
    outVS.posH  = mul(float4(posW, 1.0f), gVP);

    // Construct Tangent space transform matrix
    float3x3 TBN;
    TBN[0] = vrt.tanL;
    TBN[1] = cross(vrt.tanL, vrt.nrmL);
    TBN[2] = vrt.nrmL; 
    
    TBN = mul(TBN, gW);
    
	outVS.nrmT  = TBN[2];
	outVS.tanT  = TBN[0];
    outVS.camW  = -posW;
    outVS.tex0  = vrt.tex0;
   
    float4 locW;
    
    LocalVertexLight(outVS.diff, outVS.spec, locW, nrmW, posW);

    //Atmospheric haze --------------------------------------------------------

    AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

    outVS.insca *= (gSun.diffuse+gSun.ambient);
    outVS.locW = locW;
    
    // Earth "glow" ------------------------------------------------------------
    float dotb = saturate(-dot(gCameraPos, gSun.direction));
    float dota = -dot(gCameraPos, nrmW);
	float angl = saturate((dota-gProxySize)/(1.0f-gProxySize));
	outVS.ambi = gAtmColor * (pow(angl*dotb, 0.3) * 0.15);
	
	// Add constanst -----------------------------------------------------------
	outVS.ambi += (gMat.ambient.rgb*gSun.ambient.rgb) + (gMat.emissive.rgb);
   
    return outVS;
}

float4 MeshTechNMPS(AdvancedNMVS frg) : COLOR
{
	// Normalize input
	float3 CamW = normalize(frg.camW);
   
	float4 cSpec; 
	float3 cRefl;
    float  iRefl;	

    float4 cTex = tex2D(WrapS, frg.tex0);
    if (gModAlpha) cTex.a *= gMat.diffuse.a;	
  
    float3 nrmT = float3(tex2D(Nrm0S, frg.tex0).rgb*2.0-1.0);       //Sampler for R8G8B8, DXT1
	
	float3x3 TBN;
    TBN[0] = frg.tanT;
    TBN[1] = cross(frg.tanT, frg.nrmT);
    TBN[2] = frg.nrmT; 
    
    float3 nrmW = mul(nrmT, TBN);
    
	if (gUseSpec) cSpec = tex2D(SpecS, frg.tex0);																			
    else 	      cSpec = float4(gMat.specular.rgb, gReflCtrl[0]);		
		
	cRefl = cSpec.rgb;
	
	 // Sunlight calculation
    float d = saturate(-dot(gSun.direction, nrmW));
    float s = pow(saturate(dot(reflect(gSun.direction, nrmW), CamW)), cSpec.a) * saturate(cSpec.a);
   			
    if (d==0) s = 0;							
    																		
    float3 diff = frg.diff.rgb * saturate(-dot(frg.locW, nrmW)) + frg.ambi + d;
    float3 spec = frg.spec.rgb + s;

    if (gUseEmis) diff += tex2D(EmisS, frg.tex0).rgb;

    cTex.rgb  *= saturate(diff);
    cSpec.rgb *= spec;
   
    if (gEnvMapEnable) {	
		cTex.rgb  *= (1.0-cSpec.a); 
		cSpec.rgb += cRefl.rgb * texCUBE(EnvMapS, reflect(-CamW, nrmW)).rgb * cSpec.a;						
    }
	
	cTex.rgb += cSpec.rgb;
    
    if (gNight && gTextured) cTex.rgb += tex2D(NightS, frg.tex0).rgb; 
    
    if (gDebugHL) cTex.rgb = cTex.rgb*0.5 + gColor.rgb;

    return float4(cTex.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);
}


// =============================================================================
// Base Tile Rendering Technique
// =============================================================================


TileMeshNMVS BaseTileNMVS(MESH_VERTEX vrt)
{
    // Null the output
	TileMeshNMVS outVS = (TileMeshNMVS)0;

    float3 posW  = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posH   = mul(float4(posW, 1.0f), gVP);

    half3x3 TBN;
	TBN[0] = vrt.tanL.xyz;
	TBN[1] = cross(vrt.tanL, vrt.nrmL);
	TBN[2] = vrt.nrmL.xyz;

    half3x3 mTS = transpose(TBN);

    float3 CamL = mul(float4(0,0,0,1), gWI).xyz;
    half3  LigL = mul(half4(gSun.direction,0), gWI).xyz;
    half3  nrmW = mul(half4(vrt.nrmL.xyz,0), gW).xyz;
	
	outVS.LigT  = mul(LigL, mTS);
    outVS.CamT  = mul(CamL-vrt.posL, mTS);
    outVS.tex0  = vrt.tex0;
	
   
    // Atmospheric haze --------------------------------------------------------

    AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

    half4 diffuse;
    float ambi, nigh;

    LegacySunColor(diffuse, ambi, nigh, nrmW);

    outVS.insca *= (diffuse+ambi);
    return outVS;
}


float4 BaseTileNMPS(TileMeshNMVS frg) : COLOR
{
    // Normalize input
	float3 LigT = normalize(frg.LigT);
    float3 CamT = normalize(frg.CamT);
    float3 nrmT = float3(0,0,1);

    half4 cTex = tex2D(ClampS, frg.tex0); 

    if (gNormalType)  nrmT = float3(tex2D(Nrm0S, frg.tex0).rgb*2.0-1.0);         //Sampler for R8G8B8, A8R8G8B8, V8U8, CxV8U8
    else              nrmT = float3(tex2D(Nrm0S, frg.tex0).ag*2.0-1.0, 1.0);     //Sampler for DXT5, A8L8 
    
	float3 r = reflect(LigT, nrmT);
	float  s = pow(max(dot(r, CamT), 0.0f), 20.0f) * (1.0f-cTex.a);
	float  d = saturate(dot(-LigT, nrmT)*1.5);
  
    if (d<=0) s = 0;
       
    half3 clr = cTex.rgb * (max(d,0) * gSun.diffuse.rgb + s * gSun.specular.rgb + gSun.ambient.rgb);
    
    return float4(clr.rgb*frg.atten.rgb+frg.insca.rgb, cTex.a);
}