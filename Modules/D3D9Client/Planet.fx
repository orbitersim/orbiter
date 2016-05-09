// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

struct TileVS
{
    float4 posH    : POSITION0;
    float2 tex0    : TEXCOORD0;
    float2 tex1    : TEXCOORD1;
    float3 normalW : TEXCOORD2; 
    float3 toCamW  : TEXCOORD3;  // Vector to the camera
    float3 posW    : TEXCOORD4;  // World space vertex position
    float4 aux     : TEXCOORD5;  // Specular, Diffuse, Twilight, Night Texture Intensity,
    float4 diffuse : TEXCOORD6;  // Sun light
	float4 atten   : COLOR0;     // Attennuate incoming fragment color
    float4 insca   : COLOR1;     // "Inscatter" Add to incoming fragment color 
};



TileVS PlanetTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS = (TileVS)0;
	
    // Apply a mesh group transformation matrix
    float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
    float3 nrmW = normalize(mul(float4(vrt.normalL, 0.0f), gW).xyz);

	// Convert transformed vertex position into a "screen" space using a combined (World, View and Projection) Matrix
	outVS.posH = mul(float4(posW, 1.0f), gVP);
	
	// A vector from the vertex to the camera
	float3 tocam  = normalize(-posW);
    float3 sundir = gSun.Dir;
    
    float diff    = saturate(dot(-sundir, nrmW));
    float dotr    = max(dot(reflect(sundir, nrmW), tocam), 0.0f);
    float spec    = pow(diff,0.25f) * pow(dotr, gWater.specPower);
    float nigh    = 0.0f;
    float ambi    = 0.0f;
    
	outVS.tex0    = float2(vrt.tex0.x*gTexOff[0] + gTexOff[1], vrt.tex0.y*gTexOff[2] + gTexOff[3]);
    outVS.tex1    = vrt.tex1;
    outVS.toCamW  = tocam;
    outVS.normalW = nrmW;
    outVS.posW    = gCameraPos*gRadius[2] + posW*gDistScale;
   
    LegacySunColor(outVS.diffuse, ambi, nigh, nrmW);

    outVS.aux     = float4(spec, diff, ambi, nigh);

    AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);

    outVS.insca *= (outVS.diffuse+ambi);
    
    return outVS;
}



float4 PlanetTechPS(TileVS frg) : COLOR
{
   
    float4 diff  = frg.aux.g*(gMat.diffuse*frg.diffuse) + (gMat.ambient*frg.aux.b); 
    float  micro = 1.0f;
    
    if (gMix>0.0f) micro -= tex2D(Planet3S, frg.tex1).a;
    
    float4 vSpe = frg.aux.r * (gWater.specular*frg.diffuse) * micro;
    float4 vEff = tex2D(Planet1S, frg.tex0);

    if (gSpecMode==2) vSpe *= 1.0f - vEff.a;
    if (gSpecMode==0) vSpe = 0;
	
    float3 cTex = tex2D(Planet0S, frg.tex0).rgb;
    
    //cTex *= float3(0.8, 0.7, 0.6);
    //cTex = pow(abs(cTex), float3(0.9, 1.1, 1.0));
    
    float3 color = diff.rgb * cTex.rgb + frg.aux.a*vEff.rgb + vSpe.rgb;

	if (gDebugHL) color = color*0.5;

    return float4(color*frg.atten.rgb+gColor.rgb+frg.insca.rgb, 1.0f);
}



float4 CloudTechPS(TileVS frg) : COLOR
{
    float mic = 1.0f;
    
    // Default
    if (gMix>0.0f) mic -= (1.0f - tex2D(Planet3S, frg.tex1).a) * gMix;
    
    // Test
    //if (gMix>0.0f) mic -= (1.0f - tex2D(Planet3S, frg.tex1).a*tex2D(Planet3S, frg.tex1*6).a) * gMix;
 
    float4 data  = (gMat.ambient*frg.aux.b);
    float4 color = tex2D(Planet0S, frg.tex0);
    float  alpha = color.a;
    
    // Modulate color
    //color = lerp(color*float4(0.5, 0.5, 0.5, 1.0), color, mic);
    
    // Modulate Alpha
    alpha *= (mic*0.5+0.5); 
    
    if (dot(frg.normalW, frg.toCamW)<0) {    // Render cloud layer from below
        float4 diff = (min(1,frg.aux.g*2) * frg.diffuse) * gMat.diffuse + data;
        return float4(color.rgb*diff.rgb, alpha);
    }

    else { // Render cloud layer from above                            
        float4 diff = (min(1,frg.aux.g*1.5) * frg.diffuse) * gMat.diffuse + data;
        return float4(color.rgb*diff.rgb, alpha); 
    }
}









// -----------------------------------------------------------------------------
// Cloud Shadow Techs
// -----------------------------------------------------------------------------


struct ShadowVS
{
    float4 posH    : POSITION0;
    float2 tex0    : TEXCOORD0;
    float2 tex1    : TEXCOORD1;
    float4 atten   : TEXCOORD2;  
};

ShadowVS CloudShadowTechVS(TILEVERTEX vrt)
{
    // Zero output.
	ShadowVS outVS = (ShadowVS)0;
	
    float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), gVP);
	outVS.tex0  = float2(vrt.tex0.x*gTexOff[0] + gTexOff[1], vrt.tex0.y*gTexOff[2] + gTexOff[3]);
    outVS.tex1  = vrt.tex1;

    float4 none;

    AtmosphericHaze(outVS.atten, none, outVS.posH.z, posW);
   
    return outVS;
}

float4 CloudShadowPS(ShadowVS frg) : COLOR
{
    float mic = 0.0f;
    if (gMix>0.0f) mic = tex2D(Planet3S, frg.tex1).a * gMix;             
    return float4(0,0,0, (1.0f-mic)*tex2D(Planet0S, frg.tex0).a * frg.atten.b);   
}





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