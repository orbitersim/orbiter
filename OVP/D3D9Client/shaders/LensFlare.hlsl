// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016 SolarLiner (Nathan Graule)
// ==============================================================

// This is the ogirinal, rewrited versiion for D3D9 Client

sampler2D tBack;
sampler2D tCLUT;

struct SUNVISPARAMS
{
	float	brightness;
	bool 	visible;
	float2	position;
	float4	color;
};

uniform extern SUNVISPARAMS sunParams;
uniform extern bool bCockpitCamera;

uniform extern float2 vResolution;

uniform extern float fSize;
uniform extern float fBrightness;
#define EPSILON					1e-10
#define CLUT_SIZE				float2(256, 16)

float2 GetDistOffset(float2 uv, float2 pxoffset, float dist)
{
	if(dist == 0.0) return pxoffset;
	float2 tocenter = uv.xy;
	float3 prep = normalize(float3(tocenter.y, -tocenter.x, 0.0));

	float angle = length(tocenter.xy)*2.221*saturate(dist);
	float3 oldoffset = float3(pxoffset,0.0);

	float3 rotated = oldoffset * cos(angle) /*+ cross(prep, oldoffset) * sin(angle) + prep * dot(prep, oldoffset) * (1.0-cos(angle))*/;

	return rotated.xy;
}

float3 HUEtoRGB(in float H)
{
	float R = abs(H * 6 - 3) - 1;
	float G = 2 - abs(H * 6 - 2);
	float B = 2 - abs(H * 6 - 4);
	return saturate(float3(R,G,B));
}
float3 RGBtoHCV(in float3 RGB)
{
	// Based on work by Sam Hocevar and Emil Persson
	float4 P = (RGB.g < RGB.b) ? float4(RGB.bg, -1.0, 2.0/3.0) : float4(RGB.gb, 0.0, -1.0/3.0);
	float4 Q = (RGB.r < P.x) ? float4(P.xyw, RGB.r) : float4(RGB.r, P.yzx);
	float C = Q.x - min(Q.w, Q.y);
	float H = abs((Q.w - Q.y) / (6 * C + EPSILON) + Q.z);
	return float3(H, C, Q.x);
}
float3 RGBtoHSV(in float3 RGB)
{
	float3 HCV = RGBtoHCV(RGB);
	float S = HCV.y / (HCV.z + EPSILON);
	return float3(HCV.x, S, HCV.z);
}
float3 HSVtoRGB(in float3 HSV)
{
	float3 RGB = HUEtoRGB(HSV.x);
	return ((RGB - 1) * HSV.y + 1) * HSV.z;
}

float3 complementary(float3 c)
{
	float3 hsv = RGBtoHSV(c);
	hsv.r += 0.5;
	hsv.r = hsv.r > 1.0? hsv.r-1.0 : hsv.r;

	return HSVtoRGB(hsv);
}

float3 CLUT(float3 color)
{
	float2 CLut_pSize =  1 / CLUT_SIZE;

	float3 c = saturate(color);
	c.b *= 15;
	float3 clut_uv = 0;
	clut_uv.z = floor(c.b);
	clut_uv.xy = c.rg*15*CLut_pSize+0.5*CLut_pSize;
	clut_uv.x += clut_uv.z * CLut_pSize.y;

	return lerp( tex2D(tCLUT, clut_uv.xy).rgb, tex2D(tCLUT, clut_uv.xy + float2(CLut_pSize.y, 0)).rgb, c.b - clut_uv.z);
}

// Renders the main light glare, along with added streaks
float glare(float2 uv, float2 pos, float size, float streakCount, float streakMix)
{
	float2 p = uv-pos;

	float angle = atan2(p.y, p.x);
	float dist = length(p);

	float bright = size-pow(dist, 0.1)*size;
	float streak = pow(sin(angle*streakCount*0.5), 2.0)*size;

	return max(0.0, bright*6.0 + streak*0.32*saturate(streakMix));
}

// Render circular blobs, colors dissociate and separate with distance of the light form the center
float flare (float2 uv, float2 pos, float dist, float size)
{
	return max(0.01 - pow(length(uv+dist*pos), 10.0*size), 0.0)*6.0;
}
float3 flare(float2 uv, float2 pos, float dist, float size, float barrel, float3 color)
{
	pos = GetDistOffset(uv, pos, barrel);

	float r = flare(uv, pos, dist-0.02, size);
	float g = flare(uv, pos, dist     , size);
	float b = flare(uv, pos, dist+0.02, size);

	return max(0.0, color*float3(r,g,b));
}

// Renders an arc lined with the light source and the center of screen (dist only changes the side the ring is rendered)
float3 ring(float2 uv, float2 pos)
{
	float2 uvd = uv*length(uv);
	float3 col = 0;
	col.r = max(0.0, pow(abs(1.0-pow(length(uvd*0.90), 4.0)) * length(uvd), 3.5));
	col.g = max(0.0, pow(abs(1.0-pow(length(uvd*0.95), 4.0)) * length(uvd), 3.2));
	col.b = max(0.0, pow(abs(1.0-pow(length(uvd*1.00), 4.0)) * length(uvd), 3.0));

	float s = max(0.0, 1.0/(1.0 + 32.0 * pow(length(uvd+pos), 4.0)));

	return col * s*4.0;
}

float3 LensFlare_Exterior(float2 uv, float2 pos, float brightness, float size, float3 color)
{
	// Here you can customize your lens flare.
	// The glare should only appear one and is the star at the position of the light.
	// The flare is the round artifact that appear along the light-center axis.
	// The orb is a collection of flares in a particular arrangement that kinda looks like a caustic.
	// The ring is a wide arc.
	//
	// The "uv" and "pos" variables are mandatory in the drawing of the lens flare and should always be put first and in this order.
	// Next argument is the distance of the artifact. -1.0 is at the light source position, 0.0 is at the center, and 1.0 at the opposite of the light source.
	// Then comes the size. Please make it a multiple of "size" so that is responds to the global size (use a constant if you want the artifact not to scale).
	// Finally is the color of the flare. use it like this: float3(red, green, blue).
	float3 f = flare(uv, pos, 1, size, 1.0, float3(0.1, 1.0, 0.5)*color)*3.0;

	f += flare(uv,pos, -3, 3*size, 0.1, float3(0.4, 0.6, 1.0)*complementary(color));

	f += flare(uv,pos, -0.5, size, 0.6, float3(0.8, 0.3, 0.9)*complementary(color))*1.5;
	f += flare(uv,pos, -0.2, size*0.6, 1.0, float3(1.0, 0.6, 0.8)*color);
	f += flare(uv,pos,  0.1, size*0.5, 0.8, float3(0.3, 0.6, 0.5)*color)*1.5;
	f += flare(uv,pos,  0.6, size, 0.5, float3(0.6, 0.3, 0.2)*complementary(color))*2.0;

	float3 c = glare(uv,pos, pow(max(1.0, length(pos)), 2.0)*size, 8.0, 0.5)*color;
	c += f*(1.0-pow(saturate(length(pos)*0.50), 2.0));

	c += ring(uv, pos)*1.5*size*color;

	return c*brightness;
}

float3 LensFlare_Cockpit(float2 uv, float2 pos, float brightness, float size, float3 color)
{
	float3 c = glare(uv, pos, size, 16.0, 0.2)*color;

	return c*brightness;
}

float3 Screen(float3 a, float3 b)
{
	return 1 - (1-saturate(a))*(1-saturate(b));
}

float4 PSMain(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 orig = tex2D(tBack, float2(x, y)).rgb;
	float3 flare = (float3)0;

	if (sunParams.visible)
	{
		float2 uv = float2(x,y) - 0.5;
		uv.x *= vResolution.x / vResolution.y; // Aspect ratio correction
		uv.y = -uv.y;

		flare = bCockpitCamera?
		LensFlare_Cockpit (uv, sunParams.position, sunParams.brightness*sunParams.color.a, 0.4/(fSize), sunParams.color.rgb)*1.414 :
		LensFlare_Exterior(uv, sunParams.position, sunParams.brightness*sunParams.color.a, 0.4/(fSize), sunParams.color.rgb)*1.414;
	}

	//float3 color = pow(max(0.0, Screen(orig, flare)), 2.2);
	float3 color = Screen(orig, pow(saturate(flare), 2.2));
	//color = CLUT(color);
	return float4(color, 1.0);
}