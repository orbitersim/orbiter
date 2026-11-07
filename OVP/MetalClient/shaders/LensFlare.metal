// Metal port of LensFlare.hlsl
#include <metal_stdlib>
using namespace metal;

struct SUNVISPARAMS
{
	float brightness;
	int visible;
	float2 position;
	float4 color;
};

struct LensFlarePSConst
{
	SUNVISPARAMS sunParams;
	int bCockpitCamera;
	float2 vResolution;
	float fSize;
	float fBrightness;
};

#define EPSILON 1e-10
#define CLUT_SIZE float2(256, 16)

static float2 GetDistOffset(float2 uv, float2 pxoffset, float dist)
{
	if (dist == 0.0) return pxoffset;
	float2 tocenter = uv.xy;
	float3 prep = normalize(float3(tocenter.y, -tocenter.x, 0.0));

	float angle = length(tocenter.xy) * 2.221 * saturate(dist);
	float3 oldoffset = float3(pxoffset, 0.0);

	float3 rotated = oldoffset * cos(angle);

	return rotated.xy;
}

static float3 HUEtoRGB(float H)
{
	float R = abs(H * 6 - 3) - 1;
	float G = 2 - abs(H * 6 - 2);
	float B = 2 - abs(H * 6 - 4);
	return saturate(float3(R, G, B));
}

static float3 RGBtoHCV(float3 RGB)
{
	float4 P = (RGB.g < RGB.b) ? float4(RGB.bg, -1.0, 2.0 / 3.0) : float4(RGB.gb, 0.0, -1.0 / 3.0);
	float4 Q = (RGB.r < P.x) ? float4(P.xyw, RGB.r) : float4(RGB.r, P.yzx);
	float C = Q.x - min(Q.w, Q.y);
	float H = abs((Q.w - Q.y) / (6 * C + EPSILON) + Q.z);
	return float3(H, C, Q.x);
}

static float3 RGBtoHSV(float3 RGB)
{
	float3 HCV = RGBtoHCV(RGB);
	float S = HCV.y / (HCV.z + EPSILON);
	return float3(HCV.x, S, HCV.z);
}

static float3 HSVtoRGB(float3 HSV)
{
	float3 RGB = HUEtoRGB(HSV.x);
	return ((RGB - 1) * HSV.y + 1) * HSV.z;
}

static float3 complementary(float3 c)
{
	float3 hsv = RGBtoHSV(c);
	hsv.r += 0.5;
	hsv.r = hsv.r > 1.0 ? hsv.r - 1.0 : hsv.r;
	return HSVtoRGB(hsv);
}

static float3 CLUT(float3 color, texture2d<float> tCLUT, sampler tCLUTS)
{
	float2 CLut_pSize = 1 / CLUT_SIZE;

	float3 c = saturate(color);
	c.b *= 15;
	float3 clut_uv = 0;
	clut_uv.z = floor(c.b);
	clut_uv.xy = c.rg * 15 * CLut_pSize + 0.5 * CLut_pSize;
	clut_uv.x += clut_uv.z * CLut_pSize.y;

	return mix(tCLUT.sample(tCLUTS, clut_uv.xy).rgb, tCLUT.sample(tCLUTS, clut_uv.xy + float2(CLut_pSize.y, 0)).rgb, c.b - clut_uv.z);
}

static float glare(float2 uv, float2 pos, float size, float streakCount, float streakMix)
{
	float2 p = uv - pos;

	float angle = atan2(p.y, p.x);
	float dist = length(p);

	float bright = size - pow(dist, 0.1) * size;
	float streak = pow(abs(sin(angle * streakCount * 0.5)), 2.0) * size;

	return max(0.0, bright * 6.0 + streak * 0.32 * saturate(streakMix));
}

static float flare(float2 uv, float2 pos, float dist, float size)
{
	return max(0.01 - pow(length(uv + dist * pos), 10.0 * size), 0.0) * 6.0;
}

static float3 flare(float2 uv, float2 pos, float dist, float size, float barrel, float3 color)
{
	pos = GetDistOffset(uv, pos, barrel);

	float r = flare(uv, pos, dist - 0.02, size);
	float g = flare(uv, pos, dist, size);
	float b = flare(uv, pos, dist + 0.02, size);

	return max(0.0, color * float3(r, g, b));
}

static float3 ring(float2 uv, float2 pos)
{
	float2 uvd = uv * length(uv);
	float3 col = 0;
	col.r = max(0.0, pow(abs(1.0 - pow(length(uvd * 0.90), 4.0)) * length(uvd), 3.5));
	col.g = max(0.0, pow(abs(1.0 - pow(length(uvd * 0.95), 4.0)) * length(uvd), 3.2));
	col.b = max(0.0, pow(abs(1.0 - pow(length(uvd * 1.00), 4.0)) * length(uvd), 3.0));

	float s = max(0.0, 1.0 / (1.0 + 32.0 * pow(length(uvd + pos), 4.0)));

	return col * s * 4.0;
}

static float3 LensFlare_Exterior(float2 uv, float2 pos, float brightness, float size, float3 color)
{
	float3 f = flare(uv, pos, 1, size, 1.0, float3(0.1, 1.0, 0.5) * color) * 3.0;

	f += flare(uv, pos, -3, 3 * size, 0.1, float3(0.4, 0.6, 1.0) * complementary(color));

	f += flare(uv, pos, -0.5, size, 0.6, float3(0.8, 0.3, 0.9) * complementary(color)) * 1.5;
	f += flare(uv, pos, -0.2, size * 0.6, 1.0, float3(1.0, 0.6, 0.8) * color);
	f += flare(uv, pos, 0.1, size * 0.5, 0.8, float3(0.3, 0.6, 0.5) * color) * 1.5;
	f += flare(uv, pos, 0.6, size, 0.5, float3(0.6, 0.3, 0.2) * complementary(color)) * 2.0;

	float3 c = glare(uv, pos, pow(max(1.0, length(pos)), 2.0) * size, 8.0, 0.5) * color;
	c += f * (1.0 - pow(saturate(length(pos) * 0.50), 2.0));

	c += ring(uv, pos) * 1.5 * size * color;

	return c * brightness;
}

static float3 LensFlare_Cockpit(float2 uv, float2 pos, float brightness, float size, float3 color)
{
	float3 c = glare(uv, pos, size, 16.0, 0.2) * color;
	return c * brightness;
}

static float3 Screen(float3 a, float3 b)
{
	return 1 - (1 - saturate(a)) * (1 - saturate(b));
}

struct IPIFrag
{
	float4 posH [[position]];
	float x;
	float y;
};

fragment float4 PSMain(IPIFrag frg [[stage_in]], constant LensFlarePSConst &cb [[buffer(0)]],
	texture2d<float> tBack [[texture(0)]], sampler tBackS [[sampler(0)]])
{
	float x = frg.x, y = frg.y;
	float3 orig = tBack.sample(tBackS, float2(x, y)).rgb;
	float3 flr = float3(0);

	if (cb.sunParams.visible)
	{
		float2 uv = float2(x, y) - 0.5;
		uv.x *= cb.vResolution.x / cb.vResolution.y;
		uv.y = -uv.y;

		flr = cb.bCockpitCamera ?
			LensFlare_Cockpit(uv, cb.sunParams.position, cb.sunParams.brightness * cb.sunParams.color.a, 0.4 / (cb.fSize), cb.sunParams.color.rgb) * 1.414 :
			LensFlare_Exterior(uv, cb.sunParams.position, cb.sunParams.brightness * cb.sunParams.color.a, 0.4 / (cb.fSize), cb.sunParams.color.rgb) * 1.414;
	}

	float3 color = Screen(orig, pow(saturate(flr), 2.2));
	return float4(color, 1.0);
}
