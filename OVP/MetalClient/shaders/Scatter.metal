// Metal port of Scatter.hlsl
#include <metal_stdlib>
using namespace metal;

#if defined(_PERFORMANCE)
#define Nc  6
#define Wc  72
#define Qc  64
#else
#define Nc  8
#define Wc  128
#define Qc  96
#endif

#define NSEG 5
#define iNSEG 1.0f / NSEG
#define MINANGLE -0.33f
#define ANGRNG (0.25f - MINANGLE)
#define iANGRNG (1.0f / ANGRNG)
#define LastLine (0.999999f - 1.0f / Qc)

struct AtmoParams
{
	float4x4 mVP;
	float3 CamPos;
	float3 toCam;
	float3 toSun;
	float3 SunAz;
	float3 ZeroAz;
	float3 Up;
	float3 vTangent;
	float3 vBiTangent;
	float3 vPolarAxis;
	float3 cSun;
	float3 RayWave;
	float3 MieWave;
	float4 HG;
	float2 iH;
	float2 rmO;
	float2 rmI;
	float3 cAmbient;
	float3 cGlare;
	float PlanetRad;
	float PlanetRad2;
	float AtmoAlt;
	float AtmoRad;
	float AtmoRad2;
	float CloudAlt;
	float MinAlt;
	float MaxAlt;
	float iAltRng;
	float AngMin;
	float AngRng;
	float iAngRng;
	float AngCtr;
	float HrzDst;
	float CamAlt;
	float CamElev;
	float CamRad;
	float CamRad2;
	float Expo;
	float Time;
	float TrGamma;
	float TrExpo;
	float Ambient;
	float Clouds;
	float TW_Terrain;
	float TW_Dst;
	float CosAlpha;
	float SinAlpha;
	float CamSpace;
	float Cr2;
	float ShdDst;
	float SunVis;
	float dCS;
	float smi;
	float ecc;
	float trLS;
	float wNrmStr;
	float wSpec;
	float wBrightness;
	float wBoost;
};

struct sFlow
{
	int bRay;
	int bCamLit;
	int bCamInSpace;
};

struct ScatterConstPS
{
	AtmoParams Const;
};

struct ScatterFlowPS
{
	AtmoParams Const;
	sFlow Flo;
};

#define CONST_P constant AtmoParams &Const
#define FLOW_P constant sFlow &Flo
#define TEX_P(t) texture2d<float> t, sampler t##S

#if NSEG == 5
constant float n[] = { 0.050, 0.25, 0.50, 0.75, 0.950 };
constant float w[] = { 0.125, 0.25, 0.25, 0.25, 0.125 };
#endif

#if NSEG == 7
constant float n[] = { 0.05, 0.167, 0.333, 0.500, 0.667, 0.833, 0.95 };
constant float w[] = { 0.08, 0.167, 0.167, 0.167, 0.167, 0.167, 0.08 };
#endif

constant float4 n0 = float4(0.0714, 0.21428, 0.35714, 0.5);
constant float4 w0 = float4(0.1295, 0.27971, 0.38183, 0.41796);
constant float4 n1 = float4(0.64285, 0.78571, 0.92857, 0);
constant float4 w1 = float4(0.38183, 0.27971, 0.1295, 0);

constant float4 n4 = float4(0.06943, 0.33001, 0.66999, 0.93057);
constant float4 w4 = float4(0.34786, 0.65215, 0.65215, 0.34786);

static float ilerp(float a, float b, float x)
{
	return saturate((x - a) / (b - a));
}

static float3 sqr(float3 x)
{
	return x * x;
}

static float4 expc(float4 x) { return exp(clamp(x, -20.0f, 20.0f)); }
static float3 expc(float3 x) { return exp(clamp(x, -20.0f, 20.0f)); }
static float2 expc(float2 x) { return exp(clamp(x, -20.0f, 20.0f)); }
static float expc(float x) { return exp(clamp(x, -20.0f, 20.0f)); }

static float2 NrmToUV(CONST_P, float3 vNrm)
{
	float2 uv = float2(dot(Const.ZeroAz, vNrm), dot(Const.SunAz, vNrm)) / Const.AngCtr;
	return (uv * 0.5 + 0.5);
}

static float3 uvToLoc(float2 uv, float r)
{
	uv = (uv * 2.0 - 1.0);
	float w = uv.x * uv.x + uv.y * uv.y;
	if (w > 1.0f) uv *= rsqrt(w);
	uv *= r;
	float det = 1.0f - uv.x * uv.x - uv.y * uv.y;
	float cos_ab = det > 0.0f ? sqrt(det) : 0.0f;
	return float3(uv.xy, cos_ab);
}

static float3 uvToNrm(CONST_P, float2 uv)
{
	float3 q = uvToLoc(uv, Const.AngCtr);
	return normalize(Const.SunAz * q.y + Const.ZeroAz * q.x + Const.toCam * q.z);
}

static float3 uvToDir(CONST_P, float2 uv)
{
	uv.y = uv.y * rsqrt(0.2f + uv.y * uv.y) * sqrt(1.2f);

	float x = uv.x * 2.0f - 1.0f;
	float y = sqrt(1.0f - x * x);
	float z = 1.0f - uv.y * Const.AngRng;
	float k = sqrt(1.0f - z * z);

	return normalize(Const.SunAz * x * k + Const.ZeroAz * y * k + Const.toCam * z);
}

static float2 DirToUV(CONST_P, float3 uDir)
{
	float y = dot(uDir, Const.toCam);
	float3 uOrt = normalize(uDir - Const.toCam * y);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	float2 uv = float2(x, 1.0f - (y - Const.AngMin) * Const.iAngRng);
	uv.y = sqrt(0.2f) * uv.y * rsqrt(1.2f - uv.y * uv.y);
	return uv;
}

static float3 HDR(CONST_P, float3 x)
{
	return 1.0f - exp(-Const.Expo * x);
}

static float3 LightFX(float3 x)
{
	return 2.0 * x / (1.0f + x);
}

static float RayLength(float cos_dir, float r0, float r1)
{
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	return sqrt(r1 * r1 - z2) + y;
}

static float RayLength(CONST_P, float cos_dir, float r0)
{
	return RayLength(cos_dir, r0, Const.AtmoRad);
}

static float3 TransformUV(float3 uv, const float rc, const float prc)
{
	const float ipix = 1.0f / rc;

	uv = saturate(uv);
	uv.z *= (rc - 1);
	uv.x *= ipix;
	uv.x += floor(uv.z) * ipix;
	return uv;
}

static float4 smaple3D(TEX_P(tSamp), float3 uv, const float rc, const float pix)
{
	const float x = 1.0f / rc;

	float4 a = tSamp.sample(tSampS, uv.xy).rgba;
	float4 b = tSamp.sample(tSampS, uv.xy + float2(x, 0)).rgba;
	return mix(a, b, fract(uv.z));
}

static float2 Gauss7(CONST_P, float cos_dir, float r0, float dist, float2 ih0)
{
	int i;
	float x = 2.0 * r0 * cos_dir;
	float r2 = r0 * r0;

	float4 d0 = dist * n0;
	float4 a0 = sqrt(r2 + d0 * (d0 - x)) - Const.PlanetRad;
	float4 d1 = dist * n1;
	float4 a1 = sqrt(r2 + d1 * (d1 - x)) - Const.PlanetRad;

	float2 sum = 0.0f;
	for (i = 0; i < 4; i++) sum += expc(-a0[i] * ih0) * w0[i];
	for (i = 0; i < 3; i++) sum += expc(-a1[i] * ih0) * w1[i];
	return sum * dist * 0.5f;
}

static float2 Gauss4(CONST_P, float cos_dir, float r0, float dist, float2 ih0)
{
	float4 d0 = dist * n4;
	float4 a0 = sqrt(r0 * r0 + d0 * d0 - 2.0 * r0 * d0 * cos_dir) - Const.PlanetRad;
	float4 ray = expc(-a0 * ih0.x);
	float4 mie = expc(-a0 * ih0.y);
	return float2(dot(ray, w4), dot(mie, w4)) * dist * 0.5f;
}

static float RayPhase(float cw)
{
	return 0.25f * (4.0f + cw * cw);
}

static float MiePhase(CONST_P, float cw)
{
	return 8.0f * Const.HG.x / (1.0f - Const.HG.y * cw) + Const.HG.w;
}

static float3 GetSunColor(CONST_P, TEX_P(tSun), float dir, float alt)
{
	float maxalt = max(Const.MaxAlt, Const.CloudAlt);
	alt = ilerp(Const.MinAlt, maxalt, alt);
	dir = saturate((dir - MINANGLE) * iANGRNG);
	alt = sqrt(alt);
	return tSun.sample(tSunS, float2(dir, alt)).rgb;
}

static float3 ComputeCameraView(CONST_P, float a, float r, float d)
{
	float2 rm = Gauss7(Const, a, r, d, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}

static float4 AmbientApprox(CONST_P, float dNS, const bool bR = true)
{
	float fA = 1.0f - smoothstep(0.0f, Const.TW_Dst, -dNS);
	float3 clr = (bR ? Const.RayWave : Const.cAmbient);
	return float4(clr, fA);
}

static float4 AmbientApprox(CONST_P, float3 vNrm, const bool bR = true)
{
	float dNS = dot(vNrm, Const.toSun);
	return AmbientApprox(Const, dNS, bR);
}

struct RayData
{
	float se;
	float sx;
	float ae;
	float ax;
	float hd;
	float ca;
};

struct IData
{
	float s0;
	float s1;
	float e0;
	float e1;
};

static RayData ComputeRayStats(CONST_P, float3 vRay, const bool bPreProcessData)
{
	RayData dat = {};

	const float invalid = -1e9;

	float u = dot(vRay, Const.Up);
	float t = dot(vRay, Const.ZeroAz);
	float z = dot(vRay, Const.toSun);

	float a = u * rsqrt(u * u + t * t);

	float k2 = Const.Cr2 * a * a;
	float h2 = Const.Cr2 - k2;
	float w2 = Const.PlanetRad2 - h2;
	float2 b = sqrt(float2(w2, k2));
	float k = b.y * sign(a);
	float v2 = 0;
	float m = Const.CamRad2 - Const.PlanetRad2;

	dat.se = k - b.x;
	dat.sx = dat.se + 2.0f * b.x;

	float q = rsqrt(max(2.5e-5, 1.0 - z * z));
	dat.se *= q;
	dat.sx *= q;

	a = -dot(Const.toCam, vRay);
	k2 = Const.CamRad2 * a * a;
	h2 = Const.CamRad2 - k2;
	v2 = Const.AtmoRad2 - h2;
	float3 n = sqrt(float3(v2, k2, m));
	k = n.y * sign(a);

	dat.hd = m > 0.0 ? n.z : 0.0;
	dat.ae = (k - n.x);
	dat.ax = dat.ae + 2.0f * n.x;
	dat.ca = Const.CamRad * a;

	if (v2 < 0) dat.ae = dat.ax = invalid;

	if (w2 < 0) dat.se = dat.sx = invalid;

	if (bPreProcessData)
	{
		float3 vEn = Const.CamPos + vRay * dat.se;
		float3 vEx = Const.CamPos + vRay * dat.sx;

		if (dot(vEn, Const.toSun) > 0) dat.se = invalid;
		if (dot(vEx, Const.toSun) > 0) dat.sx = invalid;
	}

	return dat;
}

static IData PostProcessData(FLOW_P, RayData sp)
{
	IData d;
	if (!Flo.bCamLit) {
		d.s0 = max(sp.sx, sp.ae);

		float lf = max(0.0f, sp.ca) / max(1.0f, abs(sp.hd));
		float mp = mix((sp.ax + d.s0) * 0.5f, sp.hd, saturate(lf));

		d.e0 = mp;
		d.s1 = max(sp.sx, mp);
		d.e1 = sp.ax;
	}
	else {
		d.s0 = max(0.0f, sp.ae);

		float lf = max(0.0f, sp.ca) / max(1.0f, abs(sp.hd));
		float mp = mix((sp.ax + d.s0) * 0.5f, sp.hd, saturate(lf));

		bool bA = (sp.se > sp.ax || sp.se < 0);

		d.e0 = bA ? mp : sp.se;
		d.s1 = bA ? mp : max(sp.sx, sp.ae);
		d.e1 = sp.ax;
	}
	return d;
}

static float3 ComputeCameraView(CONST_P, FLOW_P, float3 vPos, float3 vNrm, float3 vRay, float r)
{
	float d;
	float a = dot(vNrm, vRay);
	if (Flo.bCamInSpace) d = RayLength(Const, a, r);
	else d = dot(vPos - Const.CamPos, vRay);
	float2 rm = Gauss7(Const, a, r, d, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}

static float4 IntegrateSegmentMP(CONST_P, FLOW_P, TEX_P(tSun), float3 vOrig, float3 vRay, float len, float iH)
{
	float4 ret = 0;
	float3 vR = vRay * len;
	for (int i = 0; i < NSEG; i++)
	{
		float3 pos = vOrig + vR * (iNSEG * (float(i) + 0.5f));
		float3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		float3 x = GetSunColor(Const, tSun, tSunS, dot(n, Const.toSun), alt);
		x *= ComputeCameraView(Const, Flo, pos, n, vRay, rad);
		float f = exp(-alt * iH) * iNSEG;
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}

static float4 IntegrateSegmentNS(CONST_P, FLOW_P, TEX_P(tSun), float3 vOrig, float3 vRay, float len, float iH)
{
	float4 ret = 0;
	float3 vR = vRay * len;
	for (int i = 0; i < NSEG; i++)
	{
		float3 pos = vOrig + vR * n[i];
		float3 nn = normalize(pos);
		float rad = dot(nn, pos);
		float alt = rad - Const.PlanetRad;
		float3 x = GetSunColor(Const, tSun, tSunS, dot(nn, Const.toSun), alt);
		x *= ComputeCameraView(Const, Flo, pos, nn, vRay, rad);
		float f = exp(-alt * iH) * w[i];
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}

struct ScatterFrag
{
	float4 posH [[position]];
	float x;
	float y;
};

fragment float4 SunColor(ScatterFrag frg [[stage_in]], constant ScatterConstPS &cb [[buffer(0)]])
{
	constant AtmoParams &Const = cb.Const;
	float x = frg.x, y = frg.y;
	float maxalt = max(Const.MaxAlt, Const.CloudAlt);
	float alt = mix(Const.MinAlt, maxalt, y * y);
	float ang = x * ANGRNG + MINANGLE;
	float rad = alt + Const.PlanetRad;
	float dist = RayLength(Const, -ang, rad);
	float2 rm = Gauss7(Const, -ang, rad, dist, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;

	return float4(exp(-clr), 1.0f);
}

struct SkyOut
{
	float4 ray;
	float4 mie;
};

static SkyOut GetSkyColor(CONST_P, TEX_P(tSkyRayColor), TEX_P(tSkyMieColor), float3 uDir)
{
	float2 uv = DirToUV(Const, uDir);

	SkyOut o;
	o.ray = tSkyRayColor.sample(tSkyRayColorS, uv).rgba;
	o.mie = tSkyMieColor.sample(tSkyMieColorS, uv).rgba;
	return o;
}

fragment float4 SkyView(ScatterFrag frg [[stage_in]], constant ScatterFlowPS &cb [[buffer(0)]],
	texture2d<float> tSun [[texture(6)]], sampler tSunS [[sampler(6)]])
{
	constant AtmoParams &Const = cb.Const;
	constant sFlow &Flo = cb.Flo;
	float u = frg.x, v = frg.y;

	float3 vRay = uvToDir(Const, float2(u, v));

	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	RayData sp = ComputeRayStats(Const, vRay, true);
	IData id = PostProcessData(Flo, sp);

	float4 ret = 0;
	if (id.e0 > id.s0)
		ret += IntegrateSegmentNS(Const, Flo, tSun, tSunS, Const.CamPos + vRay * id.s0, vRay, id.e0 - id.s0, iH);

	if (id.e1 > id.s1)
		ret += IntegrateSegmentNS(Const, Flo, tSun, tSunS, Const.CamPos + vRay * id.s1, vRay, id.e1 - id.s1, iH);

	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	if (Flo.bRay)
	{
		float2 vDepth = Gauss7(Const, dot(Const.toCam, -vRay), Const.CamRad, sp.ax, Const.iH);
		float2 vOut = vDepth * Const.rmO;
		float4 cMlt = AmbientApprox(Const, Const.toCam);
		cMlt.rgb *= exp(-Const.CamAlt * Const.iH.r);
		cMlt.rgb *= cMlt.a;
		cMlt.rgb *= exp(-(Const.RayWave * vOut.r + Const.MieWave * vOut.g));
		float alpha = ilerp(10e3, 150e3, vDepth.r);
		return float4(ret.rgb + cMlt.rgb, alpha);
	}

	return float4(ret.rgb, 1.0f);
}

fragment float4 RingView(ScatterFrag frg [[stage_in]], constant ScatterFlowPS &cb [[buffer(0)]],
	texture2d<float> tSun [[texture(6)]], sampler tSunS [[sampler(6)]])
{
	constant AtmoParams &Const = cb.Const;
	constant sFlow &Flo = cb.Flo;
	float u = frg.x, v = frg.y;

	if (v >= LastLine) return float4(0, 0, 0, 0);

	v *= v;
	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	float x = -1.0f + u * 2.0f;
	float y = sqrt(max(1e-6, 1.0f - x * x));
	float e = v * Const.AtmoAlt;
	float re = Const.PlanetRad + e;
	float z = re * Const.CosAlpha;
	float j = re * Const.SinAlpha;

	float3 vPos = Const.SunAz * x * j + Const.ZeroAz * y * j + Const.toCam * z;
	float3 vRay = normalize(vPos - Const.CamPos);
	float cpd = abs(dot(vRay, Const.CamPos - vPos));

	RayData sp = ComputeRayStats(Const, vRay, true);
	IData id = PostProcessData(Flo, sp);

	float4 ret = 0;
	if (id.e0 > id.s0)
		ret += IntegrateSegmentNS(Const, Flo, tSun, tSunS, vPos - vRay * (cpd - id.s0), vRay, id.e0 - id.s0, iH);

	if (id.e1 > id.s1)
		ret += IntegrateSegmentNS(Const, Flo, tSun, tSunS, vPos - vRay * (cpd - id.s1), vRay, id.e1 - id.s1, iH);

	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	if (Flo.bRay)
	{
		float3 vSrc = vPos - vRay * (cpd - sp.ax);
		float3 vNrm = normalize(vSrc);
		float2 vDepth = Gauss4(Const, dot(vNrm, vRay), dot(vSrc, vNrm), sp.ax - sp.ae, Const.iH);
		float2 vOut = vDepth * Const.rmO;
		float4 cMlt = AmbientApprox(Const, Const.toCam);
		cMlt.rgb *= exp(-Const.CamAlt * Const.iH.r);
		cMlt.rgb *= cMlt.a;
		cMlt.rgb *= exp(-(Const.RayWave * vOut.r + Const.MieWave * vOut.g));
		float alpha = ilerp(10e3, 150e3, vDepth.r);
		alpha = alpha > 0 ? sqrt(alpha) : 0;
		return float4(ret.rgb + cMlt.rgb, alpha);
	}

	return float4(ret.rgb, 1.0f);
}

static float3 GetAmbient(CONST_P, TEX_P(tAmbient), float3 vRay)
{
	return tAmbient.sample(tAmbientS, DirToUV(Const, vRay)).rgb;
}

fragment float4 AmbientSky(ScatterFrag frg [[stage_in]], constant ScatterConstPS &cb [[buffer(0)]],
	texture2d<float> tSkyRayColor [[texture(4)]], sampler tSkyRayColorS [[sampler(4)]],
	texture2d<float> tSkyMieColor [[texture(5)]], sampler tSkyMieColorS [[sampler(5)]])
{
	constant AtmoParams &Const = cb.Const;
	float2 uv = float2(frg.x, frg.y);
	float3 uDir = uvToDir(Const, uv);
	float ph = dot(uDir, Const.toSun);

	float3 ray = tSkyRayColor.sample(tSkyRayColorS, uv).rgb;
	float3 mie = tSkyMieColor.sample(tSkyMieColorS, uv).rgb;
	float3 color = ray * RayPhase(ph) + mie * MiePhase(Const, ph);

	return float4(color, 1.0f);
}

struct LandOut
{
	float4 ray;
	float4 mie;
	float4 atn;
};

static LandOut GetLandView(CONST_P, TEX_P(tLndRay), TEX_P(tLndMie), TEX_P(tLndAtn), float rad, float3 vNrm)
{
	float2 uv = NrmToUV(Const, vNrm);

	float a = rad - Const.PlanetRad;
	float z = saturate((a - Const.MinAlt) * Const.iAltRng);

	float3 uvb = TransformUV(float3(uv, z), Nc, Wc);

	LandOut o;
	o.ray = smaple3D(tLndRay, tLndRayS, uvb, Nc, Wc);
	o.mie = smaple3D(tLndMie, tLndMieS, uvb, Nc, Wc);
	o.atn = smaple3D(tLndAtn, tLndAtnS, uvb, Nc, Wc);
	return o;
}

fragment float4 LandView(ScatterFrag frg [[stage_in]], constant ScatterFlowPS &cb [[buffer(0)]],
	texture2d<float> tSun [[texture(6)]], sampler tSunS [[sampler(6)]])
{
	constant AtmoParams &Const = cb.Const;
	constant sFlow &Flo = cb.Flo;
	float u = frg.x, v = frg.y;

	u *= Nc;
	float a = floor(u) / Nc;
	float alt = mix(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	float3 vNrm = uvToNrm(Const, float2(fract(u), v));
	float3 vVrt = vNrm * r;

	float3 vCam = vVrt - Const.CamPos;
	float3 vRay = normalize(vCam);
	float cpd = abs(dot(vRay, vCam));

	RayData sp = ComputeRayStats(Const, vRay, true);

	float s0 = sp.ae > 0 ? sp.ae : 0;
	float e0 = sp.se > 0 ? min(cpd, sp.se) : cpd;

	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	float4 ret = IntegrateSegmentNS(Const, Flo, tSun, tSunS, vVrt - vRay * (cpd - s0), vRay, e0 - s0, iH);

	ret.rgb *= Const.cSun;
	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;

	return float4(ret.rgb, 0.0f);
}

fragment float4 LandViewAtten(ScatterFrag frg [[stage_in]], constant ScatterConstPS &cb [[buffer(0)]])
{
	constant AtmoParams &Const = cb.Const;
	float u = frg.x, v = frg.y;

	u *= Nc;
	float a = floor(u) / Nc;
	float alt = mix(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	float3 vNrm = uvToNrm(Const, float2(fract(u), v));
	float3 vVrt = vNrm * r;

	float3 vCam = vVrt - Const.CamPos;
	float3 vRay = normalize(vCam);

	float ang = dot(vNrm, vRay);
	float len = RayLength(Const, ang, r);

	float dist = min(len, dot(vRay, vCam));

	float3 ret = ComputeCameraView(Const, ang, r, dist);

	return float4(ret, 1.0);
}
