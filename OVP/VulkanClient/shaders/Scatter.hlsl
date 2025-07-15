
// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under MIT
// Copyright (C) 2022 Jarmo Nikkanen
// ============================================================================

#if defined(_PERFORMANCE) // DO NOT CHANGE THESE, MUST MATCH WITH C++ CODE
#define Nc  6		//Z-dimension count in 3D texture
#define Wc  72		//3D texture size (pixels)
#define Qc  64		//2D texture size (pixels)
#else
#define Nc  8		//Z-dimension count in 3D texture
#define Wc  128		//3D texture size (pixels)
#define Qc  96		//2D texture size (pixels)
#endif


#define NSEG 5
#define iNSEG 1.0f / NSEG
#define MINANGLE -0.33f			// Minumum angle
#define ANGRNG (0.25f - MINANGLE)
#define iANGRNG (1.0f / ANGRNG)
#define BOOL bool
#define LastLine (0.999999f - 1.0f / Qc)


// Per-Frame Params
//
struct AtmoParams
{
	float4x4 mVP;				// View Projection Matrix
	float3 CamPos;				// Geocentric Camera position
	float3 toCam;				// Geocentric Camera direction (unit vector)
	float3 toSun;				// Geocentric Sun direction (unit vector)
	float3 SunAz;				// Atmo scatter ref.frame (unit vector) (toCam, ZeroAz, SunAz)
	float3 ZeroAz;				// Atmo scatter ref.frame (unit vector)
	float3 Up;					// Sun/Shadow Ref Frame (Unit Vector) (Up, toSun, ZeroAz)
	float3 vTangent;			// Reference frame for normal mapping (Unit Vector)
	float3 vBiTangent;			// Reference frame for normal mapping (Unit Vector)
	float3 vPolarAxis;			// North Pole (unit vector)
	float3 cSun;				// Sun Color and intensity
	float3 RayWave;				// .rgb Rayleigh Wave lenghts
	float3 MieWave;				// .rgb Mie Wave lenghts
	float4 HG;					// Henyey-Greenstein Phase function params
	float2 iH;					// Inverse scale height for ray(.r) and mie(.g) e.g. exp(-altitude * iH) 
	float2 rmO;					// Ray and Mie out-scatter factors
	float2 rmI;					// Ray and Mie in-scatter factors
	float3 cAmbient;			// Ambient light color at sealevel
	float3 cGlare;				// Sun glare color
	float  PlanetRad;			// Planet Radius
	float  PlanetRad2;			// Planet Radius Squared
	float  AtmoAlt;				// Atmospehere upper altitude limit
	float  AtmoRad;				// Atmospehere outer radius
	float  AtmoRad2;			// Atmospehere outer radius squared
	float  CloudAlt;			// Cloud layer altitude for color and light calculations (not for phisical rendering) 
	float  MinAlt;				// Minimum terrain altitude
	float  MaxAlt;				// Maximum terrain altitude
	float  iAltRng;				// 1.0 / (MaxAlt - MinAlt);
	float  AngMin;
	float  AngRng;
	float  iAngRng;
	float  AngCtr;				// Cos of View cone angle from planet center that's visible from camera location 
	float  HrzDst;				// Distance to horizon (500 m) minimum if camera below sea level
	float  CamAlt;				// Camera Altitude
	float  CamElev;				// Camera Elevation above surface
	float  CamRad;				// Camera geo-distance
	float  CamRad2;				// Camera geo-distance squared
	float  Expo;				// "HDR" exposure factor (atmosphere only)
	float  Time;				// Simulation time / 180
	float  TrGamma;				// Terrain "Gamma" correction setting
	float  TrExpo;				// "HDR" exposure factor (terrain only)
	float  Ambient;				// Global ambient light level
	float  Clouds;				// Cloud layer intensity (if below), and Blue light inscatter scale factor (if camera Above clouds)
	float  TW_Terrain;			// Twilight intensity
	float  TW_Dst;				// Twilight distance behind terminator
	float  CosAlpha;			// Cosine of camera horizon angle i.e. PlanetRad/CamRad
	float  SinAlpha;			// Sine of ^^
	float  CamSpace;			// Camera in space scale factor 0.0 = surf, 1.0 = space
	float  Cr2;					// Camera radius on shadow plane (dot(cp.toCam, cp.Up) * cp.CamRad)^2
	float  ShdDst;
	float  SunVis;
	float  dCS;
	float  smi;
	float  ecc;
	float  trLS;
	float  wNrmStr;				// Water normal strength
	float  wSpec;				// Water smoothness
	float  wBrightness;
	float  wBoost;
};

struct sFlow {
	BOOL bRay;					// True for rayleigh render pass
	BOOL bCamLit;				// True if camera is lit by sunlight
	BOOL bCamInSpace;			// True if camera is in space (i.e. not in atmosphere)
};

uniform extern AtmoParams Const;
uniform extern sFlow Flo;

sampler2D tSun;
sampler2D tCam;
sampler2D tLndRay;
sampler2D tLndMie;
sampler2D tLndAtn;
sampler2D tSunGlare;
sampler2D tAmbient;
sampler2D tSkyRayColor;
sampler2D tSkyMieColor;

#if NSEG == 5
static const float n[] = { 0.050, 0.25, 0.50, 0.75, 0.950 };
static const float w[] = { 0.125, 0.25, 0.25, 0.25, 0.125 };
#endif

#if NSEG == 7
static const float n[] = { 0.05, 0.167, 0.333, 0.500, 0.667, 0.833, 0.95 };
static const float w[] = { 0.08, 0.167, 0.167, 0.167, 0.167, 0.167, 0.08 };
#endif

// Gauss7 points and weights
static const float4 n0 = float4(0.0714, 0.21428, 0.35714, 0.5 );
static const float4 w0 = float4(0.1295, 0.27971, 0.38183, 0.41796);
static const float4 n1 = float4(0.64285, 0.78571, 0.92857, 0 );
static const float4 w1 = float4(0.38183, 0.27971, 0.1295, 0 );

// Gauss4 points and weights
static const float4 n4 = float4( 0.06943, 0.33001, 0.66999, 0.93057 );
static const float4 w4 = float4( 0.34786, 0.65215, 0.65215, 0.34786 );

float ilerp(float a, float b, float x)
{
	return saturate((x - a) / (b - a));
}

float3 sqr(float3 x)
{
	return x * x;
}

float4 expc(float4 x) { return exp(clamp(x, -20, 20)); }
float3 expc(float3 x) { return exp(clamp(x, -20, 20)); }
float2 expc(float2 x) { return exp(clamp(x, -20, 20)); }
float  expc(float x)  { return exp(clamp(x, -20, 20)); }


float2 NrmToUV(float3 vNrm)
{
	float2 uv = float2(dot(Const.ZeroAz, vNrm), dot(Const.SunAz, vNrm)) / Const.AngCtr;
	return (uv * 0.5 + 0.5);
}


float3 uvToLoc(float2 uv, float r)
{
	uv = (uv * 2.0 - 1.0);
	float w = uv.x * uv.x + uv.y * uv.y;
	if (w > 1.0f) uv *= rsqrt(w);
	uv *= r;
	float det = 1.0f - uv.x * uv.x - uv.y * uv.y;
	float cos_ab = det > 0.0f ? sqrt(det) : 0.0f;
	return float3(uv.xy, cos_ab);
}


float3 uvToNrm(float2 uv)
{
	float3 q = uvToLoc(uv, Const.AngCtr);
	return normalize(Const.SunAz * q.y + Const.ZeroAz * q.x + Const.toCam * q.z);
}


float3 uvToDir(float2 uv)
{
	uv.y = uv.y * rsqrt(0.2f + uv.y * uv.y) * sqrt(1.2f);

	float x = uv.x * 2.0f - 1.0f;
	float y = sqrt(1.0f - x * x);
	float z = 1.0f - uv.y * Const.AngRng;
	float k = sqrt(1.0f - z * z);

	return normalize(Const.SunAz * x * k + Const.ZeroAz * y * k + Const.toCam * z);
}


float2 DirToUV(float3 uDir)
{
	float y = dot(uDir, Const.toCam);
	float3 uOrt = normalize(uDir - Const.toCam * y);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	float2 uv = float2(x, 1.0f - (y - Const.AngMin) * Const.iAngRng);
	uv.y = sqrt(0.2f) * uv.y * rsqrt(1.2f - uv.y * uv.y);
	return uv;
}


float3 HDR(float3 x)
{
	return 1.0f - exp(-Const.Expo * x);
}


float3 LightFX(float3 x)
{
	//return x * rsqrt(1.0f + x*x) * 1.4f;
	return 2.0 * x / (1.0f + x);
}


float RayLength(float cos_dir, float r0, float r1)
{
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	return sqrt(r1 * r1 - z2) + y;
}


float RayLength(float cos_dir, float r0)
{
	return RayLength(cos_dir, r0, Const.AtmoRad);
}


// Compute UV and blend factor for smaple3D routine
//
float3 TransformUV(float3 uv, const float rc, const float prc)
{
	const float ipix = 1.0f / rc;

	uv = saturate(uv);
	uv.z *= (rc - 1);
	uv.x *= ipix;
	uv.x += floor(uv.z) * ipix;
	return uv;
}


// Sample a 3D texture composed from an array of 2D textures
//
float4 smaple3D(sampler2D tSamp, float3 uv, const float rc, const float pix)
{
	const float x = 1.0f / rc;

	float4 a = tex2D(tSamp, uv.xy).rgba;
	float4 b = tex2D(tSamp, uv.xy + float2(x, 0)).rgba;
	return lerp(a, b, frac(uv.z));
}






// Optical depth integral in atmosphere for a given distance
//
float2 Gauss7(float cos_dir, float r0, float dist, float2 ih0)
{
	int i;
	float x = 2.0 * r0 * cos_dir;
	float r2 = r0 * r0;

	// Compute altitudes of sample points
	float4 d0 = dist * n0;
	float4 a0 = sqrt(r2 + d0 * (d0 - x)) - Const.PlanetRad;
	float4 d1 = dist * n1;
	float4 a1 = sqrt(r2 + d1 * (d1 - x)) - Const.PlanetRad;
	
	float2 sum = 0.0f;
	for (i = 0; i < 4; i++) sum += expc(-a0[i] * ih0) * w0[i];
	for (i = 0; i < 3; i++) sum += expc(-a1[i] * ih0) * w1[i];
	return sum * dist * 0.5f;
}


// Optical depth integral in atmosphere for a given distance
//
float2 Gauss4(float cos_dir, float r0, float dist, float2 ih0)
{
	float4 d0 = dist * n4;
	float4 a0 = sqrt(r0 * r0 + d0 * d0 - 2.0 * r0 * d0 * cos_dir) - Const.PlanetRad;
	float4 ray = expc(-a0 * ih0.x);
	float4 mie = expc(-a0 * ih0.y);
	return float2(dot(ray, w4), dot(mie, w4)) * dist * 0.5f;
}


// Rayleigh phase function
//
float RayPhase(float cw)
{
	return 0.25f * (4.0f + cw * cw);
}


// Henyey-Greenstein Phase function
//
/*float MiePhase(float cw)
{
	float cw2 = cw * cw;
	return Const.HG.x * (1.0f + cw2) * pow(abs(Const.HG.y - Const.HG.z * cw2*cw), -1.5f) + Const.HG.w;
}*/

float MiePhase(float cw)
{
	return 8.0f * Const.HG.x / (1.0f - Const.HG.y * cw) + Const.HG.w;
}


// Get a color of sunlight for a given altitude and normal-sun angle
//
float3 GetSunColor(float dir, float alt)
{
	float maxalt = max(Const.MaxAlt, Const.CloudAlt);
	alt = ilerp(Const.MinAlt, maxalt, alt);
	dir = saturate((dir - MINANGLE) * iANGRNG);
	alt = sqrt(alt);
	return tex2D(tSun, float2(dir, alt)).rgb;
}


float3 ComputeCameraView(float a, float r, float d)
{
	float2 rm = Gauss7(a, r, d, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}



// Approximate multi-scatter effect to atmospheric color and light travel behind terminator
//
float4 AmbientApprox(float dNS, uniform const bool bR = true)
{
	float fA = 1.0f - smoothstep(0.0f, Const.TW_Dst, -dNS);
	float3 clr = (bR ? Const.RayWave : Const.cAmbient);
	return float4(clr, fA);
}

float4 AmbientApprox(float3 vNrm, uniform const bool bR = true)
{
	float dNS = dot(vNrm, Const.toSun);
	return AmbientApprox(dNS, bR);
}



struct RayData {
	float se;	// Distance to 'Shadow entry' point from a camera
	float sx;	// Shadow exit
	float ae;	// Atmosphere entry
	float ax;	// Atmosphere exit
	float hd;	// Horizon distance from a camera
	float ca;	// Closest approach distance	
};

struct IData {
	float s0, s1, e0, e1;
};


// Compute ray passage information
// vRay must point away from the camera
//
RayData ComputeRayStats(in float3 vRay, in uniform const bool bPreProcessData)
{
	RayData dat = (RayData)0;

	static const float invalid = -1e9;

	// Projection of viewing ray on 'shadow' axes
	float u = dot(vRay, Const.Up);
	float t = dot(vRay, Const.ZeroAz);
	float z = dot(vRay, Const.toSun);

	// Shadow Entry and Exit points
	// Cosine 'a'
	float a = u * rsqrt(u * u + t * t);

	float k2 = Const.Cr2 * a * a;
	float h2 = Const.Cr2 - k2;
	float w2 = Const.PlanetRad2 - h2;
	float2 b = sqrt(float2(w2, k2));
	float k  = b.y * sign(a);
	float v2 = 0;
	float m  = Const.CamRad2 - Const.PlanetRad2;

	dat.se = k - b.x;
	dat.sx = dat.se + 2.0f * b.x;
	
	// Project distances back to 3D space
	float q = rsqrt(max(2.5e-5, 1.0 - z * z));
	dat.se *= q;
	dat.sx *= q;

	// Compute atmosphere entry and exit points 
	//
	a = -dot(Const.toCam, vRay);
	k2 = Const.CamRad2 * a * a;
	h2 = Const.CamRad2 - k2;
	v2 = Const.AtmoRad2 - h2;
	float3 n = sqrt(float3(v2, k2, m));
	k = n.y * sign(a);

	dat.hd  = m > 0.0 ? n.z : 0.0;
	dat.ae = (k - n.x);
	dat.ax = dat.ae + 2.0f * n.x;
	dat.ca = Const.CamRad * a;

	// If the ray doesn't intersect atmosphere then set both distances to zero
	if (v2 < 0) dat.ae = dat.ax = invalid;

	// If the ray doesn't intersect shadow then set both distances to atmo exit
	if (w2 < 0) dat.se = dat.sx = invalid;

	if (bPreProcessData)
	{
		float3 vEn = Const.CamPos + vRay * dat.se;
		float3 vEx = Const.CamPos + vRay * dat.sx;

		// If shadow entry/exit point is Lit then set it to atmo exit point
		if (dot(vEn, Const.toSun) > 0) dat.se = invalid;
		if (dot(vEx, Const.toSun) > 0) dat.sx = invalid;	
	}

	return dat;
}


IData PostProcessData(RayData sp)
{
	IData d;
	if (!Flo.bCamLit) {	// Camera in Shadow
		d.s0 = max(sp.sx, sp.ae);

		float lf = max(0, sp.ca) / max(1.0f, abs(sp.hd)); // Lerp Factor
		float mp = lerp((sp.ax + d.s0) * 0.5f, sp.hd, saturate(lf));

		d.e0 = mp;
		d.s1 = max(sp.sx, mp);
		d.e1 = sp.ax;
	}
	else { // Camera is Lit
		d.s0 = max(0, sp.ae);

		float lf = max(0, sp.ca) / max(1.0f, abs(sp.hd)); // Lerp Factor
		float mp = lerp((sp.ax + d.s0) * 0.5f, sp.hd, saturate(lf));

		bool bA = (sp.se > sp.ax || sp.se < 0);
		
		d.e0 = bA ? mp : sp.se;
		d.s1 = bA ? mp : max(sp.sx, sp.ae);
		d.e1 = sp.ax;
	}
	return d;
}

// Compute attennuation from vPos in atmosphere to camera (or atm exit point)
//
float3 ComputeCameraView(float3 vPos, float3 vNrm, float3 vRay, float r)
{
	float d;
	float a = dot(vNrm, vRay);
	if (Flo.bCamInSpace) d = RayLength(a, r);
	else d = dot(vPos - Const.CamPos, vRay);
	float2 rm = Gauss7(a, r, d, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}

// Integrate viewing ray for incatter color (.rgb) and optical depth (.a)
// vRay must point from camera to vOrig
//
float4 IntegrateSegmentMP(float3 vOrig, float3 vRay, float len, float iH)
{
	float4 ret = 0;
	float3 vR = vRay * len;
	for (int i = 0; i < NSEG; i++)
	{
		float3 pos = vOrig + vR * (iNSEG * (float(i) + 0.5f));
		float3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		float3 x = GetSunColor(dot(n, Const.toSun), alt);
		x *= ComputeCameraView(pos, n, vRay, rad);
		float f = exp(-alt * iH) * iNSEG;
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}

float4 IntegrateSegmentNS(float3 vOrig, float3 vRay, float len, float iH)
{
	// TODO: Could try to accummulation of CamView seg. by seg.

	float4 ret = 0;
	float3 vR = vRay * len;
	for (int i = 0; i < NSEG; i++)
	{
		float3 pos = vOrig + vR * n[i];
		float3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		float3 x = GetSunColor(dot(n, Const.toSun), alt);
		x *= ComputeCameraView(pos, n, vRay, rad);
		float f = exp(-alt * iH) * w[i];
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}



// 2D lookup-table, for direct sunlight being filtered by atmosphere
//
float4 SunColor(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float maxalt = max(Const.MaxAlt, Const.CloudAlt);
	float alt = lerp(Const.MinAlt, maxalt, y*y);
	float ang = x * ANGRNG + MINANGLE;
	float rad = alt + Const.PlanetRad;
	float dist = RayLength(-ang, rad);
	float2 rm = Gauss7(-ang, rad, dist, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;

	return float4(exp(-clr), 1.0f);
}


struct SkyOut
{
	float4 ray;
	float4 mie;
};


// Get a precomputed rayleight and mie color values for a given direction from a camera
//
SkyOut GetSkyColor(float3 uDir)
{
	float2 uv = DirToUV(uDir);

	SkyOut o;
	o.ray = tex2D(tSkyRayColor, uv).rgba;
	o.mie = tex2D(tSkyMieColor, uv).rgba;
	return o;
}



float4 SkyView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	// Viewing ray
	float3 vRay = uvToDir(float2(u,v));

	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	RayData sp = ComputeRayStats(vRay, true);
	IData id = PostProcessData(sp);

	// First segment
	float4 ret = 0;
	if (id.e0 > id.s0)
		ret += IntegrateSegmentNS(Const.CamPos + vRay * id.s0, vRay, id.e0 - id.s0, iH);

	// Second segment
	if (id.e1 > id.s1)
		ret += IntegrateSegmentNS(Const.CamPos + vRay * id.s1, vRay, id.e1 - id.s1, iH);


	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	if (Flo.bRay)
	{
		float2 vDepth = Gauss7(dot(Const.toCam, -vRay), Const.CamRad, sp.ax, Const.iH);
		float2 vOut = vDepth * Const.rmO;
		float4 cMlt = AmbientApprox(Const.toCam);
		cMlt.rgb *= exp(-Const.CamAlt * Const.iH.r);
		cMlt.rgb *= cMlt.a;
		cMlt.rgb *= exp(-(Const.RayWave * vOut.r + Const.MieWave * vOut.g));
		float alpha = ilerp(10e3, 150e3, vDepth.r);
		return float4(ret.rgb + cMlt.rgb, alpha);
	}

	return float4(ret.rgb, 1.0f);
}



// 2D lookup-table for pre-computed sky color. (pre frame), varies with camera pos and sun pos
//
float4 RingView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	if (v >= LastLine) return float4(0, 0, 0, 0);

	v *= v;
	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH  = Flo.bRay ? Const.iH.r : Const.iH.g;

	float x = -1.0f + u * 2.0f;
	float y = sqrt(max(1e-6, 1.0f - x * x));
	float e = v * Const.AtmoAlt;
	float re = Const.PlanetRad + e;
	float z = re * Const.CosAlpha;
	float j = re * Const.SinAlpha;

	// Sample position and viewing ray
	float3 vPos = Const.SunAz * x * j + Const.ZeroAz * y * j + Const.toCam * z;
	float3 vRay = normalize(vPos - Const.CamPos);
	float cpd = abs(dot(vRay, Const.CamPos - vPos));

	RayData sp = ComputeRayStats(vRay, true);
	IData id = PostProcessData(sp);

	// First segment
	float4 ret = 0;
	if (id.e0 > id.s0)
		ret += IntegrateSegmentNS(vPos - vRay * (cpd - id.s0), vRay, id.e0 - id.s0, iH);

	// Second segment
	if (id.e1 > id.s1)
		ret += IntegrateSegmentNS(vPos - vRay * (cpd - id.s1), vRay, id.e1 - id.s1, iH);

	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	if (Flo.bRay)
	{
		float3 vSrc = vPos - vRay * (cpd - sp.ax);
		float3 vNrm = normalize(vSrc);
		float2 vDepth = Gauss4(dot(vNrm, vRay), dot(vSrc, vNrm), sp.ax - sp.ae, Const.iH);
		float2 vOut = vDepth * Const.rmO;
		float4 cMlt = AmbientApprox(Const.toCam);
		cMlt.rgb *= exp(-Const.CamAlt * Const.iH.r);
		cMlt.rgb *= cMlt.a;
		cMlt.rgb *= exp(-(Const.RayWave * vOut.r + Const.MieWave * vOut.g));
		float alpha = ilerp(10e3, 150e3, vDepth.r);
		alpha = alpha > 0 ? sqrt(alpha) : 0;
		return float4(ret.rgb + cMlt.rgb, alpha);
	}

	return float4(ret.rgb, 1.0f);
}


// Get a precomputed total (combined) sky color for a given direction from a camera
//
float3 GetAmbient(float3 vRay)
{
	return tex2D(tAmbient, DirToUV(vRay)).rgb;
}


// 2D lookup-table for pre-computed total (combined) sky color. (pre frame), varies with sun/cam relation
//
float4 AmbientSky(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	// Viewing ray
	float2 uv = float2(u, v);
	float3 uDir = uvToDir(uv);
	float  ph = dot(uDir, Const.toSun);
	
	float3 ray = tex2D(tSkyRayColor, uv).rgb;
	float3 mie = tex2D(tSkyMieColor, uv).rgb;
	float3 color = ray * RayPhase(ph) + mie * MiePhase(ph);

	return float4(color, 1.0f);
}



struct LandOut
{
	float4 ray;
	float4 mie;
	float4 atn;
};


// Get a precomputed rayleight and mie haze values for a given altitude and normal
//
LandOut GetLandView(float rad, float3 vNrm)
{
	float2 uv = NrmToUV(vNrm);

	float a = rad - Const.PlanetRad;
	float z = saturate((a - Const.MinAlt) * Const.iAltRng); // inverse lerp

	float3 uvb = TransformUV(float3(uv, z), Nc, Wc);

	LandOut o;
	o.ray = smaple3D(tLndRay, uvb, Nc, Wc);
	o.mie = smaple3D(tLndMie, uvb, Nc, Wc);
	o.atn = smaple3D(tLndAtn, uvb, Nc, Wc);
	return o;
}


// Render 3D Lookup texture for land view
//
float4 LandView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u *= Nc;
	float a = floor(u) / Nc;
	float alt = lerp(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	float3 vNrm = uvToNrm(float2(frac(u), v));
	float3 vVrt = vNrm * r;

	// Viewing ray
	float3 vCam = vVrt - Const.CamPos;
	float3 vRay = normalize(vCam); // From camera to vertex
	float cpd = abs(dot(vRay, vCam)); // Camera pixel distance
	
	RayData sp = ComputeRayStats(vRay, true);

	float s0 = sp.ae > 0 ? sp.ae : 0;
	float e0 = sp.se > 0 ? min(cpd, sp.se) : cpd;

	//if (!Flo.bRay) dist = min(dist, 10e3);

	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH  = Flo.bRay ? Const.iH.r : Const.iH.g;

	float4 ret = IntegrateSegmentNS(vVrt - vRay * (cpd - s0), vRay, e0 - s0, iH);

	ret.rgb *= Const.cSun;
	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;

	return float4(ret.rgb, 0.0f);
}



// Render 3D Lookup texture for land view attennuation
//
float4 LandViewAtten(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u *= Nc;
	float a = floor(u) / Nc;
	float alt = lerp(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	float3 vNrm = uvToNrm(float2(frac(u), v));
	float3 vVrt = vNrm * r;

	// Viewing ray
	float3 vCam = vVrt - Const.CamPos;
	float3 vRay = normalize(vCam); // Towards vertex from camera

	float ang = dot(vNrm, vRay);
	float len = RayLength(ang, r);

	float dist = min(len, dot(vRay, vCam));

	float3 ret = ComputeCameraView(ang, r, dist);

	return float4(ret, 1.0);
}
