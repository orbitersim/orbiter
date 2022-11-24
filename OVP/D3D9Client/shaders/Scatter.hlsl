
// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under LGPL v2
// Copyright (C) 2022 Jarmo Nikkanen
// ============================================================================

#define Nc  16		//Z-dimension count in 3D texture
#define Wc  80		//3D texture size (pixels)
#define Qc  96		//2D texture size (pixels)


#define NSEG 6
#define iNSEG 1.0f / NSEG
#define MINANGLE -0.3f			// Minumum angle
#define ANGRNG (1.0f - MINANGLE)
#define iANGRNG (1.0f / ANGRNG)
#define xANGRNG (2.0f * ANGRNG)
#define ixANGRNG (1.0f / xANGRNG)
#define PI 3.14159265
#define BOOL bool



#define GLARE_SIZE 4.0f
#define MieMin  0.01f

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
	float3 Up;					// Sun Ref Frame (Unit Vector) (Up, toSun, ZeroAz)
	float3 vTangent;			// Reference frame for narmal mapping (Unit Vector)
	float3 vBiTangent;			// Reference frame for narmal mapping (Unit Vector)
	float3 vPolarAxis;			// North Pole (unit vector)
	float3 cSun;				// Sun Color and intensity
	float3 RayWave;				// .rgb Rayleigh Wave lenghts
	float3 MieWave;				// .rgb Mie Wave lenghts
	float3 GlareColor;
	float4 HG;					// Henyey-Greenstein Phase function params
	float2 iH;					// Inverse scale height for ray(.r) and mie(.g) exp(-altitude * iH) 
	float2 rmO;					// Ray and Mie out-scatter factors
	float2 rmI;					// Ray and Mie in-scatter factors
	float  RayPh;				// Phase
	float  PlanetRad;			// Planet Radius
	float  PlanetRad2;			// Planet Radius Squared
	float  AtmoAlt;				// Atmospehere upper altitude limit
	float  AtmoRad;				// Atmospehere outer radius
	float  AtmoRad2;			// Atmospehere outer radius squared
	float  CloudAlt;			// Cloud layer altitude 
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
	float  Clouds;
	float  Glare;
	float  TW_Multi;
	float  TW_Dst;
	float  SunRadAtHrz;
	float  CosAlpha;			// Cosine of camera horizon angle i.e. PlanetRad/CamRad
	float  SinAlpha;
	float  CamSpace;			// Camera in space scale factor 0.0 = surf, 1.0 = space
	float  Cr2;					// Camera radius on shadow plane (dot(cp.toCam, cp.Up) * cp.CamRad)^2
	float  ShdDst;
};

struct sFlow {
	BOOL bRay;					// True for rayleigh render pass
	BOOL bCamLit;				// True if camera is lit by sunlight
	BOOL bCamInSpace;			// True if camera is in space (i.e. not in atmosphere)
};

uniform extern AtmoParams Const;
uniform extern sFlow Flo;
uniform extern bool bInSpace;

sampler2D tSun;
sampler2D tCam;
sampler2D tLndRay;
sampler2D tLndMie;
sampler2D tLndAtn;
sampler2D tSunGlare;
sampler2D tAmbient;
sampler2D tSkyRayColor;
sampler2D tSkyMieColor;


static const float n[] = { 0.0714, 0.21428, 0.35714, 0.5, 0.64285, 0.78571, 0.92857 };
static const float w[] = { 0.1295, 0.27971, 0.38183, 0.41796, 0.38183, 0.27971, 0.1295 };

static const float4 n0 = float4(0.0714, 0.21428, 0.35714, 0.5 );
static const float4 w0 = float4(0.1295, 0.27971, 0.38183, 0.41796);
static const float4 n1 = float4(0.64285, 0.78571, 0.92857, 0 );
static const float4 w1 = float4(0.38183, 0.27971, 0.1295, 0 );

static const float4 n4 = float4( 0.06943, 0.33001, 0.66999, 0.93057 );
static const float4 w4 = float4( 0.34786, 0.65215, 0.65215, 0.34786 );


float2 NrmToUV(float3 vNrm)
{
	float2 uv = float2(dot(Const.ZeroAz, vNrm), dot(Const.SunAz, vNrm)) / Const.AngCtr;
	return uv * 0.5 + 0.5;
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
	const float ipix = 1.0f / (rc * prc - 1.0f);

	float z = uv.z * (rc - 1) * 0.999999f;
	uv = saturate(uv);
	uv.x *= prc * ipix - ipix;
	uv.x += prc * floor(z) * ipix;

	return uv;
}


// Sample a 3D texture composed from an array of 2D textures
//
float4 smaple3D(sampler2D tSamp, float3 uv, const float rc, const float pix)
{
	const float x = pix / (rc * pix - rc);

	float4 a = tex2D(tSamp, uv.xy).rgba;
	float4 b = tex2D(tSamp, uv.xy + float2(x, 0)).rgba;
	return lerp(a, b, frac(uv.z));
}



// Optical depth integral from point in atmosphere to infinity.
//
float2 Gauss7(float cos_dir, float r0, float2 ih0)
{
	int i;
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	float Ray = sqrt(Const.AtmoRad2 - z2) - y; // Length of the ray

	// Compute altitudes of sample points
	float4 p0 = Ray * n0 + y;
	float4 a0 = sqrt(z2 + p0 * p0) - Const.PlanetRad;
	float4 p1 = Ray * n1 + y;
	float4 a1 = sqrt(z2 + p1 * p1) - Const.PlanetRad;

	float2 sum = 0.0f;
	for (i = 0; i < 4; i++) sum += exp(-clamp(a0[i] * ih0, -20, 20)) * w0[i];
	for (i = 0; i < 3; i++) sum += exp(-clamp(a1[i] * ih0, -20, 20)) * w1[i];
	return sum * Ray * 0.5f;
}


// Optical depth integral in atmosphere for a given distance
//
float2 Gauss7(float cos_dir, float r0, float dist, float2 ih0)
{
	int i;

	// Compute altitudes of sample points
	float4 d0 = dist * n0;
	float4 a0 = sqrt(r0 * r0 + d0 * d0 - 2.0 * r0 * d0 * cos_dir) - Const.PlanetRad;
	float4 d1 = dist * n1;
	float4 a1 = sqrt(r0 * r0 + d1 * d1 - 2.0 * r0 * d1 * cos_dir) - Const.PlanetRad;
	
	float2 sum = 0.0f;
	for (i = 0; i < 4; i++) sum += exp(-clamp(a0[i] * ih0, -20, 20)) * w0[i];
	for (i = 0; i < 3; i++) sum += exp(-clamp(a1[i] * ih0, -20, 20)) * w1[i];
	return sum * dist * 0.5f;
}


// Optical depth integral from point in atmosphere to infinity.
//
float2 Gauss4(float cos_dir, float r0, float2 ih0)
{
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	float Ray = sqrt(Const.AtmoRad2 - z2) - y; // Length of the ray

	// Compute altitudes of sample points
	float4 p0 = Ray * n4 + y;
	float4 a0 = sqrt(z2 + p0 * p0) - Const.PlanetRad;

	float2 sum = 0.0f;
	for (int i = 0; i < 4; i++) sum += exp(-clamp(a0[i] * ih0, -20, 20)) * w4[i];
	return sum * Ray * 0.5f;
}

// Optical depth integral in atmosphere for a given distance
//
float2 Gauss4(float cos_dir, float r0, float dist, float2 ih0)
{
	// Compute altitudes of sample points
	float4 d0 = dist * n4;
	float4 a0 = sqrt(r0 * r0 + d0 * d0 - 2.0 * r0 * d0 * cos_dir) - Const.PlanetRad;

	float2 sum = 0.0f;
	for (int i = 0; i < 4; i++) sum += exp(-clamp(a0[i] * ih0, -20, 20)) * w4[i];
	return sum * dist * 0.5f;
}


// Rayleigh phase function
//
float RayPhase(float cw)
{
	return 0.25f * (4.0f + cw * cw) / (1.0f + Const.RayPh * cw);
}


// Henyey-Greenstein Phase function
//
float MiePhase(float cw)
{
	float cw2 = cw * cw;
	return Const.HG.x * (1.0f + cw2) * pow(abs(Const.HG.y - Const.HG.z * cw2*cw), -1.5f) + Const.HG.w;
}


float MiePhase2(float cw, float g)
{
	float cw2 = cw * cw;
	return (1.0f - g*g) * (1.0f + cw2) * pow(abs(1.0f + g * g - 2.0f * g * cw), -1.5f);
}


// Get a color of sunlight for a given altitude and normal-sun angle
//
float3 GetSunColor(float dir, float alt)
{
	alt = saturate((alt - Const.MinAlt) / Const.AtmoAlt);
	dir = (dir - MINANGLE) * iANGRNG;
	alt = sqrt(abs(alt)) * sign(alt);
	return tex2D(tSun, float2(dir, alt)).rgb;
}


// Compute attennuation from one point in atmosphere to camera
//
float3 Transmission(float3 vRay, float to, float from)
{
	float3 vP = Const.CamPos + vRay * from;
	float3 vF = normalize(vP);
	float a = dot(vF, vRay);
	float r = dot(vF, vP);
	float2 rm = Gauss7(-a, r, abs(to - from), Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}

float3 ComputeCameraView(float a, float r, float d)
{
	float2 rm = Gauss4(a, r, d, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}



// Approximate multi-scatter effect to atmospheric color and light travel behind terminator
//
float3 MultiScatterApprox(float3 vNrm)
{
	float dNS = clamp(dot(vNrm, Const.toSun) + Const.TW_Dst, 0.0f, Const.TW_Dst);
	return (Const.RayWave + 0.3f) * Const.TW_Multi * dNS;
}


struct RayData {
	float se;	// Distance to 'Shadow entry' point from a camera
	float sx;	// Shadow exit
	float ae;	// Atmosphere entry
	float ax;	// Atmosphere exit
	float hd;	// Horizon distance from a camera
	float w2;	
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
	float w  = sqrt(w2);
	float k  = sqrt(k2) * sign(a);
	float v2 = 0;

	dat.w2 = w2;
	dat.se = k - w;
	dat.sx = dat.se + 2.0f * w;
	dat.hd = Const.ShdDst;

	// Project distances back to 3D space
	float q = rsqrt(max(2.5e-5, 1.0 - z * z));
	dat.se *= q;
	dat.sx *= q;
	dat.hd *= q;


	// Compute atmosphere entry and exit points 
	//
	a = -dot(Const.toCam, vRay);
	k2 = Const.CamRad2 * a * a;
	h2 = Const.CamRad2 - k2;
	v2 = Const.AtmoRad2 - h2;
	w = sqrt(v2);
	k = sqrt(k2) * sign(a);

	dat.ae = (k - w);
	dat.ax = dat.ae + 2.0f * w;

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
float4 IntegrateSegmentNS(float3 vOrig, float3 vRay, float len, float iH)
{
	// TODO: Try accummulation of CamView seg. by seg.
	// ===============================

	float4 ret = 0;
	for (int i = 0; i < NSEG; i++)
	{
		float dst = len * iNSEG * (float(i) + 0.5f);
		float3 pos = vOrig + vRay * dst;
		float3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		float3 x = GetSunColor(dot(n, Const.toSun), alt); // +MultiScatterApprox(n);
		x *= ComputeCameraView(pos, n, vRay, rad);
		float f = exp(-alt * iH) * iNSEG;
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}



// 2D lookup-table, for direct sunlight being filtered by atmosphere
//
float4 SunColor(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float alt = lerp(Const.MinAlt, Const.AtmoAlt, y*y);
	float ang = x * ANGRNG + MINANGLE;

	float2 rm = Gauss7(ang, alt + Const.PlanetRad, Const.iH) * Const.rmO;
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

	// Check mid-point validity
	float mp = sp.hd > sp.ae && sp.hd < sp.ax ? sp.hd : -1e6;
	if (sp.w2 < Const.PlanetRad2) mp = -1e6;

	float s0 = max(0, sp.ae);
	float e0 = sp.se > 0 ? sp.se : mp;
	float s1 = max(max(mp, sp.sx), s0);
	float e1 = sp.ax;

	// First segment
	float4 ret = 0;
	if (e0 > s0)
		ret += IntegrateSegmentNS(Const.CamPos + vRay * s0, vRay, e0 - s0, iH);
	// Second segment
	if (e1 > s1)
		ret += IntegrateSegmentNS(Const.CamPos + vRay * s1, vRay, e1 - s1, iH);

	
	float3 transmission = Transmission(vRay, sp.ax, 0);

	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	float alpha = 1.0;// -saturate(transmission.g);

	return float4(ret.rgb, alpha);
}



// 2D lookup-table for pre-computed sky color. (pre frame), varies with camera pos and sun pos
//
float4 RingView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
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
	
	// Check mid-point validity
	float mp = sp.hd > sp.ae && sp.hd < sp.ax ? sp.hd : -1e6;

	if (sp.w2 < Const.PlanetRad2) mp = -1e6;

	float s0 = max(0, sp.ae);
	float e0 = sp.se > 0 ? sp.se : mp;
	float s1 = max(max(mp, sp.sx), s0);
	float e1 = sp.ax;
	
	
	// First segment
	float4 ret = 0;
	if (e0 > s0)
		ret += IntegrateSegmentNS(vPos - vRay * (cpd - s0), vRay, e0 - s0, iH);
	// Second segment
	if (e1 > s1)
		ret += IntegrateSegmentNS(vPos - vRay * (cpd - s1), vRay, e1 - s1, iH);


	float3 transmission = Transmission(vRay, sp.ax, max(0, sp.ae));

	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	/*if (Flo.bRay)
	{
		ret.rgb += MultiScatterApprox(normalize(vPos)) * exp(-e * Const.iH.r * 0.5f);
	}*/

	float alpha = 1.0f;// -saturate(transmission.g);

	return float4(ret.rgb, alpha);
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
	float3 cMlt = MultiScatterApprox(Const.toCam);

	float3 color = ray * RayPhase(ph) + mie * MiePhase(ph) + cMlt;

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
	// TODO: CLAMP u,v IN PROPER RANGE
	// ===============================

	u *= Nc;
	float a = floor(u) / Nc;
	float alt = lerp(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	float3 vNrm = uvToNrm(float2(frac(u), v));
	float3 vVrt = vNrm * r;

	// Viewing ray
	float3 vCam = Const.CamPos - vVrt;
	float3 vRay = -normalize(vCam); // From camera to vertex
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





// Render Sun Texture "Glare"
//
float4 RenderSun(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u = u * 2.0 - 1.0;
	v = v * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.5f * pow(sin(3.0f * a), 4.0f);
	float w = 0.5f + 0.3f * pow(sin(30.0f * a), 2.0f) * pow(sin(41.0f * a), 2.0f);

	//float I = pow(max(0, 2.0f * (1 - r / q)), 12.0f);
	//float K = pow(max(0, 2.0f * (1 - r / w)), 12.0f);

	float I = pow(max(0, (1 - r / q)), 6.0f) * 8e4;
	float K = pow(max(0, (1 - r / w)), 6.0f) * 16e4;

	//float I = exp(max(0, 10.0f * (1 - r / q))) - 1.0f;
	//float K = exp(max(0, 10.0f * (1 - r / w))) - 1.0f;

	float L = exp(max(0, 0.35f - r) * 43.0f);

	return float4(max(I+L,K+L), 0, 0, 1);
}
