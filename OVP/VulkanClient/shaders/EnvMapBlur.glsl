

layout(binding = 1, row_major, scalar) uniform EnvMapBlurPS	// uniform extern: pixel shader constants (IPI.glsl's are binding 0)
{
vec3  vDir;
vec3  vUp;
vec3  vCp;
float   fD;
bool    bDir;
};

layout(binding = 4) uniform samplerCube tCube;	// s0
layout(binding = 5) uniform sampler2D tSrc;	// s1

float coeff[8] = float[8]( 0.13298076, 0.125794409, 0.106482669, 0.080656908, 0.054670025, 0.033159046, 0.017996989, 0.00874063 );

vec4 PSBlur(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	x = x * 2.0f - 1.0f;
	y = y * 2.0f - 1.0f;

	vec3 vD;

	vec3 dir = vDir - vUp*y + vCp*x;

	if (bDir) vD = cross(dir, vCp);
	else	  vD = cross(dir, vUp);

	vD = normalize(vD) * fD;

	vec3 color = texture(tCube, dir).rgb;
	vec3 vX = vec3(0);
	float f = 0.75f;
	float a = 0.5f;

	for (int i = 1; i < 16; i++) {
		vX += vD;
		color += f * texture(tCube, dir + vX).rgb;
		color += f * texture(tCube, dir - vX).rgb;
		a += f;
		f *= 0.75f;
	}
	color /= (a*2.0f);
	return vec4(color, 1);
}

#ifdef PS_PSBlur
layout(location = 0) in float iX;	// TEXCOORD0 of IPI.glsl
layout(location = 1) in float iY;	// TEXCOORD1
layout(location = 0) out vec4 oColor;
void main() { oColor = PSBlur(iX, iY); }
#endif
