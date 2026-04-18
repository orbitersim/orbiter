#define INTERPRETER_IMPLEMENTATION

#include "Interpreter.h"


// ============================================================================
// 

/***
Vector library functions.
@module vec
*/

void Interpreter::LoadVecMatAPI()
{
	// Load the vector library
	static const struct luaL_reg vecLib[] = {
		{"set", vec_set},
		{"add", vec_add},
		{"sub", vec_sub},
		{"mul", vec_mul},
		{"div", vec_div},
		{"dotp", vec_dotp},
		{"crossp", vec_crossp},
		{"length", vec_length},
		{"dist", vec_dist},
		{"unit", vec_unit},
		{NULL, NULL}
	};
	luaL_openlib (L, "vec", vecLib, 0);

	static const struct luaL_reg matLib[] = {
		{"identity", mat_identity},
		{"mul", mat_mul},
		{"tmul", mat_tmul},
		{"mmul", mat_mmul},
		{"rotm", mat_rotm},
		{NULL, NULL}
	};
	luaL_openlib (L, "mat", matLib, 0);
}

/***
Define a vector from its components.

You can also use standard Lua syntax to define the components of the vector
(vectors are defined as tables with fields 'x', 'y' and 'z').

The _V function provides a handier notation :
    v = _V(0,0,1)

@function set
@tparam number x
@tparam number y
@tparam number z
@treturn vector vector
@usage v = vec.set(x,y,z)
*/
int Interpreter::vec_set (lua_State *L)
{
	int i;
	VECTOR3 v;
	for (i = 0; i < 3; i++) {
		ASSERT_SYNTAX(lua_isnumber(L,i+1), "expected three numeric arguments");
		v.data[i] = lua_tonumber(L,i+1);
	}
	lua_pushvector(L,v);
	return 1;
}

/***
Sum of two vectors.

Each argument can be either a vector or a number.
The return value is a vector, unless both a and b are numbers

When operating on a number and a vector, the number is replaced with the vector {x=number, y=number, z=number}
@function add
@tparam (vector|number) a
@tparam (vector|number) b
@treturn (vector|number) result of a+b
@usage v = vec.add(a,b)
*/
int Interpreter::vec_add (lua_State *L)
{
	VECTOR3 va, vb;
	double fa, fb;
	if (lua_isvector(L,1)) {
		va = lua_tovector (L,1);
		if (lua_isvector(L,2)) {
			vb = lua_tovector (L,2);
			lua_pushvector (L, va+vb);
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			fb = lua_tonumber (L,2);
			lua_pushvector (L, _V(va.x+fb, va.y+fb, va.z+fb));
		}
	} else {
		ASSERT_SYNTAX (lua_isnumber(L,1), "Argument 1: expected vector or number");
		fa = lua_tonumber (L,1);
		if (lua_isvector (L,2)) {
			vb = lua_tovector (L,2);
			lua_pushvector (L, _V(fa+vb.x, fa+vb.y, fa+vb.z));
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			fb = lua_tonumber (L,2);
			lua_pushnumber (L, fa+fb);
		}
	}
	return 1;
}

/***
Difference of two vectors.

Each argument can be either a vector or a number.
The return value is a vector, unless both a and b are numbers

Substracting a number to a vector results in substracting the number to each component of the vector
@function sub
@tparam (vector|number) a
@tparam (vector|number) b
@treturn (vector|number) result of a-b
@usage v = vec.sub(a,b)
*/
int Interpreter::vec_sub (lua_State *L)
{
	VECTOR3 va, vb;
	double fa, fb;
	if (lua_isvector(L,1)) {
		va = lua_tovector (L,1);
		if (lua_isvector(L,2)) {
			vb = lua_tovector (L,2);
			lua_pushvector (L, va-vb);
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			fb = lua_tonumber (L,2);
			lua_pushvector (L, _V(va.x-fb, va.y-fb, va.z-fb));
		}
	} else {
		ASSERT_SYNTAX (lua_isnumber(L,1), "Argument 1: expected vector or number");
		fa = lua_tonumber (L,1);
		if (lua_isvector (L,2)) {
			vb = lua_tovector (L,2);
			lua_pushvector (L, _V(fa-vb.x, fa-vb.y, fa-vb.z));
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			fb = lua_tonumber (L,2);
			lua_pushnumber (L, fa-fb);
		}
	}
	return 1;
}

/***
Elementwise vector multiplication.

Each argument can be either a vector or a number.
The return value is a vector, unless both a and b are numbers

Multiplying a number with a vector results in multiplying the number with each component of the vector
@function mul
@tparam (vector|number) a
@tparam (vector|number) b
@treturn (vector|number) result of a*b
@usage v = vec.mul(a,b)
*/
int Interpreter::vec_mul (lua_State *L)
{
	VECTOR3 v1, v2, res;
	double f1, f2;
	int i;
	if (lua_isvector(L,1)) {
		v1 = lua_tovector(L,1);
		if (lua_isvector(L,2)) {
			v2 = lua_tovector(L,2);
			for (i = 0; i < 3; i++) res.data[i] = v1.data[i]*v2.data[i];
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			f2 = lua_tonumber(L,2);
			for (i = 0; i < 3; i++) res.data[i] = v1.data[i]*f2;
		}
	} else {
		ASSERT_SYNTAX (lua_isnumber(L,1), "Argument 1: expected vector or number");
		f1 = lua_tonumber(L,1);
		if (lua_isvector(L,2)) {
			v2 = lua_tovector(L,2);
			for (i = 0; i < 3; i++) res.data[i] = f1*v2.data[i];
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			f2 = lua_tonumber(L,2);
			lua_pushnumber (L,f1*f2);
			return 1;
		}
	}
	lua_pushvector(L,res);
	return 1;
}

/***
Elementwise vector division.

Each argument can be either a vector or a number.
The return value is a vector, unless both a and b are numbers

Dividing a number with a vector results in dividing the number with each component of the vector
@function div
@tparam (vector|number) a
@tparam (vector|number) b
@treturn (vector|number) result of a/b
@usage v = vec.div(a,b)
*/
int Interpreter::vec_div (lua_State *L)
{
	VECTOR3 v1, v2, res;
	double f1, f2;
	int i;
	if (lua_isvector(L,1)) {
		v1 = lua_tovector(L,1);
		if (lua_isvector(L,2)) {
			v2 = lua_tovector(L,2);
			for (i = 0; i < 3; i++) res.data[i] = v1.data[i]/v2.data[i];
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			f2 = lua_tonumber(L,2);
			for (i = 0; i < 3; i++) res.data[i] = v1.data[i]/f2;
		}
	} else {
		ASSERT_SYNTAX (lua_isnumber(L,1), "Argument 1: expected vector or number");
		f1 = lua_tonumber(L,1);
		if (lua_isvector(L,2)) {
			v2 = lua_tovector(L,2);
			for (i = 0; i < 3; i++) res.data[i] = f1/v2.data[i];
		} else {
			ASSERT_SYNTAX (lua_isnumber(L,2), "Argument 2: expected vector or number");
			f2 = lua_tonumber(L,2);
			lua_pushnumber(L,f1/f2);
			return 1;
		}
	}
	lua_pushvector(L,res);
	return 1;
}

/***
Scalar (inner, dot) product of two vectors.
@function dotp
@tparam vector a
@tparam vector b
@treturn number scalar product ab
@usage v = vec.dotp(a,b)
*/
int Interpreter::vec_dotp (lua_State *L)
{
	VECTOR3 v1, v2;
	ASSERT_SYNTAX(lua_isvector(L,1), "Argument 1: expected vector");
	v1 = lua_tovector(L,1);
	ASSERT_SYNTAX(lua_isvector(L,2), "Argument 2: expected vector");
	v2 = lua_tovector(L,2);
	lua_pushnumber (L, dotp(v1,v2));
	return 1;
}

/***
Cross product of two vectors.
@function crossp
@tparam vector a
@tparam vector b
@treturn vector crossp product a * b
@usage v = vec.crossp(a,b)
*/
int Interpreter::vec_crossp (lua_State *L)
{
	VECTOR3 v1, v2;
	ASSERT_SYNTAX(lua_isvector(L,1), "Argument 1: expected vector");
	v1 = lua_tovector(L,1);
	ASSERT_SYNTAX(lua_isvector(L,2), "Argument 2: expected vector");
	v2 = lua_tovector(L,2);
	lua_pushvector (L, crossp(v1,v2));
	return 1;
}

/***
Length of a vector.
@function length
@tparam vector a
@treturn number length of a
@usage len = vec.length(a)
*/
int Interpreter::vec_length (lua_State *L)
{
	VECTOR3 v;
	ASSERT_SYNTAX(lua_isvector(L,1), "Argument 1: expected vector");
	v = lua_tovector(L,1);
	lua_pushnumber (L, length(v));
	return 1;
}

/***
Distance between two points.
@function dist
@tparam vector a (point position)
@tparam vector b (point position)
@treturn number distance between point a and point b
@usage dist = vec.dist(a, b)
*/
int Interpreter::vec_dist (lua_State *L)
{
	VECTOR3 v1, v2;
	ASSERT_SYNTAX(lua_isvector(L,1), "Argument 1: expected vector");
	v1 = lua_tovector(L,1);
	ASSERT_SYNTAX(lua_isvector(L,2), "Argument 2: expected vector");
	v2 = lua_tovector(L,2);
	lua_pushnumber (L, dist(v1,v2));
	return 1;
}

/***
Unit vector.
@function unit
@tparam vector a
@treturn vector unit vector constructed from a
@usage u = vec.unit(a)
*/
int Interpreter::vec_unit (lua_State *L)
{
	VECTOR3 v;
	ASSERT_SYNTAX(lua_isvector(L,1), "Argument 1: expected vector");
	v = lua_tovector(L,1);
	lua_pushvector (L, unit(v));
	return 1;
}

/***
Matrix library functions.
@module mat
*/

/***
Identity matrix.
@function identity
@treturn matrix identity matrix
@usage I = mat.identity()
*/
int Interpreter::mat_identity (lua_State *L)
{
	lua_pushmatrix (L,identity());
	return 1;
}

/***
Matrix vector multiplication.
@function mul
@tparam matrix m
@tparam vector v
@treturn vector result of M * v
@usage newpos = mat.mul(rot, pos)
*/
int Interpreter::mat_mul (lua_State *L)
{
	ASSERT_SYNTAX(lua_ismatrix(L,1), "Argument 1: expected matrix");
	ASSERT_SYNTAX(lua_isvector(L,2), "Argument 2: expected vector");
	lua_pushvector (L, mul (lua_tomatrix(L,1), lua_tovector(L,2)));
	return 1;
}

/***
Matrix-transpose vector multiplication.
@function tmul
@tparam matrix m
@tparam vector v
@treturn vector result of M<sup>T</sup> * v
@usage p = mat.tmul(op, v)
*/
int Interpreter::mat_tmul (lua_State *L)
{
	ASSERT_SYNTAX(lua_ismatrix(L,1), "Argument 1: expected matrix");
	ASSERT_SYNTAX(lua_isvector(L,2), "Argument 2: expected vector");
	lua_pushvector (L, tmul (lua_tomatrix(L,1), lua_tovector(L,2)));
	return 1;
}

/***
Matrix matrix multiplication.
@function mmul
@tparam matrix A
@tparam matrix B
@treturn matrix result of A * B
@usage R = mat.mmul(A, B)
*/
int Interpreter::mat_mmul (lua_State *L)
{
	ASSERT_SYNTAX(lua_ismatrix(L,1), "Argument 1: expected matrix");
	ASSERT_SYNTAX(lua_ismatrix(L,2), "Argument 2: expected matrix");
	lua_pushmatrix (L, mul(lua_tomatrix(L,1), lua_tomatrix(L,2)));
	return 1;
}
/***
Construct a rotation matrix from an axis and an angle.
@function rotm
@tparam vector axis (must be normalized)
@tparam number angle [rad]
@treturn matrix rotation matrix
@usage R = mat.rotm(dir, angle)
*/

int Interpreter::mat_rotm (lua_State *L) {
	ASSERT_SYNTAX(lua_isvector(L,1), "Argument 1: expected vector");
	ASSERT_SYNTAX(lua_isnumber(L,2), "Argument 2: expected number");
	VECTOR3 axis = lua_tovector(L, 1);
	double angle = lua_tonumber(L, 2);
	double c = cos(angle), s = sin(angle);
	double t = 1-c, x = axis.x, y = axis.y, z = axis.z;

	MATRIX3 rot = _M(t*x*x+c, t*x*y-z*s, t*x*z+y*s,
		      t*x*y+z*s, t*y*y+c, t*y*z-x*s,
			  t*x*z-y*s, t*y*z+x*s, t*z*z+c);
	lua_pushmatrix(L, rot);
	return 1;
}

VECTOR3 Interpreter::lua_tovector_safe (lua_State *L, int idx, int prmno, const char *funcname)
{
	AssertPrmType(L, idx, prmno, PRMTP_VECTOR, funcname);
	return lua_tovector(L, idx);
}

VECTOR3 Interpreter::lua_tovector_safe (lua_State *L, int idx, const char *funcname)
{
	return lua_tovector_safe (L, idx, idx, funcname);
}

VECTOR3 Interpreter::luamtd_tovector_safe (lua_State *L, int idx, const char *funcname)
{
	return lua_tovector_safe (L, idx, idx-1, funcname);
}

MATRIX3 Interpreter::lua_tomatrix_safe (lua_State *L, int idx, int prmno, const char *funcname)
{
	AssertPrmType(L, idx, prmno, PRMTP_MATRIX, funcname);
	return lua_tomatrix(L, idx);
}
MATRIX3 Interpreter::lua_tomatrix_safe (lua_State *L, int idx, const char *funcname)
{
	return lua_tomatrix_safe (L, idx, idx - 1, funcname);
}

MATRIX3 Interpreter::luamtd_tomatrix_safe (lua_State *L, int idx, const char *funcname)
{
	return lua_tomatrix_safe (L, idx, idx - 1, funcname);
}

void Interpreter::lua_pushvector (lua_State *L, const VECTOR3 &vec)
{
	lua_createtable (L, 0, 3);
	lua_pushnumber (L, vec.x);
	lua_setfield (L, -2, "x");
	lua_pushnumber (L, vec.y);
	lua_setfield (L, -2, "y");
	lua_pushnumber (L, vec.z);
	lua_setfield (L, -2, "z");
}

int Interpreter::lua_isvector (lua_State *L, int idx)
{
	if (!lua_istable (L, idx)) return 0;
	static char fieldname[3] = {'x','y','z'};
	static char field[2] = "x";
	int i, ii, n;
	bool fail;

	lua_pushnil(L);
	ii = (idx >= 0 ? idx : idx-1);
	n = 0;
	while(lua_next(L,ii)) {
		lua_pop(L,1);
		n++;
	}
	if (n != 3) return 0;

	for (i = 0; i < 3; i++) {
		field[0] = fieldname[i];
		lua_getfield (L, idx, field);
		fail = (lua_isnil (L,-1));
		lua_pop (L,1);
		if (fail) return 0;
	}
	return 1;
}

void Interpreter::lua_pushmatrix (lua_State *L, const MATRIX3 &mat)
{
	lua_createtable(L,0,9);
	lua_pushnumber(L,mat.m11);  lua_setfield(L,-2,"m11");
	lua_pushnumber(L,mat.m12);  lua_setfield(L,-2,"m12");
	lua_pushnumber(L,mat.m13);  lua_setfield(L,-2,"m13");
	lua_pushnumber(L,mat.m21);  lua_setfield(L,-2,"m21");
	lua_pushnumber(L,mat.m22);  lua_setfield(L,-2,"m22");
	lua_pushnumber(L,mat.m23);  lua_setfield(L,-2,"m23");
	lua_pushnumber(L,mat.m31);  lua_setfield(L,-2,"m31");
	lua_pushnumber(L,mat.m32);  lua_setfield(L,-2,"m32");
	lua_pushnumber(L,mat.m33);  lua_setfield(L,-2,"m33");
}

MATRIX3 Interpreter::lua_tomatrix (lua_State *L, int idx)
{
	MATRIX3 mat;
	lua_getfield (L, idx, "m11");  mat.m11 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m12");  mat.m12 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m13");  mat.m13 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m21");  mat.m21 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m22");  mat.m22 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m23");  mat.m23 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m31");  mat.m31 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m32");  mat.m32 = lua_tonumber (L, -1);  lua_pop (L,1);
	lua_getfield (L, idx, "m33");  mat.m33 = lua_tonumber (L, -1);  lua_pop (L,1);
	return mat;
}

int Interpreter::lua_ismatrix (lua_State *L, int idx)
{
	if (!lua_istable (L, idx)) return 0;
	static const char *fieldname[9] = {"m11","m12","m13","m21","m22","m23","m31","m32","m33"};
	int i, ii, n;
	bool fail;

	lua_pushnil(L);
	ii = (idx >= 0 ? idx : idx-1);
	n = 0;
	while(lua_next(L,ii)) {
		lua_pop(L,1);
		n++;
	}
	if (n != 9) return 0;

	for (i = 0; i < 9; i++) {
		lua_getfield (L, idx, fieldname[i]);
		fail = (lua_isnil (L,-1));
		lua_pop (L,1);
		if (fail) return 0;
	}
	return 1;
}
