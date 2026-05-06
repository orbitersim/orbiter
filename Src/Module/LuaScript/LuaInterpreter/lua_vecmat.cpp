#define INTERPRETER_IMPLEMENTATION

#include "Interpreter.h"


// ============================================================================
// 

/***
Vector library functions.
@module vec
*/
static int vec_index(lua_State *L)
{
    VECTOR3* v = (VECTOR3*)luaL_checkudata(L, 1, VEC3_META);
    const char* key = luaL_checkstring(L, 2);

    switch (key[0])
    {
        case 'x': lua_pushnumber(L, v->x); return 1;
        case 'y': lua_pushnumber(L, v->y); return 1;
        case 'z': lua_pushnumber(L, v->z); return 1;
    }

	// fallback to metatable for methods
    luaL_getmetatable(L, VEC3_META);
    lua_pushvalue(L, 2);
    lua_rawget(L, -2);
	lua_remove(L, -2);
	return 1;
}

static int vec_newindex(lua_State *L)
{
    VECTOR3* v = (VECTOR3*)luaL_checkudata(L, 1, VEC3_META);
    const char* key = luaL_checkstring(L, 2);
    double val = luaL_checknumber(L, 3);

    switch (key[0])
    {
        case 'x': v->x = val; break;
        case 'y': v->y = val; break;
        case 'z': v->z = val; break;
		default: luaL_error(L, "invalid vec3 field '%s'", key);
    }

    return 0;
}

static int mat_index(lua_State *L)
{
    MATRIX3* m = (MATRIX3*)luaL_checkudata(L, 1, MAT3_META);
    const char* key = luaL_checkstring(L, 2);

    if (key[0] == 'm' && key[1] && key[2] && key[3] == '\0')
    {
        int r = key[1] - '1';
        int c = key[2] - '1';

        if (r >= 0 && r < 3 && c >= 0 && c < 3)
        {
            const double* base = &m->m11;
            lua_pushnumber(L, base[r*3 + c]);
            return 1;
        }
    }

	// fallback to metatable for methods
    luaL_getmetatable(L, MAT3_META);
    lua_pushvalue(L, 2);
    lua_rawget(L, -2);
	lua_remove(L, -2);
	return 1;
}

static int mat_newindex(lua_State *L)
{
    MATRIX3* m = (MATRIX3*)luaL_checkudata(L, 1, MAT3_META);
    const char* key = luaL_checkstring(L, 2);
    double val = luaL_checknumber(L, 3);

    // Fast dispatch using second char
    if (key[0] == 'm' && key[1] && key[2] && key[3] == '\0')
    {
        int r = key[1] - '1';
        int c = key[2] - '1';

        if (r >= 0 && r < 3 && c >= 0 && c < 3)
        {
			double* base = &m->m11;
			base[r * 3 + c] = val;
			return 0;
        }
    }

    luaL_error(L, "invalid mat3 field '%s'", key);
	return 0;
}

int Interpreter::mat_mul_dispatch(lua_State *L)
{
    if (lua_ismatrix(L, 1)) {
		if(lua_isvector(L, 2))
		{
			return mat_mul(L);
		} else if (lua_ismatrix(L, 2)) {
			return mat_mmul(L);
		}
	}
    luaL_error(L, "invalid operands for mat3 multiplication");
    return 0;
}

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
		{"set", mat_set},
		{"mul", mat_mul},
		{"tmul", mat_tmul},
		{"mmul", mat_mmul},
		{"rotm", mat_rotm},
		{NULL, NULL}
	};
	luaL_openlib (L, "mat", matLib, 0);

	// metatable
    // ===== vec3 =====

	static const struct luaL_reg vec3Lib[] = {
		{"__add", vec_add},
		{"__sub", vec_sub},
		{"__unm", vec_unm},
		{"__mul", vec_mul},
		{"__div", vec_div},
		{"__index", vec_index},
		{"__newindex", vec_newindex},
		{"dotp", vec_dotp},
		{"crossp", vec_crossp},
		{"length", vec_length},
		{"dist", vec_dist},
		{"unit", vec_unit},
		{NULL, NULL}
	};

	luaL_newmetatable (L, VEC3_META);
	luaL_openlib (L, NULL, vec3Lib, 0);

    // ===== mat3 =====
	static const struct luaL_reg mat3Lib[] = {
		{"__mul", mat_mul_dispatch},
		{"__index", mat_index},
		{"__newindex", mat_newindex},
		{NULL, NULL}
	};

	luaL_newmetatable (L, MAT3_META);
	luaL_openlib (L, NULL, mat3Lib, 0);
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
int Interpreter::vec_set(lua_State *L)
{
    double x = luaL_checknumber(L, 1);
    double y = luaL_checknumber(L, 2);
    double z = luaL_checknumber(L, 3);

    VECTOR3* v = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));
    v->x = x;
    v->y = y;
    v->z = z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

    return 1;
}

// to_vec differs from lua_tovector in that it converts a number to a vector
static VECTOR3* to_vec(lua_State *L, int idx, VECTOR3 *tmp)
{
    VECTOR3* v = (VECTOR3*)Interpreter::luaL_tryudata(L, idx, VEC3_META);
    if (v)
        return v;

	if (lua_isnumber(L, idx))
    {
        double s = lua_tonumber(L, idx);

        tmp->x = s;
        tmp->y = s;
        tmp->z = s;
        return tmp;
    }

    if (!lua_istable(L, idx))
        luaL_error(L, "bad argument #%d, expected vec3 or {x,y,z} table", idx);

    lua_getfield(L, idx, "x");
    tmp->x = luaL_checknumber(L, -1);
    lua_pop(L, 1);
    lua_getfield(L, idx, "y");
    tmp->y = luaL_checknumber(L, -1);
    lua_pop(L, 1);
    lua_getfield(L, idx, "z");
    tmp->z = luaL_checknumber(L, -1);
    lua_pop(L, 1);

    return tmp;
}

VECTOR3 *lua_tovector (lua_State *L, int idx, VECTOR3 *tmp)
{
    VECTOR3* v = (VECTOR3*)Interpreter::luaL_tryudata(L, idx, VEC3_META);
    if (v)
        return v;

    if (!lua_istable(L, idx))
        luaL_error(L, "bad argument #%d, expected vec3 or {x,y,z} table", idx);

    lua_getfield(L, idx, "x");
    tmp->x = luaL_checknumber(L, -1);
    lua_pop(L, 1);
    lua_getfield(L, idx, "y");
    tmp->y = luaL_checknumber(L, -1);
    lua_pop(L, 1);
    lua_getfield(L, idx, "z");
    tmp->z = luaL_checknumber(L, -1);
    lua_pop(L, 1);

    return tmp;
}

/***
Sum of two vectors.

Each argument can be either a vector or a number.
number arguments are expanded to a vector: a -> (a,a,a)
The return value is a vector

When operating on a number and a vector, the number is replaced with the vector {x=number, y=number, z=number}
@function add
@tparam (vector|number) a
@tparam (vector|number) b
@treturn vector result of a+b
@usage v = vec.add(a,b)
*/
int Interpreter::vec_add(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = a->x + b->x;
    r->y = a->y + b->y;
    r->z = a->z + b->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

    return 1;
}

/***
Difference of two vectors.

Each argument can be either a vector or a number.
number arguments are expanded to a vector: a -> (a,a,a)
The return value is a vector

Substracting a number to a vector results in substracting the number to each component of the vector
@function sub
@tparam (vector|number) a
@tparam (vector|number) b
@treturn vector result of a-b
@usage v = vec.sub(a,b)
*/
int Interpreter::vec_sub(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = a->x - b->x;
    r->y = a->y - b->y;
    r->z = a->z - b->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

    return 1;
}

int Interpreter::vec_unm(lua_State *L)
{
    VECTOR3 tmp;
    VECTOR3* v = to_vec(L, 1, &tmp);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = -v->x;
    r->y = -v->y;
    r->z = -v->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

    return 1;
}

/***
Elementwise vector multiplication.

Each argument can be either a vector or a number.
number arguments are expanded to a vector: a -> (a,a,a)
The return value is a vector

Multiplying a number with a vector results in multiplying the number with each component of the vector
@function mul
@tparam (vector|number) a
@tparam (vector|number) b
@treturn vector result of a*b
@usage v = vec.mul(a,b)
*/
int Interpreter::vec_mul(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = a->x * b->x;
    r->y = a->y * b->y;
    r->z = a->z * b->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

    return 1;
}

/***
Elementwise vector division.

Each argument can be either a vector or a number.
number arguments are expanded to a vector: a -> (a,a,a)
The return value is a vector

Dividing a number with a vector results in dividing the number with each component of the vector
@function div
@tparam (vector|number) a
@tparam (vector|number) b
@treturn vector result of a/b
@usage v = vec.div(a,b)
*/
int Interpreter::vec_div(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = a->x / b->x;
    r->y = a->y / b->y;
    r->z = a->z / b->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

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
int Interpreter::vec_dotp(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    lua_pushnumber(L,
        a->x*b->x +
        a->y*b->y +
        a->z*b->z
    );

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
int Interpreter::vec_crossp(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = a->y*b->z - a->z*b->y;
    r->y = a->z*b->x - a->x*b->z;
    r->z = a->x*b->y - a->y*b->x;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

    return 1;
}

/***
Length of a vector.
@function length
@tparam vector a
@treturn number length of a
@usage len = vec.length(a)
*/
int Interpreter::vec_length(lua_State *L)
{
	VECTOR3 tmp;
    VECTOR3* v = to_vec(L, 1, &tmp);

    lua_pushnumber(L,
        sqrt(v->x*v->x + v->y*v->y + v->z*v->z)
    );

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
int Interpreter::vec_dist(lua_State *L)
{
    VECTOR3 ta, tb;

    VECTOR3* a = to_vec(L, 1, &ta);
    VECTOR3* b = to_vec(L, 2, &tb);

    double dx = a->x - b->x;
    double dy = a->y - b->y;
    double dz = a->z - b->z;

    lua_pushnumber(L, sqrt(dx*dx + dy*dy + dz*dz));
    return 1;
}
/***
Unit vector.
@function unit
@tparam vector a
@treturn vector unit vector constructed from a
@usage u = vec.unit(a)
*/
int Interpreter::vec_unit(lua_State *L)
{
	VECTOR3 tmp;
    VECTOR3* v = to_vec(L, 1, &tmp);

    double len = sqrt(v->x*v->x + v->y*v->y + v->z*v->z);

    if (len == 0)
        luaL_error(L, "cannot normalize zero vector");

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = v->x / len;
    r->y = v->y / len;
    r->z = v->z / len;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

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
int Interpreter::mat_identity(lua_State *L)
{
    MATRIX3* m = (MATRIX3*)lua_newuserdata(L, sizeof(MATRIX3));

    *m = identity();

    luaL_getmetatable(L, MAT3_META);
    lua_setmetatable(L, -2);

    return 1;
}

int Interpreter::mat_set(lua_State *L)
{
    // Expect exactly 9 numbers
    for (int i = 1; i <= 9; i++)
        luaL_checknumber(L, i);

    MATRIX3* m = (MATRIX3*)lua_newuserdata(L, sizeof(MATRIX3));

    m->m11 = lua_tonumber(L, 1);
    m->m12 = lua_tonumber(L, 2);
    m->m13 = lua_tonumber(L, 3);

    m->m21 = lua_tonumber(L, 4);
    m->m22 = lua_tonumber(L, 5);
    m->m23 = lua_tonumber(L, 6);

    m->m31 = lua_tonumber(L, 7);
    m->m32 = lua_tonumber(L, 8);
    m->m33 = lua_tonumber(L, 9);

    luaL_getmetatable(L, MAT3_META);
    lua_setmetatable(L, -2);

    return 1;
}
static MATRIX3* to_mat(lua_State *L, int idx, MATRIX3 *tmp)
{
    MATRIX3* m = (MATRIX3*)Interpreter::luaL_tryudata(L, idx, MAT3_META);
    if (m)
        return m;

    if (!lua_istable(L, idx))
        luaL_error(L, "bad argument #%d, expected mat3 userdata or table", idx);

    const char* k[9] = {
        "m11","m12","m13",
        "m21","m22","m23",
        "m31","m32","m33"
    };

    for (int i = 0; i < 9; i++)
    {
        lua_getfield(L, idx, k[i]);
        tmp->data[i] = luaL_checknumber(L, -1);
        lua_pop(L, 1);
    }
    return tmp;
}

/***
Matrix vector multiplication.
@function mul
@tparam matrix m
@tparam vector v
@treturn vector result of M * v
@usage newpos = mat.mul(rot, pos)
*/
int Interpreter::mat_mul(lua_State *L)
{
    MATRIX3 tm;
    VECTOR3 tv;

    MATRIX3* m = to_mat(L, 1, &tm);
    VECTOR3* v = to_vec(L, 2, &tv);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    r->x = m->m11*v->x + m->m12*v->y + m->m13*v->z;
    r->y = m->m21*v->x + m->m22*v->y + m->m23*v->z;
    r->z = m->m31*v->x + m->m32*v->y + m->m33*v->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

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
int Interpreter::mat_tmul(lua_State *L)
{
    MATRIX3 tm;
    VECTOR3 tv;

    MATRIX3* m = to_mat(L, 1, &tm);
    VECTOR3* v = to_vec(L, 2, &tv);

    VECTOR3* r = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));

    // Transpose multiplication
    r->x = m->m11*v->x + m->m21*v->y + m->m31*v->z;
    r->y = m->m12*v->x + m->m22*v->y + m->m32*v->z;
    r->z = m->m13*v->x + m->m23*v->y + m->m33*v->z;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);

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
int Interpreter::mat_mmul(lua_State *L)
{
    MATRIX3 ta, tb;

    MATRIX3* A = to_mat(L, 1, &ta);
    MATRIX3* B = to_mat(L, 2, &tb);

    MATRIX3* R = (MATRIX3*)lua_newuserdata(L, sizeof(MATRIX3));

    R->m11 = A->m11*B->m11 + A->m12*B->m21 + A->m13*B->m31;
    R->m12 = A->m11*B->m12 + A->m12*B->m22 + A->m13*B->m32;
    R->m13 = A->m11*B->m13 + A->m12*B->m23 + A->m13*B->m33;

    R->m21 = A->m21*B->m11 + A->m22*B->m21 + A->m23*B->m31;
    R->m22 = A->m21*B->m12 + A->m22*B->m22 + A->m23*B->m32;
    R->m23 = A->m21*B->m13 + A->m22*B->m23 + A->m23*B->m33;

    R->m31 = A->m31*B->m11 + A->m32*B->m21 + A->m33*B->m31;
    R->m32 = A->m31*B->m12 + A->m32*B->m22 + A->m33*B->m32;
    R->m33 = A->m31*B->m13 + A->m32*B->m23 + A->m33*B->m33;

    luaL_getmetatable(L, MAT3_META);
    lua_setmetatable(L, -2);

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
int Interpreter::mat_rotm(lua_State *L)
{
    VECTOR3 t_axis;
    VECTOR3* axis = to_vec(L, 1, &t_axis);

    double angle = luaL_checknumber(L, 2);

    double x = axis->x;
    double y = axis->y;
    double z = axis->z;

    double c = cos(angle);
    double s = sin(angle);
    double t = 1.0 - c;

    MATRIX3* R = (MATRIX3*)lua_newuserdata(L, sizeof(MATRIX3));

    R->m11 = t*x*x + c;
    R->m12 = t*x*y - z*s;
    R->m13 = t*x*z + y*s;

    R->m21 = t*x*y + z*s;
    R->m22 = t*y*y + c;
    R->m23 = t*y*z - x*s;

    R->m31 = t*x*z - y*s;
    R->m32 = t*y*z + x*s;
    R->m33 = t*z*z + c;

    luaL_getmetatable(L, MAT3_META);
    lua_setmetatable(L, -2);

    return 1;
}

VECTOR3 *Interpreter::lua_tovector_safe (lua_State *L, int idx, int prmno, const char *funcname, VECTOR3 *tmp)
{
	AssertPrmType(L, idx, prmno, PRMTP_VECTOR, funcname);
	return lua_tovector(L, idx, tmp);
}

VECTOR3 *Interpreter::lua_tovector_safe (lua_State *L, int idx, const char *funcname, VECTOR3 *tmp)
{
	return lua_tovector_safe (L, idx, idx, funcname, tmp);
}

VECTOR3 *Interpreter::luamtd_tovector_safe (lua_State *L, int idx, const char *funcname, VECTOR3 *tmp)
{
	return lua_tovector_safe (L, idx, idx-1, funcname, tmp);
}

MATRIX3 *Interpreter::lua_tomatrix_safe (lua_State *L, int idx, int prmno, const char *funcname, MATRIX3 *tmp)
{
	AssertPrmType(L, idx, prmno, PRMTP_MATRIX, funcname);
	return lua_tomatrix(L, idx, tmp);
}
MATRIX3 *Interpreter::lua_tomatrix_safe (lua_State *L, int idx, const char *funcname, MATRIX3 *tmp)
{
	return lua_tomatrix_safe (L, idx, idx - 1, funcname, tmp);
}

MATRIX3 *Interpreter::luamtd_tomatrix_safe (lua_State *L, int idx, const char *funcname, MATRIX3 *tmp)
{
	return lua_tomatrix_safe (L, idx, idx - 1, funcname, tmp);
}

void Interpreter::lua_pushvector (lua_State *L, const VECTOR3 &vec)
{
    VECTOR3* u = (VECTOR3*)lua_newuserdata(L, sizeof(VECTOR3));
    *u = vec;

    luaL_getmetatable(L, VEC3_META);
    lua_setmetatable(L, -2);
}

int Interpreter::lua_isvector (lua_State *L, int idx)
{
    VECTOR3* v = (VECTOR3*)Interpreter::luaL_tryudata(L, idx, VEC3_META);
    if (v)
        return true;

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
    MATRIX3* u = (MATRIX3*)lua_newuserdata(L, sizeof(MATRIX3));
    *u = mat;

    luaL_getmetatable(L, MAT3_META);
    lua_setmetatable(L, -2);
}

MATRIX3 *Interpreter::lua_tomatrix (lua_State *L, int idx, MATRIX3 *tmp)
{
    MATRIX3* m = (MATRIX3*)Interpreter::luaL_tryudata(L, idx, MAT3_META);
    if (m)
        return m;

    if (!lua_istable(L, idx))
        luaL_error(L, "bad argument #%d, expected mat3 userdata or table", idx);

    const char* k[9] = {
        "m11","m12","m13",
        "m21","m22","m23",
        "m31","m32","m33"
    };

    for (int i = 0; i < 9; i++)
    {
        lua_getfield(L, idx, k[i]);
        tmp->data[i] = luaL_checknumber(L, -1);
        lua_pop(L, 1);
    }
    return tmp;
}

int Interpreter::lua_ismatrix (lua_State *L, int idx)
{
    MATRIX3* m = (MATRIX3*)Interpreter::luaL_tryudata(L, idx, MAT3_META);
    if (m)
        return true;

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
