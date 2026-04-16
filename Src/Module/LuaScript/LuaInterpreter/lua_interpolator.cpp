// Copyright (c) 2026 Gondos
// Licensed under the MIT License
// Helpers for the Lua interpreter to provide interpolation facilities

#define INTERPRETER_IMPLEMENTATION

#include "Interpreter.h"

/***
Interpolator interface functions

Note: creating interpolators incurs a cost. It's best to create them once during startup and use them later.
@module interpolator

*/
/// @lookup types

// Uniform linear interpolation
// We can interpolate several tables in one go
typedef struct {
    int tsize;  // table size
    int ntable; // number of tables
    double min, max;
    double inv_step;
    double *data; // Will contain data for m tables
} uniform_interpolator;

// non-uniform linear interpolation
// We can interpolate several tables in one go
typedef struct {
    int tsize; // table size
    int ntable; // number of tables
    int last; // cache for last index
    double *x;
    double *y; // Will contain data for multiple tables
    double *slope; // Precomputed slopes for the x table
} nonuniform_interpolator;

/***
Create an instance of a uniform interpolator.

The returned object can be called to compute a piece-wise linear interpolation
given a uniformly distributed set of points over a range.

If several tables are provided, calling the interpolator will return several values,
one for each table, in the same order.

The output is clamped if the input value is outside of the range.

@function uniform
@tparam number min lower value of the range
@tparam number max upper value of the range
@param ... tables with sample points used for interpolation
@treturn interpolator
@usage
local VCL = {0.1, 0.17, 0.2, 0.2, 0.17, 0.1, 0, -0.11, -0.24, -0.38,  -0.5,  -0.5, -0.02, 0.6355,    0.63,   0.46, 0.28, 0.13, 0.0, -0.16, -0.26, -0.29, -0.24, -0.1, 0.1}
local VCM = {  0,    0,   0,   0,    0,   0, 0,     0,    0,0.002,0.004, 0.0025,0.0012,      0,-0.0012,-0.0007,    0,    0,   0,     0,     0,     0,     0,    0,   0}
local VInterp = interpolator.uniform(-math.pi, math.pi, VCL, VCM)
local cl, cm = VInterp(aoa)
*/
static int create_uniform_interpolator(lua_State *L) {
    int top = lua_gettop(L);

    if (top < 3)
        return luaL_error(L, "Need min, max, and at least one table");

    double min = luaL_checknumber(L, 1);
    double max = luaL_checknumber(L, 2);

    if (max == min)
        return luaL_error(L, "max must differ from min");

    int ntable = top - 2;

    // Check first table
    luaL_checktype(L, 3, LUA_TTABLE);
    int tsize = lua_objlen(L, 3);

    if (tsize < 2)
        return luaL_error(L, "Tables must have at least 2 elements");

    // Check other tables
    for (int t = 0; t < ntable; t++) {
        int idx = 3 + t;
        luaL_checktype(L, idx, LUA_TTABLE);

        if (lua_objlen(L, idx) != tsize)
            return luaL_error(L, "All tables must have the same size");
    }

	// Try to have a good alignment for doubles
    size_t sz = sizeof(uniform_interpolator);
    sz = (sz + alignof(double) - 1) & ~(alignof(double) - 1);
    uniform_interpolator *interp = (uniform_interpolator *)lua_newuserdata(L, sz + ntable * tsize * sizeof(double));

    interp->data = (double *)((char *)interp + sz);
    interp->tsize = tsize;
    interp->ntable = ntable;
    interp->min = min;
    interp->max = max;
    interp->inv_step = (tsize - 1) / (max - min);

    // Extract tables data
    for (int t = 0; t < ntable; t++) {
        int tbl = 3 + t;

        for (int i = 0; i < tsize; i++) {
            lua_rawgeti(L, tbl, i + 1);
            interp->data[t * tsize + i] = luaL_checknumber(L, -1);
            lua_pop(L, 1);
        }
    }

    luaL_getmetatable(L, "UNIINTERP.vtable");
    lua_setmetatable(L, -2);

    return 1;
}

static int call_uniform_interpolator(lua_State *L) {
    uniform_interpolator *interp = (uniform_interpolator *)luaL_checkudata(L, 1, "UNIINTERP.vtable");
    double x = luaL_checknumber(L, 2);

    double t = (x - interp->min) * interp->inv_step;

    // Clamp
    if (t < 0.0) t = 0.0;
    if (t > (double)(interp->tsize - 1)) t = (double)(interp->tsize - 1);

    int idx = (int)t;

    if (idx >= interp->tsize - 1) {
        for (int k = 0; k < interp->ntable; k++) {
            lua_pushnumber(L, interp->data[k * interp->tsize + (interp->tsize - 1)]);
        }
        return interp->ntable;
    }

    double frac = t - idx;

    for (int k = 0; k < interp->ntable; k++) {
        double *d = &interp->data[k * interp->tsize];

        double y0 = d[idx];
        double y1 = d[idx + 1];

        lua_pushnumber(L, y0 + (y1 - y0) * frac);
    }

    return interp->ntable;
}

/***
Create an instance of a non uniform interpolator.

The returned object can be called to compute a piece-wise linear interpolation
given a non uniformly distributed set of points.

You must first provide a table with the 'x' values associated with 'y' values provided in other tables

The next tables to provide correspond to the 'y' values.

If several more tables are provided, calling the interpolator will return several values,
one for each table in the same order.

The output is clamped if the input value is outside of the range.

@function nonuniform
@tparam table xs values at which sample points are provided
@param ... tables with sample points used for interpolation, each point associated with a value in xs
@treturn interpolator
@usage
local AOA = {-180*RAD,-60*RAD,-30*RAD, -2*RAD, 15*RAD,20*RAD,25*RAD,60*RAD,180*RAD}
local CL  = {       0,      0,   -0.4,      0,    0.7,     1,   0.8,     0,      0}
local CM  = {       0,      0,  0.014, 0.0039, -0.006,-0.008,-0.010,     0,      0}
local VInterp = interpolator.nonuniform(AOA, CL, CM)
local cl, cm = VInterp(aoa)
*/

static int create_nonuniform_interpolator(lua_State *L) {
    int top = lua_gettop(L);

    if (top < 2)
        return luaL_error(L, "Need xs and at least one ys table");

    int ntable = top - 1;

    luaL_checktype(L, 1, LUA_TTABLE); // xs
    int tsize = lua_objlen(L, 1);

    if (tsize < 2)
        return luaL_error(L, "Need at least 2 points");

    // Validate all ys tables
    for (int t = 0; t < ntable; t++) {
        int idx = 2 + t;
        luaL_checktype(L, idx, LUA_TTABLE);

        if (lua_objlen(L, idx) != tsize)
            return luaL_error(L, "All tables must have same size as xs");
    }

    size_t sz = sizeof(nonuniform_interpolator);
    sz = (sz + alignof(double) - 1) & ~(alignof(double) - 1);

    nonuniform_interpolator *interp =
        (nonuniform_interpolator *)lua_newuserdata(
            L, sz + (1 + ntable) * tsize * sizeof(double) + ntable * (tsize - 1) * sizeof(double)
        );

    char *ptr = (char *)interp + sz;

    interp->x = (double *)ptr;
    ptr += tsize * sizeof(double);

    interp->y = (double *)ptr;
    ptr += ntable * tsize * sizeof(double);

    interp->slope = (double *)ptr;

    interp->tsize = tsize;
    interp->ntable = ntable;
    interp->last = 0;

    // Load xs
    for (int i = 0; i < tsize; i++) {
        lua_rawgeti(L, 1, i + 1);
        interp->x[i] = luaL_checknumber(L, -1);
        lua_pop(L, 1);

        if (i > 0 && interp->x[i] <= interp->x[i - 1])
            return luaL_error(L, "xs must be strictly increasing");
    }

    // Load ys
    for (int t = 0; t < ntable; t++) {
        int tbl = 2 + t;

        for (int i = 0; i < tsize; i++) {
            lua_rawgeti(L, tbl, i + 1);
            interp->y[t * tsize + i] = luaL_checknumber(L, -1);
            lua_pop(L, 1);
        }
    }

    // Precompute slopes
    for (int t = 0; t < ntable; t++) {
        double *y = &interp->y[t * tsize];
        double *s = &interp->slope[t * (tsize - 1)];

        for (int i = 0; i < tsize - 1; i++) {
            double dx = interp->x[i + 1] - interp->x[i];
            s[i] = (y[i + 1] - y[i]) / dx;
        }
    }

    luaL_getmetatable(L, "NONUNIINTERP.vtable");
    lua_setmetatable(L, -2);

    return 1;
}

// We cache the last accessed interval to perform a fast
// check for current or adjacent cells.
// If not, then use a binary search to find the good interval
static int find_interval_cached(nonuniform_interpolator *interp, double x) {
    int i = interp->last;

    // current
    if (i + 1 < interp->tsize) {
        if (x >= interp->x[i] && x < interp->x[i + 1])
            return i;
    }

    // forward
    if (i + 2 < interp->tsize) {
        if (x >= interp->x[i + 1] && x < interp->x[i + 2])
            return i + 1;
    }

    // backward
    if (i > 0) {
        if (x >= interp->x[i - 1] && x < interp->x[i])
            return i - 1;
    }

    // binary search
    int lo = 0;
    int hi = interp->tsize - 2;

    while (lo <= hi) {
        int mid = (lo + hi) >> 1;

        if (x < interp->x[mid]) {
            hi = mid - 1;
        } else if (x >= interp->x[mid + 1]) {
            lo = mid + 1;
        } else {
            return mid;
        }
    }

    return 0;
}

static int nonuniform_interp_call(lua_State *L) {
    nonuniform_interpolator *interp = (nonuniform_interpolator *)luaL_checkudata(L, 1, "NONUNIINTERP.vtable");
    double x = luaL_checknumber(L, 2);

    // Clamp low
    if (x <= interp->x[0]) {
        interp->last = 0;
        for (int t = 0; t < interp->ntable; t++) {
            lua_pushnumber(L, interp->y[t * interp->tsize]);
        }
        return interp->ntable;
    }

    // Clamp high
    if (x >= interp->x[interp->tsize - 1]) {
        interp->last = interp->tsize - 2;
        for (int t = 0; t < interp->ntable; t++) {
            lua_pushnumber(L, interp->y[t * interp->tsize + (interp->tsize - 1)]);
        }
        return interp->ntable;
    }

    int i = find_interval_cached(interp, x);
    interp->last = i;

    double dx = x - interp->x[i];

    for (int t = 0; t < interp->ntable; t++) {
        double *y = &interp->y[t * interp->tsize];
        double *s = &interp->slope[t * (interp->tsize - 1)];

        double val = y[i] + s[i] * dx;
        lua_pushnumber(L, val);
    }

    return interp->ntable;
}

void Interpreter::LoadInterpolatorAPI ()
{
	static const struct luaL_reg interpLib[] = {
		{"uniform", create_uniform_interpolator},
		{"nonuniform", create_nonuniform_interpolator},
		{NULL, NULL}
	};
	luaL_openlib (L, "interpolator", interpLib, 0);

	luaL_newmetatable(L, "UNIINTERP.vtable");
    lua_pushstring(L, "__call");
    lua_pushcfunction(L, call_uniform_interpolator);
    lua_settable(L, -3);
    lua_pop(L, 1);

    luaL_newmetatable(L, "NONUNIINTERP.vtable");
    lua_pushstring(L, "__call");
    lua_pushcfunction(L, nonuniform_interp_call);
    lua_settable(L, -3);
    lua_pop(L, 1);
}
