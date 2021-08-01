/***************
 * First comment is ignored,
 * containing licenses, warnings,
 * old-fashioned commit info and so forth
 */

/** No-brainer C extension.
Description as before.
@module x
*/
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

/***
@string name
@int age
@table output
*/

/***
Create a table with given array and hash slots.
Note that we can't (yet) deduce the name of
the Lua function.
@function createtable
@param narr initial array slots, default 0
@param nrec initial hash slots, default 0
@return table
*/
static int l_createtable (lua_State *L) {
  int narr = luaL_optint(L,1,0);
  int nrec = luaL_optint(L,2,0);
  lua_createtable(L,narr,nrec);
  return 1;
}

static const luaL_reg x[] = {
    {"createtable",l_createtable},
    {NULL,NULL}
};

int luaopen_x(lua_State *L)
{
    luaL_register (L, "x", x);
    return 1;
}
