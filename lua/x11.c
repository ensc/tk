/*	--*- c -*--
 * Copyright (C) 2015 Enrico Scholz <enrico.scholz@sigma-chemnitz.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/scrnsaver.h>

#include <lua.h>
#include <lauxlib.h>

struct x11_idle {
	XScreenSaverInfo	*info;
	Display			*display;
	Drawable		root;
};

static int l_idle_get(lua_State *L)
{
	struct x11_idle		*idle = lua_touserdata(L, lua_upvalueindex(1));
	unsigned int		tm;

	XScreenSaverQueryInfo(idle->display, idle->root, idle->info);

	tm = idle->info->idle;
	lua_pushinteger(L, tm);

	return 1;
}

static int l_idle_init(lua_State *L)
{
	struct x11_idle		*idle;

	idle = lua_newuserdata(L, sizeof *idle);

	luaL_getmetatable(L, "ensc.x11_idle");
	lua_setmetatable(L, -2);

	idle->info    = XScreenSaverAllocInfo();
	idle->display = XOpenDisplay(NULL);
	idle->root    = RootWindow(idle->display, DefaultScreen(idle->display));

	/* TODO: check errors! */
	
	lua_pushcclosure(L, l_idle_get, 1);
	return 1;
}

static int l_idle_gc(lua_State *L)
{
	struct x11_idle		*idle = lua_touserdata(L, 1);

	if (idle) {
		XFree(idle->info);
		XCloseDisplay(idle->display);
	}

	return 0;
}

int luaopen_x11(lua_State *L)
{
      luaL_newmetatable(L, "ensc.x11_idle");

      lua_pushstring(L, "__gc");
      lua_pushcfunction(L, l_idle_gc);
      lua_settable(L, -3);

      /* register the `dir' function */
      lua_pushcfunction(L, l_idle_init);
      lua_setglobal(L, "x11_idle");

      return 0;
}
