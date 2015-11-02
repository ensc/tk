-- Copyright (C) 2015 Enrico Scholz <enrico.scholz@sigma-chemnitz.de>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; version 3 of the License.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

local socket = require("socket")
local posix = require("posix")
local tk = {}
local d = require 'pl.pretty'.dump

local capi = {
    client = client,
    root = root,
}

local QUEUE_LIMIT = 1000
local QUEUE_REMOVAL_EXTRA = QUEUE_LIMIT/10
local MAX_QUEUE_PER_RUN = 20

function tk.init()
    self = {
	event		= event,
	connect_signals	= connect_signals,
	reset_idle	= reset_idle,
	connect_fn	= function(self)
	    if self.sock_path ~= nil then
		require("socket.unix")
		sock = socket.unix()
		sock:connect(self.sock_path)
	    else
		sock = socket.tcp()
		sock:connect(self.host, self.port)
	    end

	    if sock:getfd() < 0 then
		error("bad socket")
	    end

	    return sock
	end,

	host		= "localhost",
	port		= 9999,

	_sock_path	= nil,
	_queue		= {},
	_conn		= nil,
	_outbuf_len	= 0,
	_outbuf_pos	= 0,
	_outbuf		= nil,
	_send		= nil,
	_idle		= {
	    timer	= timer({ timeout = 3 }),
	    last_x	= -1,
	    last_y	= -1,
	    last_tm	= -1,
	    timeout	= 10,
	    is_idle	= false,
	},

	_names		= { },
    }

    rdir = os.getenv("XDG_RUNTIME_DIR")
    if rdir then
	self.sock_path = rdir  .. "/tk.sock"
    end

    self.thread = coroutine.create(function () loop(self) end)
    self._idle.timer:connect_signal("timeout", function () check_idle(self) end)
    reset_idle(self)
    return self
end

function event(self, id, c)
    tags = capi.root.tags()

    _enqueue(self, id, { name = c.name, class = c.class, pid = c.pid })
    reset_idle(self)
    coroutine.resume(self.thread)
end

function reset_idle(self)
    _reset_idle(self, mouse.coords(), os.time())
end

function connect_signals(self, client)
    client.connect_signal("manage", function (c, startup)
			      self.event(self, "manage", c)
			      self._names[c.pid] = c.name
				    end)

    client.connect_signal("unmanage", function (c, startup)
			      self._names[c.pid] = nil
				      end)

    client.connect_signal("focus", function(c)
			      self.event(self, "focus", c)
				   end)

    client.connect_signal("unfocus", function(c)
			      self.event(self, "unfocus", c)
				     end)

    client.connect_signal("property::name", function(c)
			      if self._names[c.pid] ~= c.name then
				  self._names[c.pid] = c.name
				  self.event(self, "name", c)
			      end
					    end)
end


-- private API

function check_idle(self)
    -- print("check_idle")
    c = mouse.coords()
    now = os.time()
    if c.x ~= self._idle.last_x or c.y ~= self._idle.last_y then
	-- print("  moved")
	reset_idle(self, c, now)
	return
    end

    if now > self._idle.last_tm + self._idle.timeout then
	-- print("  idle")
	_enqueue(self, "idle", nil)
	self._idle.timer:stop()
	self._idle.is_idle = true
    end
end

function _reset_idle(self, c, tm)
    if self._idle.is_idle then
	_enqueue(self, "resume", nil)
	self._idle.is_idle = false
    end

    self._idle.last_x = c.x
    self._idle.last_y = c.y
    self._idle.last_tm = tm

    self._idle.timer:again()
end

function _serialize_u64(v)
    return string.pack(">I8", v)
end

function _serialize_u32(v)
    return string.pack(">I4", v)
end

function _serialize_u16(v)
    return string.pack(">I2", v)
end

function _serialize_u8(v)
    return string.pack(">I1", v)
end

function _serialize_string(v)
    if v == nil then
	return _serialize_string("")
    end

    return string.pack(">s4", v)
end

function _enqueue(self, event, data)
    table.insert(self._queue, { event, os.time(), data } )
end

function _close(self)
    -- print("closing socket")
    self._conn:close()
    self._conn = nil
end

function connect(self)
    -- print("connect", self.connect_fn, self)
    rc, c = pcall(self.connect_fn, self)
    if not rc or c == nil then
	print("failed to connect:", c)
	return
    end

    c:settimeout(0)
    c:setoption('keepalive', true)
    self._try = socket.newtry(function() _close(self) end)

    posix.fcntl(c:getfd(), posix.F_SETFD, 1);

    if self._outbuf_pos > 0 then
	-- clear dirty outbuf buffer
	_reset_buffer(self)
    end

    self._conn = c
    _enqueue(self, '_conn', nil)
end

function _reset_buffer(self)
    self._outbuf_pos = 0
    self._outbuf_len = 0
    self._outbuf     = nil
end

function flush_buffer(self)
    -- print("flush_buffer", self._outbuf_len, self._outbuf_pos)

    if self._outbuf_len == 0 then
	return true
    end

    while self._outbuf_pos < self._outbuf_len do
	t = socket.select(nil, { self._conn }, 0)
	if t == nil then
	    break
	end

	res = self._try(self._conn:send(self._outbuf, self._outbuf_pos + 1,
				       self._outbuf_len))
	self._outbuf_pos = res
    end

    if self._outbuf_pos == self._outbuf_len then
	_reset_buffer(self)
	return true
    end

    return false
end

function _process_queue_item(self)
    e = table.remove(self._queue, 1)
    -- print("preparing", e, e[1], e[2])
    op = e[1]

    t = ''
    if op == 'focus' or op == 'unfocus' or op == 'manage' then
	if op == 'focus' then
	    code = 'F'
	elseif op == 'unfocus' then
	    code = 'f'
	elseif op == 'manage' then
	    code = 'M'
	end
	t = _serialize_u32(e[3].pid) ..
	    _serialize_string(e[3].name) ..
	    _serialize_string(e[3].class)
    elseif op == 'name' then
	print("--")
	d(e)
	code = 'N'
	t = _serialize_u32(e[3].pid) ..
	    _serialize_string(e[3].name)
    elseif op == '_conn' then
	code = 'C'
	t = _serialize_u32(posix.getpid().pid) ..
	    _serialize_string("awesome")
    elseif op == '_dropped' then
	code = 'D'
	t    = _serialize_u32(e[3].count)
    elseif op == 'idle' then
	code = 'I'
    elseif op == 'resume' then
	code = 'i'
    else
	print("unhandled event", op)
	return
    end

    t = code .. _serialize_u64(e[2]) .. t

    self._outbuf     = _serialize_u32(#t) .. t
    self._outbuf_len = #self._outbuf
    self._outbuf_pos = 0
end

function process_queue(self)
    -- print("process_queue()", #self._queue)

    for i = 1, MAX_QUEUE_PER_RUN do
	if not flush_buffer(self) then
	    break
	end

	if #self._queue == 0 then
	    break
	end

	_process_queue_item(self)
    end

    flush_buffer(self)
end

function _run(self)
    if self._conn == nil then
	connect(self)
    end

    if self._conn == nil and #self._queue > QUEUE_LIMIT then
	len = #self._queue
	pos = QUEUE_LIMIT + QUEUE_REMOVAL_EXTRA
	if pos > len then
	    pos = len
	end

	_enqueue(self, '_dropped', { count = pos })
	table.move(self._queue, pos, len, 1)
	print("queue dropped elements from queue")
    end

    if self._conn ~= nil then
	process_queue(self)
	-- d(self._queue)
	-- print("tk.loop")
    end
end

function loop(self)
    while true do
	rc, msg = pcall(_run, self)
	if not rc then
	    print(msg)
	end
	coroutine.yield(#self._queue > 0)
    end
end

return tk
