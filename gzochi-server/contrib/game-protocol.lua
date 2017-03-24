-- Copyright (C) 2017 Julian Graham
-- Wireshark protocol dissector for gzochi game protocol
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this package.  If not, see <http://www.gnu.org/licenses/>.

-- Global protocol declaration

game_proto = Proto("gzochi", "gzochi game Protocol")

-- Protocol field declarations

local pf_app = ProtoField.new("Application", "gzochi-meta.app", ftypes.STRING)
local pf_credentials = ProtoField.new("Credentials", "gzochi-meta.credentials",
				      ftypes.BYTES)
local pf_message = ProtoField.new("Message", "gzochi-meta.message",
				  ftypes.BYTES)
local pf_type = ProtoField.new("Type", "gzochi-meta.type", ftypes.STRING)

game_proto.fields = { pf_app, pf_credentials, pf_message, pf_type }

-- Decodes the two-byte message length prefix from the start of the PDU

function get_len(tvb, pinfo, offset)
   return tvb:range(offset, 2):uint() + 3
end

-- Message-specific dissector functions

function dissect_login_request(tvb, pinfo, tree)
   tree:add(pf_type, "LOGIN_REQUEST")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_credentials, tvb:range(app:len() + 4))
end

function dissect_login_success(tvb, pinfo, tree)
   tree:add(pf_type, "LOGIN_SUCCESS")
end

function dissect_login_failure(tvb, pinfo, tree)
   tree:add(pf_type, "LOGIN_FAILURE")
end

function dissect_logout_request(tvb, pinfo, tree)
   tree:add(pf_type, "LOGOUT_REQUEST")
end

function dissect_logout_success(tvb, pinfo, tree)
   tree:add(pf_type, "LOGOUT_SUCCESS")
end

function dissect_session_disconnected(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_DISCONNECTED")
end

function dissect_session_message(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_MESSAGE")
   tree:add(pf_message, tvb:range(3))
end

-- Dissector function opcode table

msg_type_dissectors = {
   [0x10] = dissect_login_request,
   [0x11] = dissect_login_success,
   [0x12] = dissect_login_failure,
   [0x20] = dissect_logout_request,
   [0x21] = dissect_logout_success,
   [0x30] = dissect_session_disconnected,
   [0x31] = dissect_session_message
}

-- Dissector router function

function dissect(tvb, pinfo, root)
   local type_dissector_func = msg_type_dissectors[tvb:range(2, 1):uint()]

   if type_dissector_func ~= nil then
      local tree = root:add(game_proto,
			    tvb:range(0, tvb:reported_length_remaining()))

      type_dissector_func(tvb, pinfo, tree)
   end
end

-- Protocol dissector front-end; uses the Wireshark API's automatic PDU
-- reassembly mechanism

function game_proto.dissector(tvb, pinfo, tree)
   dissect_tcp_pdus(tvb, tree, 2, get_len, dissect)
end

-- Register the protocol in the dissector table

DissectorTable.get("tcp.port"):add(8001, game_proto)
