-- Copyright (C) 2017 Julian Graham
-- Wireshark protocol dissector for gzochi meta protocol
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

meta_proto = Proto("gzochi-meta", "gzochi meta Protocol")

-- Protocol field declarations

local pf_admin_url = ProtoField.new("Admin server URL", "gzochi-meta.admin_url",
				    ftypes.STRING)
local pf_app = ProtoField.new("Application", "gzochi-meta.app", ftypes.STRING)
local pf_block_size = ProtoField.new("Block size", "gzochi-meta.block_size",
				     ftypes.UINT32)
local pf_channel_oid = ProtoField.new("Channel OID", "gzochi-meta.channel_oid",
				      ftypes.UINT64)
local pf_deleted = ProtoField.new("Deleted?", "gzochi-meta.deleted",
				  ftypes.BOOLEAN)
local pf_first_oid = ProtoField.new("First OID", "gzochi-meta.first_oid",
				    ftypes.UINT64)
local pf_for_write = ProtoField.new("For write?", "gzochi-meta.for_write",
				    ftypes.BOOLEAN)
local pf_from_key = ProtoField.new("From key", "gzochi-meta.from_key",
				   ftypes.BYTES)
local pf_key = ProtoField.new("Key", "gzochi-meta.key", ftypes.BYTES)
local pf_message = ProtoField.new("Message", "gzochi-meta.message",
				  ftypes.BYTES)
local pf_session_oid = ProtoField.new("Session OID", "gzochi-meta.session_oid",
				      ftypes.UINT64)
local pf_store = ProtoField.new("Store", "gzochi-meta.store", ftypes.STRING)
local pf_success = ProtoField.new("Success?", "gzochi-meta.success",
				  ftypes.BOOLEAN) 
local pf_to_key = ProtoField.new("To key", "gzochi-meta.to_key", ftypes.BYTES)
local pf_type = ProtoField.new("Type", "gzochi-meta.type", ftypes.STRING)
local pf_value = ProtoField.new("Value", "gzochi-meta.value", ftypes.BYTES)
local pf_version = ProtoField.new("Version", "gzochi-meta.version",
				  ftypes.UINT8)

meta_proto.fields = {
   pf_admin_url,
   pf_app,
   pf_block_size,
   pf_channel_oid,
   pf_deleted,
   pf_first_oid,
   pf_for_write,
   pf_from_key,
   pf_key,
   pf_message,
   pf_session_oid,
   pf_store,
   pf_success,
   pf_to_key,
   pf_type,
   pf_value,
   pf_version
}

-- Decodes the two-byte message length prefix from the start of the PDU

function get_len(tvb, pinfo, offset)
   return tvb:range(offset, 2):uint() + 3
end

-- Message-specific dissector functions

function dissect_meta_login(tvb, pinfo, tree)
   tree:add(pf_type, "META_LOGIN")
   tree:add(pf_version, tvb:range(3, 1):uint())

   if (tvb:len() > 5) then
      tree:add(pf_admin_url, tvb:range(4):stringz())
   end
end

function dissect_meta_login_response(tvb, pinfo, tree)
   tree:add(pf_type, "META_LOGIN_RESPONSE")
   tree:add(pf_version, tvb:range(3, 1):uint())

   if (tvb:len() > 5) then
      tree:add(pf_admin_url, tvb:range(4):stringz())
   end
end

function dissect_data_request_oids(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_REQUEST_OIDS")
   tree:add(pf_app, tvb:range(3):stringz())
end

function dissect_data_request_value(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_REQUEST_VALUE")
   
   local app = tvb:range(3):stringz()
   local store = tvb:range(app:len() + 4):stringz()
   local appstorelen = app:len() + store:len() 
   
   tree:add(pf_app, app)
   tree:add(pf_store, store)
   tree:add(pf_for_write, tvb:range(appstorelen + 5, 1):uint())
   tree:add(pf_key, tvb:range(appstorelen + 8))
end

function dissect_data_request_next_key(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_REQUEST_NEXT_KEY")

   local app = tvb:range(3):stringz()
   local store = tvb:range(app:len() + 4):stringz()
   local appstorelen = app:len() + store:len() 
   
   tree:add(pf_app, app)
   tree:add(pf_store, store)
   tree:add(pf_key, tvb:range(appstorelen + 7))
end

function dissect_data_submit_changeset(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_SUBMIT_CHANGESET")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)

   local num_changes = tvb:range(app:len() + 4, 2):uint()
   local offset = app:len() + 6
   
   for i = 1, num_changes, 1 do
      local store = tvb:range(offset):stringz()
      local change_len = store:len() + 1      
      local key_len = tvb:range(offset + change_len, 2):uint()

      change_len = change_len + key_len + 2

      local value_len = tvb:range(offset + change_len, 2):uint()

      change_len = change_len + value_len + 2
      
      local change = tree:add(meta_proto, tvb:range(offset, change_len))
      
      change:add(pf_store, store)
      change:add(pf_key, tvb:range(offset + store:len() + 3, key_len))

      if value_len > 0 then
	 change:add(pf_deleted, 0)
	 change:add(pf_value,
		    tvb:range(offset + store:len() + key_len + 5, value_len))
      else
	 change:add(pf_deleted, 1)
      end
      
      offset = offset + change_len
   end
end

function dissect_data_release_key(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_RELEASE_KEY")

   local app = tvb:range(3):stringz()
   local store = tvb:range(app:len() + 4):stringz()
   local appstorelen = app:len() + store:len() 

   tree:add(pf_app, app)
   tree:add(pf_store, store)
   tree:add(pf_key, tvb:range(appstorelen + 7))
end

function dissect_data_release_key_range(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_RELEASE_KEY_RANGE")

   local app = tvb:range(3):stringz()
   local store = tvb:range(app:len() + 4):stringz()
   local appstorelen = app:len() + store:len() 
   
   tree:add(pf_app, app)
   tree:add(pf_store, store)

   local from_key_len = tvb:range(appstorelen + 7, 2):uint()

   tree:add(pf_from_key, tvb_range(appstorelen + 9))
   tree:add(pf_to_key, tvb_range(appstorelen + from_key_len + 11))
end

function dissect_data_oids_response(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_OIDS_RESPONSE")

   local app = tvb:range(3):stringz()

   tree:add(pf_first_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_block_size, tvb:range(app:len() + 12, 2):uint())
end

function dissect_data_value_response(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_VALUE_RESPONSE")

   local app = tvb:range(3):stringz()
   local store = tvb:range(app:len() + 4):stringz()
   local appstorelen = app:len() + store:len() 

   tree:add(pf_app, app)
   tree:add(pf_store, store)

   local success = tvb:range(appstorelen + 5, 1):uint()

   tree:add(pf_success, success)
   
   if success == 1 then
      tree:add(pf_value, tvb:range(appstorelen + 8))
   end
end

function dissect_data_next_key_response(tvb, pinfo, tree)
   tree:add(pf_type, "DATA_NEXT_KEY_RESPONSE")

   local app = tvb:range(3):stringz()
   local store = tvb:range(app:len() + 4):stringz()
   local appstorelen = app:len() + store:len() 

   tree:add(pf_app, app)
   tree:add(pf_store, store)

   local success = tvb:range(appstorelen + 5, 1):uint()

   tree:add(pf_success, success)
   
   if success == 1 then
      tree:add(pf_key, tvb:range(appstorelen + 8))
   end
end

function dissect_session_connected(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_CONNECTED")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_session_oid, tvb:range(app:len() + 4, 8):uint64())
end

function dissect_session_disconnected(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_DISCONNECTED")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_session_oid, tvb:range(app:len() + 4, 8):uint64())
end

function dissect_session_relay_disconect_from(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_RELAY_DISCONNECT_FROM")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_session_oid, tvb:range(app:len() + 4, 8):uint64())
end

function dissect_session_relay_disconnect_to(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_RELAY_DISCONNECT_TO")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_session_oid, tvb:range(app:len() + 4, 8):uint64())
end

function dissect_session_relay_message_from(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_RELAY_MESSAGE_FROM")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_session_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_message, tvb:range(app:len() + 14))
end

function dissect_session_relay_message_to(tvb, pinfo, tree)
   tree:add(pf_type, "SESSION_RELAY_MESSAGE_TO")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_session_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_message, tvb:range(app:len() + 14))
end

function dissect_channel_relay_join_from(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_JOIN_FROM")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_session_oid, tvb:range(app:len() + 12, 8):uint64())
end

function dissect_channel_relay_join_to(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_JOIN_TO")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_session_oid, tvb:range(app:len() + 12, 8):uint64())
end

function dissect_channel_relay_leave_from(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_LEAVE_FROM")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_session_oid, tvb:range(app:len() + 12, 8):uint64())
end

function dissect_channel_relay_leave_to(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_LEAVE_TO")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_session_oid, tvb:range(app:len() + 12, 8):uint64())
end

function dissect_channel_relay_message_from(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_MESSAGE_FROM")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_message, tvb:range(app:len() + 12))
end

function dissect_channel_relay_message_to(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_MESSAGE_TO")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
   tree:add(pf_message, tvb:range(app:len() + 12))
end

function dissect_channel_relay_close_from(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_CLOSE_FROM")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
end

function dissect_channel_relay_close_to(tvb, pinfo, tree)
   tree:add(pf_type, "CHANNEL_RELAY_CLOSE_TO")

   local app = tvb:range(3):stringz()

   tree:add(pf_app, app)
   tree:add(pf_channel_oid, tvb:range(app:len() + 4, 8):uint64())
end

-- Dissector function opcode table

msg_type_dissectors = {
   [0x10] = dissect_meta_login,
   [0x11] = dissect_meta_login_response,
   [0x20] = dissect_data_request_oids,
   [0x21] = dissect_data_request_value,
   [0x22] = dissect_data_request_next_key,
   [0x30] = dissect_data_submit_changeset,
   [0x40] = dissect_data_release_key,
   [0x42] = dissect_data_release_key_range,
   [0x50] = dissect_data_oids_response,
   [0x51] = dissect_data_value_response,
   [0x52] = dissect_data_next_key_response,
   [0x60] = dissect_session_connected,
   [0x61] = dissect_session_disconnected,
   [0x62] = dissect_session_relay_disconnect_from,
   [0x63] = dissect_session_relay_disconnect_to,
   [0x64] = dissect_session_relay_message_from,
   [0x65] = dissect_session_relay_message_to,
   [0x70] = dissect_channel_relay_join_from,
   [0x71] = dissect_channel_relay_join_to,
   [0x72] = dissect_channel_relay_leave_from,
   [0x73] = dissect_channel_relay_leave_to,
   [0x74] = dissect_channel_relay_message_from,
   [0x75] = dissect_channel_relay_message_to,
   [0x76] = dissect_channel_relay_close_from,
   [0x77] = dissect_channel_relay_close_to
}

-- Dissector router function

function dissect(tvb, pinfo, root)
   local type_dissector_func = msg_type_dissectors[tvb:range(2, 1):uint()]

   if type_dissector_func ~= nil then
      local tree = root:add(meta_proto,
			    tvb:range(0, tvb:reported_length_remaining()))

      type_dissector_func(tvb, pinfo, tree)
   end
end

-- Protocol dissector front-end; uses the Wireshark API's automatic PDU
-- reassembly mechanism

function meta_proto.dissector(tvb, pinfo, tree)
   dissect_tcp_pdus(tvb, tree, 2, get_len, dissect)
end

-- Register the protocol in the dissector table

DissectorTable.get("tcp.port"):add(9001, meta_proto)
