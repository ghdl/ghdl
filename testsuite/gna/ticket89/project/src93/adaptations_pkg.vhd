--========================================================================================================================
-- Copyright (c) 2015 by Bitvis AS.  All rights reserved.
-- A free license is hereby granted, free of charge, to any person obtaining
-- a copy of this VHDL code and associated documentation files (for 'Bitvis Utility Library'),
-- to use, copy, modify, merge, publish and/or distribute - subject to the following conditions:
--  - This copyright notice shall be included as is in all copies or substantial portions of the code and documentation
--  - The files included in Bitvis Utility Library may only be used as a part of this library as a whole
--  - The License file may not be modified
--  - The calls in the code to the license file ('show_license') may not be removed or modified.
--  - No other conditions whatsoever may be added to those of this License

-- BITVIS UTILITY LIBRARY AND ANY PART THEREOF ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
-- INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH BITVIS UTILITY LIBRARY.
--========================================================================================================================

------------------------------------------------------------------------------------------
-- VHDL unit     : Bitvis Utility Library : adaptations_pkg
--
-- Description   : See library quick reference (under 'doc') and README-file(s)
------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

library ieee_proposed;
use ieee_proposed.standard_additions.all;
use ieee_proposed.standard_textio_additions.all;

use work.types_pkg.all;

package adaptations_pkg is
  constant C_ALERT_FILE_NAME : string := "_Alert.txt";
  constant C_LOG_FILE_NAME   : string := "_Log.txt";

  constant C_SHOW_BITVIS_UTILITY_LIBRARY_INFO         : boolean := true;  -- Set this to false when you no longer need the initial info
  constant C_SHOW_BITVIS_UTILITY_LIBRARY_RELEASE_INFO : boolean := true;  -- Set this to false when you no longer need the release info

  -------------------------------------------------------------------------------
  -- Log format
  -------------------------------------------------------------------------------
  --Bitvis: [<ID>]  <time>  <Scope>        Msg
  --PPPPPPPPIIIIII TTTTTTTT  SSSSSSSSSSSSSS MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
  constant C_LOG_PREFIX : string := "Bitvis: "; -- Note: ': ' is recommended as final characters

  constant C_LOG_PREFIX_WIDTH  : natural := C_LOG_PREFIX'length;
  constant C_LOG_MSG_ID_WIDTH  : natural := 20;
  constant C_LOG_TIME_WIDTH    : natural := 16; -- 3 chars used for unit eg. " ns"
  constant C_LOG_TIME_BASE     : time    := ns; -- Unit in which time is shown in log (ns | ps)
  constant C_LOG_TIME_DECIMALS : natural := 1; -- Decimals to show for given C_LOG_TIME_BASE
  constant C_LOG_SCOPE_WIDTH   : natural := 16;
  constant C_LOG_LINE_WIDTH    : natural := 150;
  constant C_LOG_INFO_WIDTH    : natural := C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH;

  constant C_USE_BACKSLASH_N_AS_LF : boolean := true; -- If true interprets '\n' as Line feed

  constant C_SINGLE_LINE_ALERT  : boolean := false; -- If true prints alerts on a single line.
  constant C_SINGLE_LINE_LOG    : boolean := false; -- If true prints log messages on a single line.

  constant C_TB_SCOPE_DEFAULT : string := "TB seq."; -- Default scope in test sequencer

  constant C_LOG_TIME_TRUNC_WARNING : boolean := true; -- Yields a single TB_WARNING if time stamp truncated. Otherwise none
  signal global_show_log_id         : boolean := true;
  signal global_show_log_scope      : boolean := true;

  -- UVVM dedicated. May be moved to separate UVVM adaptation package
  signal global_show_msg_for_uvvm_cmd  : boolean := true;
  -- End of UVVM dedicated

  -------------------------------------------------------------------------------
  -- Verbosity control
  -- NOTE: Do not enter new IDs without proper evaluation:
  --       1. Is it - or could it be covered by an existing ID
  --       2. Could it be combined with other needs for a more general new ID
  --       Feel free to suggest new ID for future versions of Bitvis Utility Library (info@bitvis.no)
  -------------------------------------------------------------------------------
  type t_msg_id is (
    -- Bitvis utility methods
    NO_ID,                -- Used as default prior to setting actual ID when transfering ID as a field in a record
    ID_UTIL_BURIED,       -- Used for buried log messages where msg and scope cannot be modified from outside
    ID_UTIL_SETUP,        -- Used for Utility setup
    ID_LOG_MSG_CTRL,      -- Used inside Utility library only - when enabling/disabling msg IDs.
    ID_ALERT_CTRL,        -- Used inside Utility library only - when setting IGNORE or REGARD on various alerts.
    ID_NEVER,             -- Used for avoiding log entry. Cannot be enabled.
    ID_CLOCK_GEN,         -- Used for logging when clock generators are enabled or disabled
    ID_GEN_PULSE,         -- Used for logging when a gen_pulse procedure starts pulsing a signal
    -- General
    ID_POS_ACK,           -- To write a positive acknowledge on a check
    -- Directly inside test sequencers
    ID_LOG_HDR,           -- ONLY allowed in test sequencer, Log section headers
    ID_LOG_HDR_LARGE,     -- ONLY allowed in test sequencer, Large log section headers
    ID_LOG_HDR_XL,        -- ONLY allowed in test sequencer, Extra large log section headers
    ID_SEQUENCER,         -- ONLY allowed in test sequencer, Normal log (not log headers)
    ID_SEQUENCER_SUB,     -- ONLY allowed in test sequencer, Subprograms defined in sequencer
    -- BFMs
    ID_BFM,               -- Used inside a BFM (to log BFM access)
    ID_BFM_WAIT,          -- Used inside a BFM to indicate that it is waiting for something (e.g. for ready)
    -- Packet related data Ids with three levels of granularity, for differentiating between frames, packets and segments.
    -- Segment Ids, finest granularity of packet data
    ID_SEGMENT_INITIATE,   -- Notify that a packet is about to be transmitted or received
    ID_SEGMENT_COMPLETE,   -- Notify that a packet has been transmitted or received
    ID_SEGMENT_HDR,        -- AS ID_SEGMENT_COMPLETE, but also writes header info
    ID_SEGMENT_DATA,       -- AS ID_SEGMENT_COMPLETE, but also writes packet data (could be huge)
    -- Packet Ids, medium granularity of packet data
    ID_PACKET_INITIATE,   -- Notify that a packet is about to be transmitted or received
    ID_PACKET_COMPLETE,   -- Notify that a packet has been transmitted or received
    ID_PACKET_HDR,        -- AS ID_PACKET_COMPLETED, but also writes header info
    ID_PACKET_DATA,       -- AS ID_PACKET_COMPLETED, but also writes packet data (could be huge)
    -- Frame Ids, roughest granularity of packet data
    ID_FRAME_INITIATE,     -- Notify that a packet is about to be transmitted or received
    ID_FRAME_COMPLETE,    -- Notify that a packet has been transmitted or received
    ID_FRAME_HDR,          -- AS ID_FRAME_COMPLETE, but also writes header info
    ID_FRAME_DATA,         -- AS ID_FRAME_COMPLETE, but also writes packet data (could be huge)
    -- Distributed command systems
    ID_UVVM_SEND_CMD,
    ID_UVVM_CMD_ACK,
    ID_UVVM_CMD_RESULT,
    ID_INTERPRETER,       -- Message from VVC interpreter about correctly received and queued/issued command
    ID_INTERPRETER_WAIT,  -- Message from VVC interpreter that it is actively waiting for a command
    ID_IMMEDIATE,         -- Message from VVC interpreter that an IMMEDIATE command has been executed
    ID_IMMEDIATE_WAIT,    -- Message from VVC interpreter that an IMMEDIATE command is waiting for command to complete
    ID_EXECUTOR,          -- Message from VVC executor about correctly received command - prior to actual execution
    ID_EXECUTOR_WAIT,     -- Message from VVC executor that it is actively waiting for a command
    -- VVC system
    ID_VVC_CONSTRUCTOR,   -- Constructor message from VVCs
    -- Special purpose - Not really IDs
    ALL_MESSAGES          -- Applies to ALL message ID apart from ID_NEVER
    );
  type  t_msg_id_panel is array (t_msg_id'left to t_msg_id'right) of t_enabled;

  constant C_DEFAULT_MSG_ID_PANEL : t_msg_id_panel := (
    ID_NEVER         => DISABLED,
    ID_UTIL_BURIED   => DISABLED,
    others           => ENABLED
  );

  type  t_msg_id_indent is array (t_msg_id'left to t_msg_id'right) of string(1 to 4);
  constant C_MSG_ID_INDENT : t_msg_id_indent := (
    ID_IMMEDIATE_WAIT     => "  ..",
    ID_INTERPRETER        => "  "   & NUL & NUL,
    ID_INTERPRETER_WAIT   => "  ..",
    ID_EXECUTOR           => "  "   & NUL & NUL,
    ID_EXECUTOR_WAIT      => "  ..",
    ID_UVVM_SEND_CMD      => "->"   & NUL & NUL,
    ID_UVVM_CMD_ACK       => "    ",
    others                => ""     & NUL & NUL & NUL & NUL
  );

  -------------------------------------------------------------------------
  -- Alert counters
  -------------------------------------------------------------------------
  -- Default values. These can be overwritten in each sequencer by using
  -- set_alert_attention or set_alert_stop_limit (see quick ref).
  constant C_DEFAULT_ALERT_ATTENTION : t_alert_attention := (others => REGARD);

  -- 0 = Never stop
  constant C_DEFAULT_STOP_LIMIT : t_alert_counters := (note to manual_check => 0,
                                                       others               => 1);

  -------------------------------------------------------------------------
  -- Deprecate
  -------------------------------------------------------------------------
  -- These values are used to indicate outdated sub-programs
  constant C_DEPRECATE_SETTING : t_deprecate_setting := DEPRECATE_ONCE;
  shared variable deprecated_subprogram_list : t_deprecate_list := (others=>(others => ' '));

end package adaptations_pkg;

package body adaptations_pkg is
end package body adaptations_pkg;
