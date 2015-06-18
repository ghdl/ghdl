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
-- VHDL unit     : Bitvis Utility Library : types_pkg
--
-- Description   : See library quick reference (under 'doc') and README-file(s)
------------------------------------------------------------------------------------------


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

library ieee_proposed;
use ieee_proposed.standard_additions.all;
use ieee_proposed.standard_textio_additions.all;

package types_pkg is
  file ALERT_FILE : text;
  file LOG_FILE : text;
  
  constant C_LOG_HDR_FOR_WAVEVIEW_WIDTH : natural := 100; -- For string in waveview indicating last log header

  type t_void is (VOID);

  type t_natural_array is array (natural range <>) of natural;
  type t_integer_array is array (natural range <>) of integer;


  -- Note: Most types below have a matching to_string() in 'string_methods_pkg.vhd'

  type t_info_target is (LOG_INFO, ALERT_INFO, USER_INFO);
  type t_alert_level is (NOTE, TB_NOTE, WARNING, TB_WARNING, MANUAL_CHECK, ERROR, TB_ERROR, FAILURE, TB_FAILURE);

  type t_enabled      is (ENABLED, DISABLED);
  type t_attention    is (REGARD, EXPECT, IGNORE);
  type t_radix        is (BIN, HEX, DEC, HEX_BIN_IF_INVALID);
  type t_radix_prefix is (EXCL_RADIX, INCL_RADIX);
  type t_order        is (INTERMEDIATE, FINAL);
  type t_ascii_allow  is (ALLOW_ALL, ALLOW_PRINTABLE_ONLY); 
  type t_blocking_mode is (BLOCKING, NON_BLOCKING);
  type t_from_point_in_time is (FROM_NOW, FROM_LAST_EVENT);

  type t_format_zeros  is (AS_IS, SKIP_LEADING_0);
  type t_format_string is (AS_IS, TRUNCATE, SKIP_LEADING_SPACE);
  
  type t_log_format is (FORMATTED, UNFORMATTED);
  type t_log_if_block_empty is (WRITE_HDR_IF_BLOCK_EMPTY, SKIP_LOG_IF_BLOCK_EMPTY, NOTIFY_IF_BLOCK_EMPTY);

  type t_alert_counters  is array (t_alert_level'left to t_alert_level'right) of natural;
  type t_alert_attention is array (t_alert_level'left to t_alert_level'right) of t_attention;

  type t_attention_counters is array (t_attention'left to t_attention'right) of natural; -- Only used to build below type
  type t_alert_attention_counters is array (t_alert_level'left to t_alert_level'right) of t_attention_counters;
  
  type t_quietness  is (NON_QUIET, QUIET);
  
  type t_deprecate_setting is (NO_DEPRECATE, DEPRECATE_ONCE, ALWAYS_DEPRECATE);
  type t_deprecate_list is array(0 to 9) of string(1 to 100);
  
  type t_global_ctrl is record
    attention  : t_alert_attention;
    stop_limit : t_alert_counters;
  end record;
  
  type t_current_log_hdr is record
    normal     : string(1 to C_LOG_HDR_FOR_WAVEVIEW_WIDTH);
    large      : string(1 to C_LOG_HDR_FOR_WAVEVIEW_WIDTH);
    xl         : string(1 to C_LOG_HDR_FOR_WAVEVIEW_WIDTH);
  end record;

  -------------------------------------
  -- BFMs and above
  -------------------------------------
  type t_transaction_result is (ACK, NAK, ERROR);  -- add more when needed



end package types_pkg;

package body types_pkg is
end package body types_pkg;
