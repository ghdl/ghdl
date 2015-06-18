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
-- VHDL unit     : Bitvis Utility Library : methods_pkg
--
-- Description   : See library quick reference (under 'doc') and README-file(s)
------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.types_pkg.all;
use work.string_methods_pkg.all;
use work.adaptations_pkg.all;
--use work.protected_types_pkg.all;
use work.vhdl_version_layer_pkg.all;
use work.license_pkg.all;


library ieee_proposed;
use ieee_proposed.standard_additions.all;
use ieee_proposed.std_logic_1164_additions.all;
use ieee_proposed.standard_textio_additions.all;

package methods_pkg is



  -- Shared variables
  shared variable shared_initialised_util        : boolean  := false;
  shared variable shared_msg_id_panel            : t_msg_id_panel   := C_DEFAULT_MSG_ID_PANEL;
  shared variable shared_log_file_name_is_set    : boolean  := false;
  shared variable shared_alert_file_name_is_set  : boolean  := false;
  shared variable shared_warned_time_stamp_trunc : boolean  := false;
  shared variable shared_alert_attention         : t_alert_attention:= C_DEFAULT_ALERT_ATTENTION;
  shared variable shared_stop_limit              : t_alert_counters := C_DEFAULT_STOP_LIMIT;
  shared variable shared_log_hdr_for_waveview    : string(1 to C_LOG_HDR_FOR_WAVEVIEW_WIDTH);
  shared variable shared_current_log_hdr         : t_current_log_hdr;
  shared variable shared_seed1                   : positive;
  shared variable shared_seed2                   : positive;


-- -- ============================================================================
-- -- Initialisation and license
-- -- ============================================================================
--   procedure initialise_util(
--     constant dummy  : in t_void
--     );
--

-- ============================================================================
-- File handling (that needs to use other utility methods)
-- ============================================================================
  procedure check_file_open_status(
    constant status      : in file_open_status;
    constant file_name   : in string
    );

  procedure set_alert_file_name(
    constant file_name   : string := C_ALERT_FILE_NAME;
    constant msg_id      : t_msg_id  := ID_UTIL_SETUP
    );

  procedure set_log_file_name(
    constant file_name   : string := C_LOG_FILE_NAME;
    constant msg_id      : t_msg_id  := ID_UTIL_SETUP
    );


-- ============================================================================
-- Log-related
-- ============================================================================
  procedure log(
    msg_id         : t_msg_id;
    msg            : string;
    scope          : string         := C_TB_SCOPE_DEFAULT;
    msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
    );

  procedure log_text_block(
    msg_id                : t_msg_id;
    variable text_block   : inout line;
    formatting            : t_log_format;  -- FORMATTED or UNFORMATTED
    msg_header            : string         := "";
    log_if_block_empty    : t_log_if_block_empty := WRITE_HDR_IF_BLOCK_EMPTY;
    scope                 : string         := C_TB_SCOPE_DEFAULT;
    msg_id_panel          : t_msg_id_panel := shared_msg_id_panel
    );

  -- Enable and Disable do not have a Scope parameter as they are only allowed from main test sequencer
  procedure enable_log_msg(
    constant msg_id         : t_msg_id;
    variable msg_id_panel   : inout t_msg_id_panel;
    constant msg            : string := "";
    constant scope          : string := C_TB_SCOPE_DEFAULT
    );

  procedure enable_log_msg(
    msg_id         : t_msg_id;
    msg            : string  := ""
    ) ;
  procedure disable_log_msg(
    constant msg_id         : t_msg_id;
    variable msg_id_panel   : inout t_msg_id_panel;
    constant msg            : string  := "";
    constant scope          : string  := C_TB_SCOPE_DEFAULT;
    constant quietness      : t_quietness := NON_QUIET
    );

  procedure disable_log_msg(
    msg_id         : t_msg_id;
    msg            : string  := "";
    quietness      : t_quietness := NON_QUIET
    );

  impure function is_log_msg_enabled(
    msg_id        : t_msg_id;
    msg_id_panel  : t_msg_id_panel :=  shared_msg_id_panel
    ) return boolean;


-- ============================================================================
-- Alert-related
-- ============================================================================
  procedure alert(
    constant alert_level : t_alert_level;
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  -- Dedicated alert-procedures all alert levels (less verbose - as 2 rather than 3 parameters...)
  procedure note(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure tb_note(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure warning(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure tb_warning(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure manual_check(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure error(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure tb_error(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure failure(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure tb_failure(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    );

  procedure increment_expected_alerts(
    constant alert_level  : t_alert_level;
    constant number       : natural := 1;
    constant msg          : string  := "";
    constant scope        : string  := C_TB_SCOPE_DEFAULT
  );

  procedure report_alert_counters(
    constant order  : in t_order
  );

  procedure report_alert_counters(
    constant dummy  : in t_void
  );

  procedure report_global_ctrl(
    constant dummy  : in t_void
  );

  procedure report_msg_id_panel(
    constant dummy  : in t_void
  );

  procedure set_alert_attention(
      alert_level : t_alert_level;
      attention   : t_attention;
      msg         : string  := ""
  );

  impure function get_alert_attention(
      alert_level : t_alert_level
  ) return t_attention;

  procedure set_alert_stop_limit(
      alert_level : t_alert_level;
      value       : natural
  );

  impure function get_alert_stop_limit(
      alert_level : t_alert_level
  ) return natural;


-- ============================================================================
-- Deprecate message
-- ============================================================================

  procedure deprecate(
    caller_name : string;
    constant msg          : string  := ""
  );


-- ============================================================================
-- Non time consuming checks
-- ============================================================================

  -- Matching if same width or only zeros in "extended width"
  function matching_widths(
    value1: std_logic_vector;
    value2: std_logic_vector
    ) return boolean;

  function matching_widths(
    value1: unsigned;
    value2: unsigned
    ) return boolean;

  function matching_widths(
    value1: signed;
    value2: signed
    ) return boolean;

  -- function version of check_value (with return value)
  impure function check_value(
    constant value       : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean ;

  impure function check_value(
    constant value       : boolean;
    constant exp         : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean ;

  impure function check_value(
    constant value       : std_logic;
    constant exp         : std_logic;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean ;

  impure function check_value(
    constant value       : std_logic_vector;
    constant exp         : std_logic_vector;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "slv"
    ) return boolean ;

  impure function check_value(
    constant value       : unsigned;
    constant exp         : unsigned;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "unsigned"
    ) return boolean ;

  impure function check_value(
    constant value       : signed;
    constant exp         : signed;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "signed"
    ) return boolean ;


  impure function check_value(
    constant value       : integer;
    constant exp         : integer;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean ;

  impure function check_value(
    constant value       : time;
    constant exp         : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean ;

  impure function check_value(
    constant value       : string;
    constant exp         : string;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean ;

  -- procedure version of check_value (no return value)
  procedure check_value(
    constant value       : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    );

  procedure check_value(
    constant value       : boolean;
    constant exp         : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    );

  procedure check_value(
    constant value       : std_logic_vector;
    constant exp         : std_logic_vector;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "slv"
    );

  procedure check_value(
    constant value       : unsigned;
    constant exp         : unsigned;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "unsigned"
    );

  procedure check_value(
    constant value       : signed;
    constant exp         : signed;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "signed"
    );


  procedure check_value(
    constant value       : std_logic;
    constant exp         : std_logic;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    );

  procedure check_value(
    constant value       : integer;
    constant exp         : integer;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    );

  procedure check_value(
    constant value       : time;
    constant exp         : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    );

  procedure check_value(
    constant value       : string;
    constant exp         : string;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    );

  -- Check_value_in_range
  impure function check_value_in_range (
    constant value        : integer;
    constant min_value    : integer;
    constant max_value    : integer;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()";
    constant value_type   : string         := "integer"
    ) return boolean;

  impure function check_value_in_range (
    constant value        : unsigned;
    constant min_value    : unsigned;
    constant max_value    : unsigned;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()";
    constant value_type   : string         := "unsigned"
    ) return boolean;

  impure function check_value_in_range (
    constant value        : signed;
    constant min_value    : signed;
    constant max_value    : signed;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()";
    constant value_type   : string         := "signed"
    ) return boolean;

  impure function check_value_in_range (
    constant value        : time;
    constant min_value    : time;
    constant max_value    : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()"
    ) return boolean;

  impure function check_value_in_range (
    constant value        : real;
    constant min_value    : real;
    constant max_value    : real;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()"
    ) return boolean;

  -- Procedure overloads for check_value_in_range
  procedure check_value_in_range (
    constant value       : integer;
    constant min_value   : integer;
    constant max_value   : integer;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()"
  );

  procedure check_value_in_range (
    constant value       : unsigned;
    constant min_value   : unsigned;
    constant max_value   : unsigned;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
  );

  procedure check_value_in_range (
    constant value       : signed;
    constant min_value   : signed;
    constant max_value   : signed;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
  );

  procedure check_value_in_range (
    constant value       : time;
    constant min_value   : time;
    constant max_value   : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
  );

  procedure check_value_in_range (
    constant value       : real;
    constant min_value   : real;
    constant max_value   : real;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
  );

  -- Check_stable
  procedure check_stable(
    signal   target      : boolean;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "boolean"
    );

  procedure check_stable(
    signal   target      : std_logic_vector;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "slv"
    );

  procedure check_stable(
    signal   target      : unsigned;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "unsigned"
    );

  procedure check_stable(
    signal   target      : signed;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "signed"
    );

  procedure check_stable(
    signal   target      : std_logic;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "std_logic"
    );

  procedure check_stable(
    signal   target      : integer;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "integer"
    );

  impure function random (
    constant length : integer
  )  return std_logic_vector;

  impure function random (
    constant VOID : t_void
  )  return std_logic;

  impure function random (
    constant min_value : integer;
    constant max_value : integer
  ) return integer;

  impure function random (
    constant min_value : real;
    constant max_value : real
  ) return real;

  impure function random (
    constant min_value : time;
    constant max_value : time
  ) return time;

  procedure random (
    variable v_seed1       : inout positive;
    variable v_seed2       : inout positive;
    variable v_target      : inout std_logic_vector
  );

  procedure random (
    variable v_seed1       : inout positive;
    variable v_seed2       : inout positive;
    variable v_target      : inout std_logic
  );

  procedure random (
    constant min_value : integer;
    constant max_value : integer;
    variable v_seed1   : inout positive;
    variable v_seed2   : inout positive;
    variable v_target  : inout integer
  );

  procedure random (
    constant min_value : real;
    constant max_value : real;
    variable v_seed1   : inout positive;
    variable v_seed2   : inout positive;
    variable v_target  : inout real
  );

  procedure random (
    constant min_value : time;
    constant max_value : time;
    variable v_seed1   : inout positive;
    variable v_seed2   : inout positive;
    variable v_target  : inout time
  );

  procedure randomize (
    constant seed1 : positive;
    constant seed2 : positive;
    constant msg   : string         := "randomizing seeds";
    constant scope : string         := C_TB_SCOPE_DEFAULT
  );

  procedure randomise (
    constant seed1 : positive;
    constant seed2 : positive;
    constant msg   : string         := "randomising seeds";
    constant scope : string         := C_TB_SCOPE_DEFAULT
  );

-- ============================================================================
-- Time consuming checks
-- ============================================================================

  procedure await_change(
    signal   target      : boolean;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant value_type  : string          := "boolean"
  );

  procedure await_change(
    signal   target      : std_logic;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant value_type  : string          := "std_logic"
  );

  procedure await_change(
    signal   target      : std_logic_vector;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant value_type  : string          := "slv"
  );

  procedure await_change(
    signal   target      : unsigned;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant value_type  : string          := "unsigned"
  );

  procedure await_change(
    signal   target      : signed;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant value_type  : string          := "signed"
  );

  procedure await_change(
    signal   target      : integer;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant value_type  : string          := "integer"
  );

  procedure await_value (
    signal   target       : boolean;
    constant exp          : boolean;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_value (
    signal   target       : std_logic;
    constant exp          : std_logic;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_value (
    signal   target       : std_logic_vector;
    constant exp          : std_logic_vector;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant radix        : t_radix         := HEX_BIN_IF_INVALID;
    constant format       : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_value (
    signal   target       : unsigned;
    constant exp          : unsigned;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant radix        : t_radix         := HEX_BIN_IF_INVALID;
    constant format       : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_value (
    signal   target       : signed;
    constant exp          : signed;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant radix        : t_radix         := HEX_BIN_IF_INVALID;
    constant format       : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_value (
    signal   target       : integer;
    constant exp          : integer;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_stable (
    signal   target           : boolean;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_stable (
    signal   target           : std_logic;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_stable (
    signal   target           : std_logic_vector;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_stable (
    signal   target           : unsigned;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_stable (
    signal   target           : signed;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure await_stable (
    signal   target           : integer;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
  );

  procedure gen_pulse(
    signal   target         : inout std_logic;
    constant pulse_duration : time;
    constant blocking_mode  : t_blocking_mode;
    constant msg            : string;
    constant scope          : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id         : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
  );

  procedure gen_pulse(
    signal   target         : inout std_logic;
    constant pulse_duration : time;
    constant msg            : string;
    constant scope          : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id         : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
  );

  procedure gen_pulse(
    signal   target         : inout std_logic;
    signal   clock_signal   : std_logic;
    constant num_periods    : natural;
    constant msg            : string;
    constant scope          : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id         : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
  );

  procedure gen_pulse(
    signal   target        : inout  std_logic_vector;
    constant pulse_value   : std_logic_vector;
    signal   clock_signal  : std_logic;
    constant num_periods   : natural;
    constant msg           : string;
    constant scope         : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id        : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel  : t_msg_id_panel := shared_msg_id_panel
  );

  procedure clock_generator(
    signal   clock_signal  : inout std_logic;
    constant clock_period  : in    time
  );

  -- Overloaded version with additional arguments
  procedure clock_generator(
    signal   clock_signal  : inout std_logic;
    signal   clock_ena     : in    boolean;
    constant clock_period  : in    time;
    constant clock_name    : in    string
  );
  
  procedure deallocate_line_if_exists(
     variable line_to_be_deallocated      : inout line
  );

end package methods_pkg;


--=================================================================================================
--=================================================================================================
--=================================================================================================

package body methods_pkg is

  constant C_BURIED_SCOPE : string := "(Util buried)";

  -- The following constants are not used. Report statements in the given functions allow elaboration time messages
  constant C_BITVIS_LICENSE_INITIALISED         : boolean := show_license(VOID);
  constant C_BITVIS_LIBRARY_INFO_SHOWN          : boolean := show_bitvis_utility_library_info(VOID);
  constant C_BITVIS_LIBRARY_RELEASE_INFO_SHOWN  : boolean := show_bitvis_utility_library_release_info(VOID);


-- ============================================================================
-- Initialisation and license
-- ============================================================================

--   -- Executed a single time ONLY
--   procedure pot_show_license(
--     constant dummy  : in t_void
--     ) is
--   begin
--     if not shared_license_shown then
--       show_license(v_trial_license);
--       shared_license_shown := true;
--     end if;
--   end;

--   -- Executed a single time ONLY
--   procedure initialise_util(
--     constant dummy  : in t_void
--     ) is
--   begin
--     set_log_file_name(C_LOG_FILE_NAME);
--     set_alert_file_name(C_ALERT_FILE_NAME);
--     shared_license_shown.set(1);
--     shared_initialised_util.set(true);
--   end;

  procedure pot_initialise_util(
    constant dummy  : in t_void
    ) is
  begin
    if not shared_initialised_util then
      shared_initialised_util := true;
      if not shared_log_file_name_is_set then
        set_log_file_name(C_LOG_FILE_NAME, ID_NEVER);
      end if;
      if not shared_alert_file_name_is_set then
        set_alert_file_name(C_ALERT_FILE_NAME, ID_NEVER);
      end if;
      --show_license(VOID);
--       if C_SHOW_BITVIS_UTILITY_LIBRARY_INFO then
--         show_bitvis_utility_library_info(VOID);
--       end if;
--       if C_SHOW_BITVIS_UTILITY_LIBRARY_RELEASE_INFO then
--         show_bitvis_utility_library_release_info(VOID);
--       end if;
    end if;
  end;
  
  procedure deallocate_line_if_exists(
     variable line_to_be_deallocated      : inout line
  ) is
  begin
    if line_to_be_deallocated /= NULL then
      deallocate(line_to_be_deallocated);
    end if;
  end procedure deallocate_line_if_exists;



-- ============================================================================
-- File handling (that needs to use other utility methods)
-- ============================================================================
  procedure check_file_open_status(
    constant status      : in file_open_status;
    constant file_name   : in string
    ) is
  begin
    case status is
      when open_ok =>
        null;  --**** logmsg (if log is open for write)
      when status_error =>
        alert(tb_warning, "File: " & file_name & " is already open", "SCOPE_TBD");
      when name_error =>
        alert(tb_error, "Cannot create file: " & file_name, "SCOPE TBD");
      when mode_error =>
        alert(tb_error, "File: " & file_name & " exists, but cannot be opened in write mode", "SCOPE TBD");
    end case;
  end;

  procedure set_alert_file_name(
    constant file_name   : string := C_ALERT_FILE_NAME;
    constant msg_id      : t_msg_id  := ID_UTIL_SETUP
    ) is
    variable v_file_open_status: file_open_status;
  begin
    if not shared_alert_file_name_is_set then
      shared_alert_file_name_is_set := true;
      file_close(ALERT_FILE);
      file_open(v_file_open_status, ALERT_FILE, file_name, write_mode);
      check_file_open_status(v_file_open_status, file_name);

      if now > 0 ns then  -- Do not show note if set at the very start.
        -- NOTE: We should usually use log() instead of report. However,
        --       in this case, there is an issue with log() initialising
        --       the log file and therefore blocking subsequent set_log_file_name().
        report "alert file name set: " & file_name;
      end if;
    else
      warning("alert file name already set - or set too late");
    end if;
  end;

  procedure set_log_file_name(
    constant file_name   : string := C_LOG_FILE_NAME;
    constant msg_id      : t_msg_id  := ID_UTIL_SETUP
    ) is
    variable v_file_open_status: file_open_status;
  begin
    if not shared_log_file_name_is_set then
      shared_log_file_name_is_set := true;
      file_close(LOG_FILE);
      file_open(v_file_open_status, LOG_FILE, file_name, write_mode);
      check_file_open_status(v_file_open_status, file_name);

      if now > 0 ns then  -- Do not show note if set at the very start.
        -- NOTE: We should usually use log() instead of report. However,
        --       in this case, there is an issue with log() initialising
        --       the alert file and therefore blocking subsequent set_alert_file_name().
        report "log file name set: " & file_name;
      end if;
    else
      warning("log file name already set - or set too late");
    end if;
  end;


-- ============================================================================
-- Log-related
-- ============================================================================
  impure function align_log_time(
    value   : time
    ) return string is
    variable v_line                : line;
    variable v_value_width         : natural;
    variable v_result              : string(1 to 50); -- sufficient for any relevant time value
    variable v_result_width        : natural;
    variable v_delimeter_pos       : natural;
    variable v_time_number_width   : natural;
    variable v_time_width          : natural;
    variable v_num_initial_blanks  : integer;
    variable v_found_decimal_point : boolean;
  begin
    -- 1. Store normal write (to string) and note width
    write(v_line, value, LEFT, 0, C_LOG_TIME_BASE);  -- required as width is unknown
    v_value_width := v_line'length;
    v_result(1 to v_value_width) := v_line.all;
    deallocate(v_line);

    -- 2. Search for decimal point or space between number and unit
    v_found_decimal_point := true;  -- default
    v_delimeter_pos := pos_of_leftmost('.', v_result(1 to v_value_width), 0);
    if v_delimeter_pos = 0 then  -- No decimal point found
      v_found_decimal_point := false;
      v_delimeter_pos := pos_of_leftmost(' ', v_result(1 to v_value_width), 0);
    end if;

    -- Potentially alert if time stamp is truncated.
    if C_LOG_TIME_TRUNC_WARNING then
      if not shared_warned_time_stamp_trunc then
        if (C_LOG_TIME_DECIMALS < (v_value_width - 3 - v_delimeter_pos)) THEN
          alert(TB_WARNING, "Time stamp has been truncated to " & to_string(C_LOG_TIME_DECIMALS) &
              " decimal(s) in the next log message - settable in adaptations_pkg." &
              " (Actual time stamp has more decimals than displayed) " &
              "\nThis alert is shown once only.",
              C_BURIED_SCOPE);
          shared_warned_time_stamp_trunc := true;
        end if;
      end if;
    end if;

    -- 3. Derive Time number (integer or real)
    if C_LOG_TIME_DECIMALS = 0 then
      v_time_number_width := v_delimeter_pos - 1;
      -- v_result as is
    else  -- i.e. a decimal value is required
      if v_found_decimal_point then
        v_result(v_value_width - 2 to v_result'right) := (others => '0'); -- Zero extend
      else  -- Shift right after integer part and add point
        v_result(v_delimeter_pos + 1 to v_result'right) := v_result(v_delimeter_pos to v_result'right - 1);
        v_result(v_delimeter_pos) := '.';
        v_result(v_value_width - 1 to v_result'right) := (others => '0'); -- Zero extend
      end if;
      v_time_number_width := v_delimeter_pos + C_LOG_TIME_DECIMALS;
    end if;

    -- 4. Add time unit for full time specification
    v_time_width := v_time_number_width + 3;
    if C_LOG_TIME_BASE = ns then
      v_result(v_time_number_width + 1 to v_time_width) := " ns";
    else
      v_result(v_time_number_width + 1 to v_time_width) := " ps";
    end if;

    -- 5. Prefix
    v_num_initial_blanks := maximum(0, (C_LOG_TIME_WIDTH - v_time_width));
    if v_num_initial_blanks > 0 then
      v_result(v_num_initial_blanks + 1 to v_result'right) := v_result(1 to v_result'right - v_num_initial_blanks);
      v_result(1 to v_num_initial_blanks) := fill_string(' ', v_num_initial_blanks);
      v_result_width := C_LOG_TIME_WIDTH;
    else
      -- v_result as is
      v_result_width := v_time_width;
    end if;
    return v_result(1 to v_result_width);
  end function align_log_time;

  -- Writes Line to a file without modifying the contents of the line
  -- Not yet available in VHDL
  procedure tee (
    file     file_handle  : text;
    variable my_line      : inout line
    ) is
    variable v_line : line;
  begin
    write (v_line, my_line.all & lf);
    writeline(file_handle, v_line);
  end procedure tee;



  procedure log(
    msg_id         : t_msg_id;
    msg            : string;
    scope          : string         := C_TB_SCOPE_DEFAULT;
    msg_id_panel   : t_msg_id_panel := shared_msg_id_panel -- compatible with old code
    ) is
    variable v_msg               : line;
    variable v_msg_indent        : line;
    variable v_msg_indent_width  : natural;
    variable v_info              : line;
    variable v_info_final        : line;
    variable v_log_msg_id        : string(1 to C_LOG_MSG_ID_WIDTH);
    variable v_log_scope         : string(1 to C_LOG_SCOPE_WIDTH);
    variable v_log_pre_msg_width : natural;
  begin
    -- Check if message ID is enabled
    if (msg_id_panel(msg_id) = ENABLED) then
      pot_initialise_util(VOID);  -- Only executed the first time called

      -- Prepare strings for msg_id and scope
      v_log_msg_id := to_upper(justify(to_string(msg_id), C_LOG_MSG_ID_WIDTH, LEFT, TRUNCATE));
      if (scope = "") then
        v_log_scope  := justify("(non scoped)", C_LOG_SCOPE_WIDTH, LEFT, TRUNCATE);
      else
        v_log_scope  := justify(scope, C_LOG_SCOPE_WIDTH, LEFT, TRUNCATE);
      end if;

      -- Handle actual log info line
      -- First write all fields preceeding the actual message - in order to measure their width
      -- (Prefix is taken care of later)
      write(v_info,
          return_string_if_true(v_log_msg_id, global_show_log_id) &           -- Optional
          " " & align_log_time(now) & "  " &
          return_string_if_true(v_log_scope, global_show_log_scope) & " ");   -- Optional
      v_log_pre_msg_width := v_info'length;      -- Width of string preceeding the actual message
      -- Handle \r as potential initial open line
      if msg'length > 1 then
        if (msg(1 to 2) = "\r") then
          write(v_info_final, LF);  -- Start transcript with an empty line
          write(v_msg, remove_initial_chars(msg, 2));
        else
          write(v_msg, msg);
        end if;
      end if;

      -- Handle dedicated ID indentation.
      write(v_msg_indent, to_string(C_MSG_ID_INDENT(msg_id)));
      v_msg_indent_width := v_msg_indent'length;
      write(v_info, v_msg_indent.all);
      deallocate_line_if_exists(v_msg_indent);

      -- Then add the message it self (after replacing \n with LF
      if msg'length > 1 then
        write(v_info, replace_backslash_n_with_lf(v_msg.all));
      end if;
      deallocate_line_if_exists(v_msg);

      if not C_SINGLE_LINE_LOG then
        -- Modify and align info-string if additional lines are required (after wrapping lines)
        wrap_lines(v_info, 1, v_log_pre_msg_width + v_msg_indent_width + 1, C_LOG_LINE_WIDTH-C_LOG_PREFIX_WIDTH);
      else
      -- Remove line feed character if
      -- single line log/alert enabled
        replace(v_info, LF, ' ');
      end if;

      -- Handle potential log header by including info-lines inside the log header format and update of waveview header.
      if (msg_id = ID_LOG_HDR) then
        write(v_info_final, LF & LF);
        -- also update the Log header string
        shared_current_log_hdr.normal   := justify(msg, C_LOG_HDR_FOR_WAVEVIEW_WIDTH, LEFT, TRUNCATE);
        shared_log_hdr_for_waveview     := justify(msg, C_LOG_HDR_FOR_WAVEVIEW_WIDTH, LEFT, TRUNCATE);
      elsif (msg_id = ID_LOG_HDR_LARGE) then
        write(v_info_final, LF & LF);
        shared_current_log_hdr.large    := justify(msg, C_LOG_HDR_FOR_WAVEVIEW_WIDTH, LEFT, TRUNCATE);
        write(v_info_final, fill_string('=', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)) & LF);
      elsif (msg_id = ID_LOG_HDR_XL) then
        write(v_info_final, LF & LF);
        shared_current_log_hdr.xl       := justify(msg, C_LOG_HDR_FOR_WAVEVIEW_WIDTH, LEFT, TRUNCATE);
        write(v_info_final, LF & fill_string('#', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH))& LF & LF);
      end if;

      write(v_info_final, v_info.all);  -- include actual info
      deallocate_line_if_exists(v_info);
      -- Handle rest of potential log header
      if (msg_id = ID_LOG_HDR) then
        write(v_info_final, LF & fill_string('-', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)));
      elsif (msg_id = ID_LOG_HDR_LARGE) then
        write(v_info_final, LF & fill_string('=', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)));
      elsif (msg_id = ID_LOG_HDR_XL) then
        write(v_info_final, LF & LF & fill_string('#', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)) & LF & LF);
      end if;

      -- Add prefix to all lines
      prefix_lines(v_info_final);

      -- Write the info string to the target file
      tee(OUTPUT, v_info_final);            -- write to transcript, while keeping the line contents
      writeline(LOG_FILE, v_info_final);
    end if;
  end;


  -- Logging for multi line text
  procedure log_text_block(
    msg_id                : t_msg_id;
    variable text_block   : inout line;
    formatting            : t_log_format;  -- FORMATTED or UNFORMATTED
    msg_header            : string         := "";
    log_if_block_empty    : t_log_if_block_empty := WRITE_HDR_IF_BLOCK_EMPTY;
    scope                 : string         := C_TB_SCOPE_DEFAULT;
    msg_id_panel          : t_msg_id_panel := shared_msg_id_panel
  ) is
    variable v_text_block_empty_note : string(1 to 26) := "Note: Text block was empty";
    variable v_header_line          : line;
    variable v_log_body             : line;
    variable v_text_block_is_empty  : boolean;
  begin
    -- Check if message ID is enabled
    if (msg_id_panel(msg_id) = ENABLED) then
      pot_initialise_util(VOID);            -- Only executed the first time called
      
      v_text_block_is_empty := (text_block = NULL);
      
      if(formatting = UNFORMATTED) then
        if(not v_text_block_is_empty) then
           -- Write the info string to the target file without any header, footer or indentation
          tee(OUTPUT, text_block);                   -- write to transcript, while keeping the line contents
          writeline(LOG_FILE, text_block);
        end if;
      elsif not (v_text_block_is_empty and (log_if_block_empty = SKIP_LOG_IF_BLOCK_EMPTY)) then
        
        -- Add and print header
        write(v_header_line, LF & LF & fill_string('*', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)));
        prefix_lines(v_header_line);
        tee(OUTPUT, v_header_line);         -- write to transcript, while keeping the line contents
        writeline(LOG_FILE, v_header_line);

        -- Print header using log function
        log(msg_id, msg_header, scope, msg_id_panel);

        -- Print header underline, body and footer
        write(v_log_body, fill_string('-', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)) & LF);
        if v_text_block_is_empty then
          if log_if_block_empty = NOTIFY_IF_BLOCK_EMPTY then
            write(v_log_body, v_text_block_empty_note); -- Notify that the text block was empty
          end if;
        else 
          write(v_log_body, text_block.all);         -- include input text
        end if;
        write(v_log_body, LF & fill_string('*', (C_LOG_LINE_WIDTH - C_LOG_PREFIX_WIDTH)) & LF);
        prefix_lines(v_log_body);
        tee(OUTPUT, v_log_body);            -- write to transcript, while keeping the line contents
        writeline(LOG_FILE, v_log_body);
      end if;
    end if;
  end;

  procedure enable_log_msg(
    constant msg_id         : t_msg_id;
    variable msg_id_panel   : inout t_msg_id_panel;
    constant msg            : string := "";
    constant scope          : string          := C_TB_SCOPE_DEFAULT
    ) is
  begin
    case msg_id is
      when ID_NEVER =>
        null;  -- Shall not be possible to enable
        log(ID_LOG_MSG_CTRL, "enable_log_msg() ignored for " & to_string(msg_id) & ". (Not allowed)" & msg, scope);
      when ALL_MESSAGES =>
        for i in t_msg_id'left to t_msg_id'right loop
          msg_id_panel(i)        := ENABLED;
        end loop;
        msg_id_panel(ID_NEVER) := DISABLED;
        log(ID_LOG_MSG_CTRL, "enable_log_msg(" & to_string(msg_id) & "). " & msg, scope);
      when others =>
        msg_id_panel(msg_id) := ENABLED;
        log(ID_LOG_MSG_CTRL, "enable_log_msg(" & to_string(msg_id) & "). " & msg, scope);
    end case;
  end;

  procedure enable_log_msg(
    msg_id         : t_msg_id;
    msg            : string  := ""
    ) is
  begin
    enable_log_msg(msg_id, shared_msg_id_panel, msg);
  end;

  procedure disable_log_msg(
    constant msg_id         : t_msg_id;
    variable msg_id_panel   : inout t_msg_id_panel;
    constant msg            : string      := "";
    constant scope          : string      := C_TB_SCOPE_DEFAULT;
    constant quietness      : t_quietness := NON_QUIET
    ) is
  begin
    case msg_id is
      when ALL_MESSAGES =>
        if quietness = NON_QUIET then
          log(ID_LOG_MSG_CTRL, "disable_log_msg(" & to_string(msg_id) & "). " & msg, scope);
        end if;
        for i in t_msg_id'left to t_msg_id'right loop
          msg_id_panel(i) := DISABLED;
        end loop;
      when others =>
        msg_id_panel(msg_id) := DISABLED;
        if quietness = NON_QUIET then
          log(ID_LOG_MSG_CTRL, "disable_log_msg(" & to_string(msg_id) & "). " & msg, scope);
        end if;
    end case;
  end;

  procedure disable_log_msg(
    msg_id         : t_msg_id;
    msg            : string  := "";
    quietness      : t_quietness := NON_QUIET
    ) is
  begin
    disable_log_msg(msg_id, shared_msg_id_panel, msg, C_TB_SCOPE_DEFAULT, quietness);
  end;

  impure function is_log_msg_enabled(
    msg_id        : t_msg_id;
    msg_id_panel  : t_msg_id_panel :=  shared_msg_id_panel
    ) return boolean is
  begin
    if msg_id_panel(msg_id) = ENABLED then
      return true;
    else
      return false;
    end if;
  end;




-- ============================================================================
-- Alert-related
-- ============================================================================
  procedure alert(
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT
    ) is
    variable v_msg       : line; -- msg after pot. replacement of \n
    variable v_info      : line;
   begin
      pot_initialise_util(VOID);  -- Only executed the first time called

      write(v_msg, replace_backslash_n_with_lf(msg));

     -- 1. Increase relevant alert counter. Exit if ignore is set for this alert type.
     if get_alert_attention(alert_level) = IGNORE then
--       protected_alert_counters.increment(alert_level, IGNORE);
       increment_alert_counter(alert_level, IGNORE);
     else
       --protected_alert_counters.increment(alert_level, REGARD);
       increment_alert_counter(alert_level, REGARD);

       -- 2. Write first part of alert message
       --    Serious alerts need more attention - thus more space and lines
       if (alert_level > MANUAL_CHECK) then
         write(v_info, LF & fill_string('=', C_LOG_INFO_WIDTH));
       end if;

       write(v_info, LF & "***  ");

       -- 3. Remove line feed character (LF)
       --    if single line alert enabled.
       if not C_SINGLE_LINE_ALERT then
         write(v_info, to_upper(to_string(alert_level)) & " #" & to_string(get_alert_counter(alert_level)) & "  ***" & LF &
           justify( to_string(now, C_LOG_TIME_BASE), C_LOG_TIME_WIDTH, RIGHT) & "   " & scope & LF &
           wrap_lines(v_msg.all, C_LOG_TIME_WIDTH + 4, C_LOG_TIME_WIDTH + 4, C_LOG_INFO_WIDTH));
       else
         replace(v_msg, LF, ' ');
         write(v_info, to_upper(to_string(alert_level)) & " #" & to_string(get_alert_counter(alert_level)) & "  ***" &
           justify( to_string(now, C_LOG_TIME_BASE), C_LOG_TIME_WIDTH, RIGHT) & "   " & scope &
           "        " & v_msg.all);
       end if;
       deallocate_line_if_exists(v_msg);

       -- 4. Write stop message if stop-limit is reached for number of this alert
       if (get_alert_stop_limit(alert_level) /= 0) and
          (get_alert_counter(alert_level) >= get_alert_stop_limit(alert_level)) then
         write(v_info, LF & LF & "Simulator has been paused as requested after " &
             to_string(get_alert_counter(alert_level)) & " " &
             to_string(alert_level) & LF);
         if (alert_level = MANUAL_CHECK) then
           write(v_info, "Carry out above check." & LF &
               "Then continue simulation from within simulator." & LF);
         else
           write(v_info, string'("*** To find the root cause of this alert, " &
               "step out the HDL calling stack in your simulator. ***" & LF &
               "*** For example, step out until you reach the call from the test sequencer. ***"));
         end if;
       end if;

       -- 5. Write last part of alert message
       if (alert_level > MANUAL_CHECK) then
         write(v_info, LF & fill_string('=', C_LOG_INFO_WIDTH) & LF & LF);
       else
         write(v_info, LF);
       end if;

       prefix_lines(v_info);
       tee(OUTPUT, v_info);
       tee(ALERT_FILE, v_info);
       writeline(LOG_FILE, v_info);

       -- 6. Stop simulation if stop-limit is reached for number of this alert
       if (get_alert_stop_limit(alert_level) /= 0) then
         if (get_alert_counter(alert_level) >= get_alert_stop_limit(alert_level)) then
           assert false
             report "This single Failure line has been provoked to stop the simulation. See alert-message above"
             severity failure;
         end if;
       end if;
     end if;
   end;

  -- Dedicated alert-procedures all alert levels (less verbose - as 2 rather than 3 parameters...)
  procedure note(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(note, msg, scope);
  end;

  procedure tb_note(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(tb_note, msg, scope);
  end;

  procedure warning(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(warning, msg, scope);
  end;

  procedure tb_warning(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(tb_warning, msg, scope);
  end;

  procedure manual_check(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(manual_check, msg, scope);
  end;

  procedure error(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(error, msg, scope);
  end;

  procedure tb_error(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(tb_error, msg, scope);
  end;

  procedure failure(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(failure, msg, scope);
  end;

  procedure tb_failure(
    constant msg               : string;
    constant scope             : string  := C_TB_SCOPE_DEFAULT
    ) is
  begin
    alert(tb_failure, msg, scope);
  end;

  procedure increment_expected_alerts(
    constant alert_level  : t_alert_level;
    constant number       : natural := 1;
    constant msg          : string  := "";
    constant scope        : string  := C_TB_SCOPE_DEFAULT
  ) is
  begin
    increment_alert_counter(alert_level, EXPECT, number);
    log(ID_UTIL_SETUP, "incremented expected " & to_string(alert_level) & "s by " & to_string(number) & ". " & msg, scope);
  end;

  -- Arguments:
  -- - order = FINAL : print out Simulation Success/Fail
  procedure report_alert_counters(
    constant order  : in t_order
  ) is
  begin
    work.vhdl_version_layer_pkg.report_alert_counters(order);
    pot_initialise_util(VOID);  -- Only executed the first time called
  end;

  -- This version (with the t_void argument) is kept for backwards compatibility
  procedure report_alert_counters(
    constant dummy  : in t_void
  ) is
  begin
    work.vhdl_version_layer_pkg.report_alert_counters(FINAL); -- Default when calling this old method is order=FINAL
    pot_initialise_util(VOID);  -- Only executed the first time called
  end;

  procedure report_global_ctrl(
    constant dummy  : in t_void
  ) is
    constant prefix          : string := C_LOG_PREFIX & "     ";
    variable v_line          : line;
  begin
    pot_initialise_util(VOID);  -- Only executed the first time called
    write(v_line,
        LF &
        fill_string('-', (C_LOG_LINE_WIDTH - prefix'length)) & LF &
        "***  REPORT OF GLOBAL CTRL ***" & LF &
        fill_string('-', (C_LOG_LINE_WIDTH - prefix'length)) & LF &
        "                          IGNORE    STOP_LIMIT                      " & LF);
    for i in t_alert_level'left to t_alert_level'right loop
      write(v_line, "          " & to_upper(to_string(i, 13, LEFT)) & ": ");          -- Severity

      write(v_line, to_string(get_alert_attention(i),      7, RIGHT) & "    ");       -- column 1
      write(v_line, to_string(integer'(get_alert_stop_limit(i)), 6, RIGHT) & "    " & LF);  -- column 2
    end loop;
    write(v_line, fill_string('-', (C_LOG_LINE_WIDTH - prefix'length)) & LF);

    wrap_lines(v_line, 1, 1, C_LOG_LINE_WIDTH-prefix'length);
    prefix_lines(v_line, prefix);

    -- Write the info string to the target file
    tee(OUTPUT, v_line);
    writeline(LOG_FILE, v_line);

  end;

  procedure report_msg_id_panel(
    constant dummy  : in t_void
  ) is
    constant prefix          : string := C_LOG_PREFIX & "     ";
    variable v_line          : line;
  begin
      write(v_line,
          LF &
          fill_string('-', (C_LOG_LINE_WIDTH - prefix'length)) & LF &
          "***  REPORT OF MSG ID PANEL ***" & LF &
          fill_string('-', (C_LOG_LINE_WIDTH - prefix'length)) & LF &
          "          " & justify("ID", C_LOG_MSG_ID_WIDTH, LEFT) & "       Status" &  LF &
          "          " & fill_string('-', C_LOG_MSG_ID_WIDTH)    & "       ------" & LF);
      for i in t_msg_id'left to t_msg_id'right loop
        if (i /= ID_NEVER) then  -- report all but ID_NEVER
        write(v_line, "          " & to_upper(to_string(i, C_LOG_MSG_ID_WIDTH+5, LEFT)) & ": ");  -- MSG_ID
        write(v_line,to_string(shared_msg_id_panel(i)) & "    " & LF); -- Enabled/disabled
        end if;
      end loop;
      write(v_line, fill_string('-', (C_LOG_LINE_WIDTH - prefix'length)) & LF);

      wrap_lines(v_line, 1, 1, C_LOG_LINE_WIDTH-prefix'length);
      prefix_lines(v_line, prefix);

      -- Write the info string to the target file
      tee(OUTPUT, v_line);
      writeline(LOG_FILE, v_line);

  end;

  procedure set_alert_attention(
      alert_level : t_alert_level;
      attention   : t_attention;
      msg         : string  := ""
  ) is
  begin
    check_value(attention = IGNORE or attention = REGARD, TB_WARNING,
        "set_alert_attention only supported for IGNORE and REGARD", C_BURIED_SCOPE, ID_NEVER);
    shared_alert_attention(alert_level) := attention;
    log(ID_ALERT_CTRL, "set_alert_attention(" & to_string(alert_level) & ", " & to_string(attention) & "). " & msg);

  end;

  impure function get_alert_attention(
      alert_level : t_alert_level
  ) return t_attention is
  begin
    return shared_alert_attention(alert_level);
  end;

  procedure set_alert_stop_limit(
      alert_level : t_alert_level;
      value       : natural
  ) is
  begin
    shared_stop_limit(alert_level) := value;

    -- Evaluate new stop limit in case it is less than or equal to the current alert counter for this alert level
    -- If that is the case, a new alert with the same alert level shall be triggered.
    if (get_alert_stop_limit(alert_level) /= 0) and
          (get_alert_counter(alert_level) >= get_alert_stop_limit(alert_level)) then
      alert(alert_level, "Alert stop limit for " & to_string(alert_level) &  " set to " & to_string(value) &
            ", which is lower than the current " & to_string(alert_level) & " count (" & to_string(get_alert_counter(alert_level)) & ").");
    end if;
  end;

  impure function get_alert_stop_limit(
      alert_level : t_alert_level
  ) return natural is
  begin
    return shared_stop_limit(alert_level);
  end;

-- ============================================================================
-- Deprecation message
-- ============================================================================

  procedure deprecate(
    caller_name  : string;
    constant msg : string  := ""
  ) is
    variable v_found : boolean;
  begin
    v_found := false;
    if C_DEPRECATE_SETTING /= NO_DEPRECATE then -- only perform if deprecation enabled
      l_find_caller_name_in_list:
      for i in deprecated_subprogram_list'range loop
        if deprecated_subprogram_list(i) = justify(caller_name, 100) then
          v_found := true;
          exit l_find_caller_name_in_list;
        end if;
      end loop;

      if v_found then
        -- Has already been printed.
        if C_DEPRECATE_SETTING = ALWAYS_DEPRECATE then
          log(ID_SEQUENCER, "Sub-program " & caller_name & " is outdated and has been replaced by another sub-program." & LF & msg);
        else -- C_DEPRECATE_SETTING = DEPRECATE_ONCE
          null;
        end if;
      else
        -- Has not been printed yet.
        l_insert_caller_name_in_first_available:
        for i in deprecated_subprogram_list'range loop
          if deprecated_subprogram_list(i) = justify("", 100) then
            deprecated_subprogram_list(i) := justify(caller_name, 100);
            exit l_insert_caller_name_in_first_available;
          end if;
        end loop;

        log(ID_SEQUENCER, "Sub-program " & caller_name & " is outdated and has been replaced by another sub-program." & LF & msg);
      end if;
    end if;
  end;

-- ============================================================================
-- Non time consuming checks
-- ============================================================================

  -- NOTE: Index in range N downto 0, with -1 meaning not found
  function idx_leftmost_p1_in_p2(
    target              : std_logic;
    vector              : std_logic_vector
    ) return integer is
    alias a_vector : std_logic_vector(vector'length - 1 downto 0) is vector;
    constant result_if_not_found : integer := -1;   -- To indicate not found
  begin
    bitvis_assert(vector'length > 0, ERROR, "idx_leftmost_p1_in_p2()", "String input is empty");
    for i in a_vector'left downto a_vector'right loop
      if (a_vector(i) = target) then
        return i;
      end if;
    end loop;
    return result_if_not_found;
  end;

  -- Matching if same width or only zeros in "extended width"
  function matching_widths(
    value1: std_logic_vector;
    value2: std_logic_vector
    ) return boolean is
    -- Normalize vectors to (N downto 0)
    alias    a_value1: std_logic_vector(value1'length - 1 downto 0) is value1;
    alias    a_value2: std_logic_vector(value2'length - 1 downto 0) is value2;

  begin
    if (a_value1'left >= maximum( idx_leftmost_p1_in_p2('1', a_value2), 0)) and
       (a_value2'left >= maximum( idx_leftmost_p1_in_p2('1', a_value1), 0)) then
      return true;
    else
      return false;
    end if;
  end;

  function matching_widths(
    value1: unsigned;
    value2: unsigned
    ) return boolean is
  begin
    return matching_widths(std_logic_vector(value1), std_logic_vector(value2));
  end;

  function matching_widths(
    value1: signed;
    value2: signed
    ) return boolean is
  begin
    return matching_widths(std_logic_vector(value1), std_logic_vector(value2));
  end;


  -- Compare values, but ignore any leading zero's at higher indexes than v_min_length-1.
  function matching_values(
    value1: std_logic_vector;
    value2: std_logic_vector
    ) return boolean is
    -- Normalize vectors to (N downto 0)
    alias    a_value1  : std_logic_vector(value1'length - 1 downto 0) is value1;
    alias    a_value2  : std_logic_vector(value2'length - 1 downto 0) is value2;
    variable v_min_length : natural := minimum(a_value1'length, a_value2'length);
    variable v_match      : boolean := true;  -- as default prior to checking
  begin
    if matching_widths(a_value1, a_value2) then
      if not std_match( a_value1(v_min_length-1 downto 0), a_value2(v_min_length-1 downto 0) ) then
          v_match := false;
      end if;
    else
      v_match := false;
    end if;
    return v_match;
  end;

  function matching_values(
    value1: unsigned;
    value2: unsigned
    ) return boolean is
  begin
    return matching_values(std_logic_vector(value1),std_logic_vector(value2));
  end;

  function matching_values(
    value1: signed;
    value2: signed
    ) return boolean is
  begin
    return matching_values(std_logic_vector(value1),std_logic_vector(value2));
  end;

  -- Function check_value,
  -- returning 'true' if OK
  impure function check_value(
    constant value       : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean is
  begin
    if value then
      log(msg_id, name & " => OK, for boolean true. " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Boolean was false. " & msg, scope);
    end if;
    return value;
  end;

  impure function check_value(
    constant value       : boolean;
    constant exp         : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean is
    constant v_value_str : string := to_string(value);
    constant v_exp_str   : string := to_string(exp);
  begin
    if value = exp then
      log(msg_id, name & " => OK, for boolean " & v_value_str & ". " & msg, scope, msg_id_panel);
      return true;
    else
      alert(alert_level, name & " => Failed. Boolean was " & v_value_str & ". Expected " & v_exp_str & ". " & LF & msg, scope);
      return false;
    end if;
  end;

  impure function check_value(
    constant value       : std_logic;
    constant exp         : std_logic;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean is
    constant value_type  : string          := "std_logic";
    constant v_value_str : string := to_string(value);
    constant v_exp_str   : string := to_string(exp);
  begin
    if std_match(value, exp) then
      if value = exp then
        log(msg_id, name & " => OK, for " & value_type & " '" & v_value_str & "'. " & msg, scope, msg_id_panel);
      else
        log(msg_id, name & " => OK, for " & value_type & " '" & v_value_str & "' (exp: '" & v_exp_str & "'). " & msg, scope, msg_id_panel);
      end if;
      return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was '"  & v_value_str & "'. Expected '" & v_exp_str & "'" & LF & msg, scope);
      return false;
    end if;
  end;

  impure function check_value(
    constant value       : std_logic_vector;
    constant exp         : std_logic_vector;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "slv"
    ) return boolean is
    -- Normalise vectors to (N downto 0)
    alias    a_value     : std_logic_vector(value'length - 1 downto 0) is value;
    alias    a_exp       : std_logic_vector(exp'length - 1 downto 0) is exp;
    constant v_value_str : string := to_string(a_value, radix, format);
    constant v_exp_str   : string := to_string(a_exp, radix, format);
    variable v_check_ok  : boolean := true;  -- as default prior to checking
  begin
    v_check_ok := matching_values(a_value, a_exp);

    if v_check_ok then
      if v_value_str = v_exp_str then
        log(msg_id, name & " => OK, for " & value_type & " x'" & v_value_str & "'. " & msg, scope, msg_id_panel);
      else
        -- H,L or - is present in v_exp_str
        log(msg_id, name & " => OK, for " & value_type & " x'" & v_value_str & "' (exp: x'" & v_exp_str & "'). " & msg,
            scope, msg_id_panel);
      end if;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was x'"  & v_value_str & "'. Expected x'" & v_exp_str & "'" & LF & msg, scope);
    end if;

    return v_check_ok;
  end;

  impure function check_value(
    constant value       : unsigned;
    constant exp         : unsigned;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "unsigned"
    ) return boolean is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(std_logic_vector(value), std_logic_vector(exp), alert_level, msg, scope,
                              radix, format, msg_id, msg_id_panel, name, value_type);
    return v_check_ok;
  end;

  impure function check_value(
    constant value       : signed;
    constant exp         : signed;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "signed"
    ) return boolean is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(std_logic_vector(value), std_logic_vector(exp), alert_level, msg, scope,
                              radix, format, msg_id, msg_id_panel, name, value_type);
    return v_check_ok;
  end;

  impure function check_value(
    constant value       : integer;
    constant exp         : integer;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean is
    constant value_type  : string          := "int";
    constant v_value_str : string := to_string(value);
    constant v_exp_str   : string := to_string(exp);
  begin
    if value = exp then
      log(msg_id, name & " => OK, for " & value_type & " " & v_value_str & ". " & msg, scope, msg_id_panel);
      return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was "  & v_value_str & ". Expected " & v_exp_str & LF & msg, scope);
      return false;
    end if;
  end;

  impure function check_value(
    constant value       : time;
    constant exp         : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean is
    constant value_type  : string          := "time";
    constant v_value_str : string := to_string(value);
    constant v_exp_str   : string := to_string(exp);
  begin
    if value = exp then
        log(msg_id, name & " => OK, for " & value_type & " " & v_value_str & ". " & msg, scope, msg_id_panel);
        return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was "  & v_value_str & ". Expected " & v_exp_str & LF & msg, scope);
      return false;
    end if;
  end;

  impure function check_value(
    constant value       : string;
    constant exp         : string;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) return boolean is
    constant value_type  : string          := "string";
  begin
    if value = exp then
        log(msg_id, name & " => OK, for " & value_type & " '" & value & "'. " & msg, scope, msg_id_panel);
        return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was '"  & value & "'. Expected '" & exp & "'" & LF & msg, scope);
      return false;
    end if;
  end;

  ----------------------------------------------------------------------
  -- Overloads for check_value functions,
  -- to allow for no return value
  ----------------------------------------------------------------------
  procedure check_value(
    constant value     : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
      ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value(
    constant value       : boolean;
    constant exp         : boolean;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value(
    constant value       : std_logic;
    constant exp         : std_logic;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value(
    constant value       : std_logic_vector;
    constant exp         : std_logic_vector;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "slv"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, radix, format, msg_id, msg_id_panel, name, value_type);
  end;

  procedure check_value(
    constant value       : unsigned;
    constant exp         : unsigned;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "unsigned"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, radix, format, msg_id, msg_id_panel, name, value_type);
  end;

  procedure check_value(
    constant value       : signed;
    constant exp         : signed;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant radix       : t_radix         := HEX_BIN_IF_INVALID;
    constant format      : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()";
    constant value_type  : string          := "signed"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, radix, format, msg_id, msg_id_panel, name, value_type);
  end;

  procedure check_value(
    constant value       : integer;
    constant exp         : integer;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value(
    constant value       : time;
    constant exp         : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value(
    constant value       : string;
    constant exp         : string;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value(value, exp, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  ------------------------------------------------------------------------
  -- check_value_in_range
  ------------------------------------------------------------------------
  impure function check_value_in_range (
    constant value        : integer;
    constant min_value    : integer;
    constant max_value    : integer;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()";
    constant value_type   : string         := "integer"
    ) return boolean is
    constant v_value_str     : string   := to_string(value);
    constant v_min_value_str : string   := to_string(min_value);
    constant v_max_value_str : string   := to_string(max_value);
    variable v_check_ok      : boolean;
  begin
    -- Sanity check
    check_value(max_value >= min_value, TB_ERROR, scope,
      " => min_value (" & v_min_value_str & ") must be less than max_value("& v_max_value_str & ")" & LF & msg, ID_NEVER, msg_id_panel, name);

    if (value >= min_value and value <= max_value) then
        log(msg_id, name & " => OK, for " & value_type & " " & v_value_str & ". " & msg, scope, msg_id_panel);
        return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was "  & v_value_str & ". Expected between " & v_min_value_str & " and " & v_max_value_str & LF & msg, scope);
      return false;
    end if;
  end;

  impure function check_value_in_range (
    constant value        : unsigned;
    constant min_value    : unsigned;
    constant max_value    : unsigned;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()";
    constant value_type   : string         := "unsigned"
    ) return boolean is
  begin
    return check_value_in_range(to_integer(value), to_integer(min_value), to_integer(max_value), alert_level, msg, scope, msg_id, msg_id_panel, name, value_type);
  end;

  impure function check_value_in_range (
    constant value        : signed;
    constant min_value    : signed;
    constant max_value    : signed;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()";
    constant value_type   : string         := "signed"
    ) return boolean is
  begin
    return check_value_in_range(to_integer(value), to_integer(min_value), to_integer(max_value), alert_level, msg, scope, msg_id, msg_id_panel, name, value_type);
  end;

  impure function check_value_in_range (
    constant value        : time;
    constant min_value    : time;
    constant max_value    : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()"
    ) return boolean is
    constant value_type      : string   := "time";
    constant v_value_str     : string   := to_string(value);
    constant v_min_value_str : string   := to_string(min_value);
    constant v_max_value_str : string   := to_string(max_value);
    variable v_check_ok      : boolean;
  begin
    -- Sanity check
    check_value(max_value >= min_value, TB_ERROR, scope,
      " => min_value (" & v_min_value_str & ") must be less than max_value("& v_max_value_str & ")" & LF & msg, ID_NEVER, msg_id_panel, name);

    if (value >= min_value and value <= max_value) then
        log(msg_id, name & " => OK, for " & value_type & " " & v_value_str & ". " & msg, scope, msg_id_panel);
        return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was "  & v_value_str & ". Expected between " & v_min_value_str & " and " & v_max_value_str & LF & msg, scope);
      return false;
    end if;
  end;

  impure function check_value_in_range (
    constant value        : real;
    constant min_value    : real;
    constant max_value    : real;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel := shared_msg_id_panel;
    constant name         : string         := "check_value_in_range()"
    ) return boolean is
    constant value_type      : string   := "real";
    constant v_value_str     : string   := to_string(value);
    constant v_min_value_str : string   := to_string(min_value);
    constant v_max_value_str : string   := to_string(max_value);
    variable v_check_ok      : boolean;
  begin
    -- Sanity check
    check_value(max_value >= min_value, TB_ERROR,
      " => min_value (" & v_min_value_str & ") must be less than max_value("& v_max_value_str & ")" & LF & msg, scope,
      ID_NEVER, msg_id_panel, name);

    if (value >= min_value and value <= max_value) then
        log(msg_id, name & " => OK, for " & value_type & " " & v_value_str & ". " & msg, scope, msg_id_panel);
        return true;
    else
      alert(alert_level, name & " => Failed. " & value_type & "  Was "  & v_value_str & ". Expected between " & v_min_value_str & " and " & v_max_value_str & LF & msg, scope);
      return false;
    end if;
  end;
  --------------------------------------------------------------------------------
  -- check_value_in_range procedures :
  -- Call the corresponding function and discard the return value
  --------------------------------------------------------------------------------
  procedure check_value_in_range (
    constant value       : integer;
    constant min_value   : integer;
    constant max_value   : integer;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value_in_range(value, min_value, max_value, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;
  procedure check_value_in_range (
    constant value       : unsigned;
    constant min_value   : unsigned;
    constant max_value   : unsigned;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value_in_range(value, min_value, max_value, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;
  procedure check_value_in_range (
    constant value       : signed;
    constant min_value   : signed;
    constant max_value   : signed;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value_in_range(value, min_value, max_value, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value_in_range (
    constant value       : time;
    constant min_value   : time;
    constant max_value   : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value_in_range(value, min_value, max_value, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  procedure check_value_in_range (
    constant value       : real;
    constant min_value   : real;
    constant max_value   : real;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_value_in_range()"
    ) is
    variable v_check_ok  : boolean;
  begin
    v_check_ok := check_value_in_range(value, min_value, max_value, alert_level, msg, scope, msg_id, msg_id_panel, name);
  end;

  --------------------------------------------------------------------------------
  -- check_stable
  --------------------------------------------------------------------------------
  procedure check_stable(
    signal   target      : boolean;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "boolean"
    ) is
    constant value_string       : string := to_string(target);
    constant last_value_string  : string := to_string(target'last_value);
    constant last_change        : time   := target'last_event;
    constant last_change_string : string := to_string(last_change, ns);
  begin
    if (last_change >= stable_req) then
      log(msg_id, name & " => OK. Stable at " & value_string & ". " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Switched from " & last_value_string & " to " &
            value_string & " " & last_change_string & " ago. Expected stable for " & to_string(stable_req) & LF & msg, scope);
    end if;
  end;

  procedure check_stable(
    signal   target      : std_logic_vector;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "slv"
    ) is
    constant value_string       : string := 'x' & to_string(target, HEX);
    constant last_value_string  : string := 'x' & to_string(target'last_value, HEX);
    constant last_change        : time   := target'last_event;
    constant last_change_string : string := to_string(last_change, ns);
  begin
    if (last_change >= stable_req) then
      log(msg_id, name & " => OK. Stable at " & value_string & ". " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Switched from " & last_value_string & " to " &
            value_string & " " & last_change_string & " ago. Expected stable for " & to_string(stable_req) & LF & msg, scope);
    end if;
  end;

  procedure check_stable(
    signal   target      : unsigned;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "unsigned"
    ) is
    constant value_string       : string := 'x' & to_string(target, HEX);
    constant last_value_string  : string := 'x' & to_string(target'last_value, HEX);
    constant last_change        : time   := target'last_event;
    constant last_change_string : string := to_string(last_change, ns);
  begin
    if (last_change >= stable_req) then
      log(msg_id, name & " => OK. Stable at " & value_string & ". " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Switched from " & last_value_string & " to " &
            value_string & " " & last_change_string & " ago. Expected stable for " & to_string(stable_req) & LF & msg, scope);
    end if;
  end;

  procedure check_stable(
    signal   target      : signed;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "signed"
    ) is
    constant value_string       : string := 'x' & to_string(target, HEX);
    constant last_value_string  : string := 'x' & to_string(target'last_value, HEX);
    constant last_change        : time   := target'last_event;
    constant last_change_string : string := to_string(last_change, ns);
  begin
    if (last_change >= stable_req) then
      log(msg_id, name & " => OK. Stable at " & value_string & ". " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Switched from " & last_value_string & " to " &
            value_string & " " & last_change_string & " ago. Expected stable for " & to_string(stable_req) & LF & msg, scope);
    end if;
  end;

  procedure check_stable(
    signal   target      : std_logic;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "std_logic"
    ) is
    constant value_string       : string := to_string(target);
    constant last_value_string  : string := to_string(target'last_value);
    constant last_change        : time   := target'last_event;
    constant last_change_string : string := to_string(last_change, ns);
  begin
    if (last_change >= stable_req) then
      log(msg_id, name & " => OK. Stable at " & value_string & ". " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Switched from " & last_value_string & " to " &
            value_string & " " & last_change_string & " ago. Expected stable for " & to_string(stable_req) & LF & msg, scope);
    end if;
  end;

  procedure check_stable(
    signal   target      : integer;
    constant stable_req  : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel  := shared_msg_id_panel;
    constant name        : string          := "check_stable()";
    constant value_type  : string          := "integer"
    ) is
    constant value_string       : string := to_string(target);
    constant last_value_string  : string := to_string(target'last_value);
    constant last_change        : time   := target'last_event;
    constant last_change_string : string := to_string(last_change, ns);
  begin
    if (last_change >= stable_req) then
      log(msg_id, name & " => OK." & value_string & " stable at " & value_string & ". " & msg, scope, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Switched from " & last_value_string & " to " &
            value_string & " " & last_change_string & " ago. Expected stable for " & to_string(stable_req) & LF & msg, scope);
    end if;
  end;


  -- check_time_window is used to check if a given condition occurred between
  -- min_time and max_time
  -- Usage: wait for requested condition until max_time is reached, then call check_time_window().
  -- The input 'success' is needed to distinguish between the following cases:
  --      - the signal reached success condition at max_time,
  --      - max_time was reached with no success condition
  procedure check_time_window(
    constant success          : boolean; -- F.ex target'event, or target=exp
    constant elapsed_time     : time;
    constant min_time         : time;
    constant max_time         : time;
    constant alert_level      : t_alert_level;
    constant name             : string;
    constant msg              : string;
    constant scope            : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel := shared_msg_id_panel
    ) is
  begin
    -- Sanity check
    check_value(max_time >= min_time, TB_ERROR, name & " => min_time must be less than max_time." & LF & msg, scope, ID_NEVER, msg_id_panel, name);

    if elapsed_time < min_time then
      alert(alert_level, name & " => Failed. Condition occurred too early, after " &
          to_string(elapsed_time, C_LOG_TIME_BASE) & ". " & msg, scope);
    elsif success then
      log(msg_id, name & " => OK. Condition occurred after " &
          to_string(elapsed_time, C_LOG_TIME_BASE) & ". " & msg, scope, msg_id_panel);
    else -- max_time reached with no success
      alert(alert_level, name & " => Failed. Timed out after " &
          to_string(max_time, C_LOG_TIME_BASE) & ". " & msg, scope);
    end if;
  end;

  ----------------------------------------------------------------------------
  -- Random functions
  ----------------------------------------------------------------------------
  -- Return a random std_logic_vector, using overload for the integer version of random()
  impure function random (
    constant length : integer
  )  return std_logic_vector is
    variable random_vec : std_logic_vector(length-1 downto 0);
  begin
    -- Iterate through each bit and randomly set to 0 or 1
    for i in 0 to length-1 loop
      random_vec(i downto i) := std_logic_vector(to_unsigned(random(0,1), 1));
    end loop;
    return random_vec;
  end;

  -- Return a random std_logic, using overload for the SLV version of random()
  impure function random (
    constant VOID : t_void
  ) return std_logic is
    variable v_random_bit : std_logic_vector(0 downto 0);
  begin
    -- randomly set bit to 0 or 1
    v_random_bit := random(1);
    return v_random_bit(0);
  end;

  -- Return a random integer between min_value and max_value
  -- Use global seeds
  impure function random (
    constant min_value : integer;
    constant max_value : integer
  ) return integer is
    variable v_rand_scaled : integer;
    variable v_seed1       : positive := shared_seed1;
    variable v_seed2       : positive := shared_seed2;
  begin
      random(min_value, max_value, v_seed1, v_seed2, v_rand_scaled);
      -- Write back seeds
      shared_seed1 := v_seed1;
      shared_seed2 := v_seed2;
    return v_rand_scaled;
  end;

  -- Return a random real between min_value and max_value
  -- Use global seeds
  impure function random (
    constant min_value : real;
    constant max_value : real
  ) return real is
    variable v_rand_scaled : real;
    variable v_seed1       : positive := shared_seed1;
    variable v_seed2       : positive := shared_seed2;
  begin
      random(min_value, max_value, v_seed1, v_seed2, v_rand_scaled);
      -- Write back seeds
      shared_seed1 := v_seed1;
      shared_seed2 := v_seed2;
    return v_rand_scaled;
  end;

  -- Return a random time between min time and max time, using overload for the integer version of random()
  impure function random (
    constant min_value : time;
    constant max_value : time
  ) return time is
  begin
     return random(min_value/1 ns, max_value/1 ns) * 1 ns;
  end;

  --
  -- Procedure versions of random(), where seeds can be specified
  --
  -- Set target to a random SLV, using overload for the integer version of random().
  procedure random (
    variable v_seed1       : inout positive;
    variable v_seed2       : inout positive;
    variable v_target      : inout std_logic_vector
  ) is
    variable v_length      : integer  := v_target'length;
  begin
    -- Iterate through each bit and randomly set to 0 or 1
    for i in 0 to v_length-1 loop
      v_target(i downto i) := std_logic_vector(to_unsigned(random(0,1),1));
    end loop;
  end;

  -- Set target to a random SL, using overload for the integer version of random().
  procedure random (
    variable v_seed1       : inout positive;
    variable v_seed2       : inout positive;
    variable v_target      : inout std_logic
  ) is
    variable v_random_slv      : std_logic_vector(0 downto 0);
  begin
    v_random_slv := std_logic_vector(to_unsigned(random(0,1),1));
    v_target := v_random_slv(0);
  end;


  -- Set target to a random integer between min_value and max_value
  procedure random (
    constant min_value : integer;
    constant max_value : integer;
    variable v_seed1   : inout positive;
    variable v_seed2   : inout positive;
    variable v_target  : inout integer
  ) is
    variable v_rand    : real;
  begin
      -- Random real-number value in range 0 to 1.0
      uniform(v_seed1, v_seed2, v_rand);
      -- Scale to a random integer between min_value and max_value
      v_target := min_value + integer(trunc(v_rand*real(1+max_value-min_value)));
  end;

  -- Set target to a random integer between min_value and max_value
  procedure random (
    constant min_value : real;
    constant max_value : real;
    variable v_seed1   : inout positive;
    variable v_seed2   : inout positive;
    variable v_target  : inout real
  ) is
    variable v_rand    : real;
  begin
      -- Random real-number value in range 0 to 1.0
      uniform(v_seed1, v_seed2, v_rand);

      -- Scale to a random integer between min_value and max_value
      v_target := min_value + v_rand*(max_value-min_value);
  end;

  -- Set target to a random integer between min_value and max_value
  procedure random (
    constant min_value : time;
    constant max_value : time;
    variable v_seed1   : inout positive;
    variable v_seed2   : inout positive;
    variable v_target  : inout time
  ) is
    variable v_rand        : real;
    variable v_rand_int    : integer;
  begin
      -- Random real-number value in range 0 to 1.0
      uniform(v_seed1, v_seed2, v_rand);
      -- Scale to a random integer between min_value and max_value
      v_rand_int := min_value/1 ns + integer(trunc(v_rand*real(1 + max_value/1 ns - min_value / 1 ns)));
      v_target   := v_rand_int * 1 ns;
  end;

  -- Set global seeds
  procedure randomize (
    constant seed1 : positive;
    constant seed2 : positive;
    constant msg   : string         := "randomizing seeds";
    constant scope : string      := C_TB_SCOPE_DEFAULT
  ) is
  begin
      log(ID_UTIL_SETUP, "Setting global seeds to " & to_string(seed1) & ", " & to_string(seed2), scope);
      shared_seed1 := seed1;
      shared_seed2 := seed2;
  end;

  -- Set global seeds
  procedure randomise (
    constant seed1 : positive;
    constant seed2 : positive;
    constant msg   : string         := "randomising seeds";
    constant scope : string      := C_TB_SCOPE_DEFAULT
  ) is
  begin
      deprecate(get_procedure_name_from_instance_name(seed1'instance_name), "Use randomize().");
      log(ID_UTIL_SETUP, "Setting global seeds to " & to_string(seed1) & ", " & to_string(seed2), scope);
      shared_seed1 := seed1;
      shared_seed2 := seed2;
  end;

-- ============================================================================
-- Time consuming checks
-- ============================================================================

  --------------------------------------------------------------------------------
  -- await_change
  -- A signal change is required, but may happen already after 1 delta if min_time = 0 ns
  --------------------------------------------------------------------------------
  procedure await_change(
    signal   target      : boolean;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel := shared_msg_id_panel;
    constant value_type  : string         := "boolean"
    ) is
    constant name           : string := "await_change(" & value_type & ", " &
                                        to_string(min_time, ns) & ", " &
                                        to_string(max_time, ns) & ")";
    constant start_time     : time   := now;
  begin
    wait on target for max_time;
    check_time_window(target'event, now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_change(
    signal   target      : std_logic;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel := shared_msg_id_panel;
    constant value_type  : string         := "std_logic"
    ) is
    constant name           : string := "await_change(" & value_type & ", " &
                                        to_string(min_time, ns) & ", " &
                                        to_string(max_time, ns) & ")";
    constant start_time     : time   := now;
  begin
    wait on target for max_time;
    check_time_window(target'event, now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_change(
    signal   target      : std_logic_vector;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel := shared_msg_id_panel;
    constant value_type  : string         := "slv"
    ) is
    constant name           : string := "await_change(" & value_type & ", " &
                                        to_string(min_time, ns) & ", " &
                                        to_string(max_time, ns) & ")";
    constant start_time     : time   := now;
  begin
    wait on target for max_time;
    check_time_window(target'event, now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_change(
    signal   target      : unsigned;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel := shared_msg_id_panel;
    constant value_type  : string         := "unsigned"
    ) is
    constant name           : string := "await_change(" & value_type & ", " &
                                        to_string(min_time, ns) & ", " &
                                        to_string(max_time, ns) & ")";
    constant start_time     : time   := now;
  begin
    -- Note that overloading by casting target to slv without creating a new signal doesn't work
    wait on target for max_time;
    check_time_window(target'event, now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_change(
    signal   target      : signed;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel := shared_msg_id_panel;
    constant value_type  : string         := "signed"
    ) is
    constant name           : string := "await_change(" & value_type & ", " &
                                        to_string(min_time, ns) & ", " &
                                        to_string(max_time, ns) & ")";
    constant start_time     : time   := now;
  begin
    wait on target for max_time;
    check_time_window(target'event, now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_change(
    signal   target      : integer;
    constant min_time    : time;
    constant max_time    : time;
    constant alert_level : t_alert_level;
    constant msg         : string;
    constant scope       : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id      : t_msg_id       := ID_POS_ACK;
    constant msg_id_panel: t_msg_id_panel := shared_msg_id_panel;
    constant value_type  : string         := "integer"
    ) is
    constant name        : string := "await_change(" & value_type & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
    constant start_time  : time   := now;
  begin
    wait on target for max_time;
    check_time_window(target'event, now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  --------------------------------------------------------------------------------
  -- await_value
  --------------------------------------------------------------------------------
  -- Potential improvements
  --  - Adding an option that the signal must last for more than one delta cycle
  --    or a specified time
  --  - Adding an "AS_IS" option that does not allow the signal to change to other values
  --    before it changes to the expected value
  --
  -- The input signal is allowed to change to other values before ending up on the expected value,
  -- as long as it changes to the expected value within the time window (min_time to max_time).

  -- Wait for target = expected or timeout after max_time.
  -- Then check if (and when) the value changed to the expected
  procedure await_value (
    signal   target       : boolean;
    constant exp          : boolean;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type   : string := "boolean";
    constant start_time   : time   := now;
    constant v_exp_str    : string := to_string(exp);
    constant name         : string := "await_value(" & value_type & " " & v_exp_str & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
  begin
    if (target /= exp) then
      wait until (target = exp) for max_time;
    end if;
    check_time_window((target = exp), now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_value (
    signal   target       : std_logic;
    constant exp          : std_logic;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type   : string := "std_logic";
    constant start_time   : time   := now;
    constant v_exp_str    : string := to_string(exp);
    constant name         : string := "await_value(" & value_type & " " & v_exp_str & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
  begin
    if (target /= exp) then
      wait until (target = exp) for max_time;
    end if;
    check_time_window((target = exp), now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  procedure await_value (
    signal   target       : std_logic_vector;
    constant exp          : std_logic_vector;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant radix        : t_radix         := HEX_BIN_IF_INVALID;
    constant format       : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type   : string := "slv";
    constant start_time   : time   := now;
    constant v_exp_str    : string := to_string(exp, radix, format, INCL_RADIX);
    constant name         : string := "await_value(" & value_type & " " & v_exp_str & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
  begin
    if matching_widths(target, exp) then
      if not matching_values(target, exp) then
        wait until matching_values(target, exp) for max_time;
      end if;
      check_time_window(matching_values(target, exp), now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Widths did not match. " & msg, scope);
    end if;
  end;

  procedure await_value (
    signal   target       : unsigned;
    constant exp          : unsigned;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant radix        : t_radix         := HEX_BIN_IF_INVALID;
    constant format       : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type   : string := "unsigned";
    constant start_time   : time   := now;
    constant v_exp_str    : string := to_string(exp, radix, format, INCL_RADIX);
    constant name         : string := "await_value(" & value_type & " " & v_exp_str & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
  begin
    if matching_widths(target, exp) then
      if not matching_values(target, exp) then
        wait until matching_values(target, exp) for max_time;
      end if;
      check_time_window(matching_values(target, exp), now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Widths did not match. " & msg, scope);
    end if;
  end;

  procedure await_value (
    signal   target       : signed;
    constant exp          : signed;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant radix        : t_radix         := HEX_BIN_IF_INVALID;
    constant format       : t_format_zeros  := SKIP_LEADING_0;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type   : string          := "signed";
    constant start_time   : time   := now;
    constant v_exp_str    : string := to_string(exp, radix, format, INCL_RADIX);
    constant name         : string := "await_value(" & value_type & " " & v_exp_str & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
  begin
    if matching_widths(target, exp) then
      if not matching_values(target, exp) then
        wait until matching_values(target, exp) for max_time;
      end if;
      check_time_window(matching_values(target, exp), now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
    else
      alert(alert_level, name & " => Failed. Widths did not match. " & msg, scope);
    end if;
  end;

  procedure await_value (
    signal   target       : integer;
    constant exp          : integer;
    constant min_time     : time;
    constant max_time     : time;
    constant alert_level  : t_alert_level;
    constant msg          : string;
    constant scope        : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id       : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type   : string := "integer";
    constant start_time   : time   := now;
    constant v_exp_str    : string := to_string(exp);
    constant name         : string := "await_value(" & value_type & " " & v_exp_str & ", " &
        to_string(min_time, ns) & ", " & to_string(max_time, ns) & ")";
  begin
    if (target /= exp) then
      wait until (target = exp) for max_time;
    end if;
    check_time_window((target = exp), now-start_time, min_time, max_time, alert_level, name, msg, scope, msg_id, msg_id_panel);
  end;

  -- Helper procedure:
  -- Convert time from 'FROM_LAST_EVENT' to 'FROM_NOW'
  procedure await_stable_calc_time (
    constant target_last_event                : time;
    constant stable_req                       : time;                  -- Minimum stable requirement
    constant stable_req_from                  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout                          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from                     : t_from_point_in_time;  -- Which point in time the timeout starts
    variable stable_req_from_now              : inout time;            -- Calculated stable requirement from now
    variable timeout_from_await_stable_entry  : inout time;            -- Calculated timeout from procedure entry
    constant alert_level                      : t_alert_level;
    constant msg                              : string;
    constant scope                            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id                           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel                     : t_msg_id_panel  := shared_msg_id_panel;
    constant name                             : string          := "await_stable_calc_time()";
    variable stable_req_met                   : inout boolean          -- When true, the stable requirement is satisfied
  ) is
  begin
    stable_req_met            := false;

    -- Convert stable_req so that it points to "time_from_now"
    if stable_req_from = FROM_NOW then
      stable_req_from_now := stable_req;
    elsif stable_req_from = FROM_LAST_EVENT then
      -- Signal has already been stable for target'last_event,
      -- so we can subtract this in the FROM_NOW version.
      stable_req_from_now := stable_req - target_last_event;
    else
      alert(tb_error, name & " => Unknown stable_req_from." & msg, scope);
    end if;

    -- Convert timeout so that it points to "time_from_now"
    if timeout_from = FROM_NOW then
      timeout_from_await_stable_entry := timeout;
    elsif timeout_from = FROM_LAST_EVENT then
      timeout_from_await_stable_entry := timeout - target_last_event;
    else
      alert(tb_error, name & " => Unknown timeout_from." & msg, scope);
    end if;

    -- Check if requirement is already OK
    if (stable_req_from_now <= 0 ns) then
      log(msg_id, name & " => OK. Condition occurred immediately." & msg, scope, msg_id_panel);
      stable_req_met := true;
    end if;

    -- Check if it is impossible to achieve stable_req before timeout
    if (stable_req_from_now > timeout_from_await_stable_entry) then
      alert(alert_level, name & " => Failed immediately: Stable for stable_req = " & to_string(stable_req_from_now, ns) &
         " is not possible before timeout = " & to_string(timeout_from_await_stable_entry, ns) &
         ". " & msg, scope);
      stable_req_met := true;
    end if;

  end;

  -- Helper procedure:
  procedure await_stable_checks (
    constant start_time                       : time;             -- Time at await_stable() procedure entry
    constant stable_req                       : time;             -- Minimum stable requirement
    variable stable_req_from_now              : inout time;       -- Minimum stable requirement from now
    variable timeout_from_await_stable_entry  : inout time;       -- Timeout value converted to FROM_NOW
    constant time_since_last_event            : time;             -- Time since previous event
    constant alert_level                      : t_alert_level;
    constant msg                              : string;
    constant scope                            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id                           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel                     : t_msg_id_panel  := shared_msg_id_panel;
    constant name                             : string          := "await_stable_checks()";
    variable stable_req_met                   : inout boolean     -- When true, the stable requirement is satisfied
    ) is
    variable v_time_left                      : time;             -- Remaining time until timeout
    variable v_elapsed_time                   : time    := 0 ns;  -- Time since procedure entry
  begin
    stable_req_met  := false;
    v_elapsed_time  := now - start_time;
    v_time_left     := timeout_from_await_stable_entry - v_elapsed_time;

    -- Check if target has been stable for stable_req
    if (time_since_last_event >= stable_req_from_now) then
      log(msg_id, name & " => OK. Condition occurred after " &
          to_string(v_elapsed_time, C_LOG_TIME_BASE) & ". " & msg, scope, msg_id_panel);
      stable_req_met := true;
    end if;

    --
    -- Prepare for the next iteration in the loop in await_stable() procedure:
    --
    if not stable_req_met then

      -- Now that an event has occurred, the stable requirement is stable_req from now (regardless of stable_req_from)
      stable_req_from_now := stable_req;

      -- Check if it is impossible to achieve stable_req before timeout
      if (stable_req_from_now > v_time_left) then
        alert(alert_level, name & " => Failed. After " & to_string(v_elapsed_time, C_LOG_TIME_BASE) &
        ", stable for stable_req = " & to_string(stable_req_from_now, ns) &
           " is not possible before timeout = " & to_string(timeout_from_await_stable_entry, ns) &
        "(time since last event = " & to_string(time_since_last_event, ns) &
           ". " & msg, scope);
        stable_req_met := true;
      end if;
    end if;
  end;


  -- Wait until the target signal has been stable for at least 'stable_req'
  -- Report an error if this does not occurr within the time specified by 'timeout'.
  -- Note : 'Stable' refers to that the signal has not had an event (i.e. not changed value).
  -- Description of arguments:
  -- stable_req_from = FROM_NOW        : Target must be stable 'stable_req' from now
  -- stable_req_from = FROM_LAST_EVENT : Target must be stable 'stable_req' from the last event of target.
  -- timeout_from    = FROM_NOW        : The timeout argument is given in time from now
  -- timeout_from    = FROM_LAST_EVENT : The timeout argument is given in time the last event of target.
  procedure await_stable (
    signal   target           : boolean;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type                : string := "boolean";
    constant start_time                : time   := now;
    constant name                      : string := "await_stable(" & value_type & ", " & to_string(stable_req, ns) &
                                                   ", " & to_string(timeout, ns) & ")";
    variable v_stable_req_from_now     : time;             -- Stable_req relative to now.
    variable v_timeout_from_proc_entry : time;             -- Timeout relative to time of procedure entry
    variable v_stable_req_met          : boolean := false; -- When true, the procedure is done and has logged a conclusion.
  begin

    -- Use a helper procedure to simplify overloading
    await_stable_calc_time(
      target_last_event                 => target'last_event,
      stable_req                        => stable_req,
      stable_req_from                   => stable_req_from,
      timeout                           => timeout,
      timeout_from                      => timeout_from,
      stable_req_from_now               => v_stable_req_from_now,
      timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
      alert_level                       => alert_level,
      msg                               => msg,
      scope                             => scope,
      msg_id                            => msg_id,
      msg_id_panel                      => msg_id_panel,
      name                              => name,
      stable_req_met                    => v_stable_req_met);

    -- Start waiting for target'event or stable_req time, unless :
    --  - stable_req already achieved, or
    --  - it is already too late to be stable for stable_req before timeout will occurr
    while not v_stable_req_met loop
      wait until target'event for v_stable_req_from_now;

      -- Use a helper procedure to simplify overloading
      await_stable_checks (
        start_time                        => start_time,
        stable_req                        => stable_req,
        stable_req_from_now               => v_stable_req_from_now,
        timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
        time_since_last_event             => target'last_event,
        alert_level                       => alert_level,
        msg                               => msg,
        scope                             => scope,
        msg_id                            => msg_id,
        msg_id_panel                      => msg_id_panel,
        name                              => name,
        stable_req_met                    => v_stable_req_met);

    end loop;
  end;

  -- Note that the waiting for target'event can't be called from overloaded procedures where 'target' is a different type.
  -- Instead, the common code is put in helper procedures
  procedure await_stable (
    signal   target           : std_logic;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type                : string := "std_logic";
    constant start_time                : time   := now;
    constant name                      : string := "await_stable(" & value_type & ", " & to_string(stable_req, ns) &
                                                   ", " & to_string(timeout, ns) & ")";
    variable v_stable_req_from_now     : time;             -- Stable_req relative to now.
    variable v_timeout_from_proc_entry : time;             -- Timeout relative to time of procedure entry
    variable v_stable_req_met          : boolean := false; -- When true, the procedure is done and has logged a conclusion.
  begin

    -- Use a helper procedure to simplify overloading
    await_stable_calc_time(
      target_last_event                 => target'last_event,
      stable_req                        => stable_req,
      stable_req_from                   => stable_req_from,
      timeout                           => timeout,
      timeout_from                      => timeout_from,
      stable_req_from_now               => v_stable_req_from_now,
      timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
      alert_level                       => alert_level,
      msg                               => msg,
      scope                             => scope,
      msg_id                            => msg_id,
      msg_id_panel                      => msg_id_panel,
      name                              => name,
      stable_req_met                    => v_stable_req_met);

    -- Start waiting for target'event or stable_req time, unless :
    --  - stable_req already achieved, or
    --  - it is already too late to be stable for stable_req before timeout will occurr
    while not v_stable_req_met loop
      wait until target'event for v_stable_req_from_now;

      -- Use a helper procedure to simplify overloading
      await_stable_checks (
        start_time                        => start_time,
        stable_req                        => stable_req,
        stable_req_from_now               => v_stable_req_from_now,
        timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
        time_since_last_event             => target'last_event,
        alert_level                       => alert_level,
        msg                               => msg,
        scope                             => scope,
        msg_id                            => msg_id,
        msg_id_panel                      => msg_id_panel,
        name                              => name,
        stable_req_met                    => v_stable_req_met);

    end loop;
  end;

  procedure await_stable (
    signal   target           : std_logic_vector;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type                : string := "std_logic_vector";
    constant start_time                : time   := now;
    constant name                      : string := "await_stable(" & value_type & ", " & to_string(stable_req, ns) &
                                                   ", " & to_string(timeout, ns) & ")";
    variable v_stable_req_from_now     : time;             -- Stable_req relative to now.
    variable v_timeout_from_proc_entry : time;             -- Timeout relative to time of procedure entry
    variable v_stable_req_met          : boolean := false; -- When true, the procedure is done and has logged a conclusion.
  begin

    -- Use a helper procedure to simplify overloading
    await_stable_calc_time(
      target_last_event                 => target'last_event,
      stable_req                        => stable_req,
      stable_req_from                   => stable_req_from,
      timeout                           => timeout,
      timeout_from                      => timeout_from,
      stable_req_from_now               => v_stable_req_from_now,
      timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
      alert_level                       => alert_level,
      msg                               => msg,
      scope                             => scope,
      msg_id                            => msg_id,
      msg_id_panel                      => msg_id_panel,
      name                              => name,
      stable_req_met                    => v_stable_req_met);

    -- Start waiting for target'event or stable_req time, unless :
    --  - stable_req already achieved, or
    --  - it is already too late to be stable for stable_req before timeout will occurr
    while not v_stable_req_met loop
        wait until target'event for v_stable_req_from_now;

      -- Use a helper procedure to simplify overloading
      await_stable_checks (
        start_time                        => start_time,
        stable_req                        => stable_req,
        stable_req_from_now               => v_stable_req_from_now,
        timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
        time_since_last_event             => target'last_event,
        alert_level                       => alert_level,
        msg                               => msg,
        scope                             => scope,
        msg_id                            => msg_id,
        msg_id_panel                      => msg_id_panel,
        name                              => name,
        stable_req_met                    => v_stable_req_met);

    end loop;
  end;

  procedure await_stable (
    signal   target           : unsigned;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type                : string := "unsigned";
    constant start_time                : time   := now;
    constant name                      : string := "await_stable(" & value_type & ", " & to_string(stable_req, ns) &
                                                   ", " & to_string(timeout, ns) & ")";
    variable v_stable_req_from_now     : time;             -- Stable_req relative to now.
    variable v_timeout_from_proc_entry : time;             -- Timeout relative to time of procedure entry
    variable v_stable_req_met          : boolean := false; -- When true, the procedure is done and has logged a conclusion.
  begin

    -- Use a helper procedure to simplify overloading
    await_stable_calc_time(
      target_last_event                 => target'last_event,
      stable_req                        => stable_req,
      stable_req_from                   => stable_req_from,
      timeout                           => timeout,
      timeout_from                      => timeout_from,
      stable_req_from_now               => v_stable_req_from_now,
      timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
      alert_level                       => alert_level,
      msg                               => msg,
      scope                             => scope,
      msg_id                            => msg_id,
      msg_id_panel                      => msg_id_panel,
      name                              => name,
      stable_req_met                    => v_stable_req_met);

    -- Start waiting for target'event or stable_req time, unless :
    --  - stable_req already achieved, or
    --  - it is already too late to be stable for stable_req before timeout will occurr
    while not v_stable_req_met loop
      wait until target'event for v_stable_req_from_now;

      -- Use a helper procedure to simplify overloading
      await_stable_checks (
        start_time                        => start_time,
        stable_req                        => stable_req,
        stable_req_from_now               => v_stable_req_from_now,
        timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
        time_since_last_event             => target'last_event,
        alert_level                       => alert_level,
        msg                               => msg,
        scope                             => scope,
        msg_id                            => msg_id,
        msg_id_panel                      => msg_id_panel,
        name                              => name,
        stable_req_met                    => v_stable_req_met);

    end loop;
  end;

  procedure await_stable (
    signal   target           : signed;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type                : string := "signed";
    constant start_time                : time   := now;
    constant name                      : string := "await_stable(" & value_type & ", " & to_string(stable_req, ns) &
                                                   ", " & to_string(timeout, ns) & ")";
    variable v_stable_req_from_now     : time;             -- Stable_req relative to now.
    variable v_timeout_from_proc_entry : time;             -- Timeout relative to time of procedure entry
    variable v_stable_req_met          : boolean := false; -- When true, the procedure is done and has logged a conclusion.
  begin

    -- Use a helper procedure to simplify overloading
    await_stable_calc_time(
      target_last_event                 => target'last_event,
      stable_req                        => stable_req,
      stable_req_from                   => stable_req_from,
      timeout                           => timeout,
      timeout_from                      => timeout_from,
      stable_req_from_now               => v_stable_req_from_now,
      timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
      alert_level                       => alert_level,
      msg                               => msg,
      scope                             => scope,
      msg_id                            => msg_id,
      msg_id_panel                      => msg_id_panel,
      name                              => name,
      stable_req_met                    => v_stable_req_met);

    -- Start waiting for target'event or stable_req time, unless :
    --  - stable_req already achieved, or
    --  - it is already too late to be stable for stable_req before timeout will occurr
    while not v_stable_req_met loop
      wait until target'event for v_stable_req_from_now;

      -- Use a helper procedure to simplify overloading
      await_stable_checks (
        start_time                        => start_time,
        stable_req                        => stable_req,
        stable_req_from_now               => v_stable_req_from_now,
        timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
        time_since_last_event             => target'last_event,
        alert_level                       => alert_level,
        msg                               => msg,
        scope                             => scope,
        msg_id                            => msg_id,
        msg_id_panel                      => msg_id_panel,
        name                              => name,
        stable_req_met                    => v_stable_req_met);

    end loop;
  end;

  procedure await_stable (
    signal   target           : integer;
    constant stable_req       : time;                  -- Minimum stable requirement
    constant stable_req_from  : t_from_point_in_time;  -- Which point in time stable_req starts
    constant timeout          : time;                  -- Timeout if stable_req not achieved
    constant timeout_from     : t_from_point_in_time;  -- Which point in time the timeout starts
    constant alert_level      : t_alert_level;
    constant msg              : string;
    constant scope            : string          := C_TB_SCOPE_DEFAULT;
    constant msg_id           : t_msg_id        := ID_POS_ACK;
    constant msg_id_panel     : t_msg_id_panel  := shared_msg_id_panel
    ) is
    constant value_type                : string := "integer";
    constant start_time                : time   := now;
    constant name                      : string := "await_stable(" & value_type & ", " & to_string(stable_req, ns) &
                                                   ", " & to_string(timeout, ns) & ")";
    variable v_stable_req_from_now     : time;             -- Stable_req relative to now.
    variable v_timeout_from_proc_entry : time;             -- Timeout relative to time of procedure entry
    variable v_stable_req_met          : boolean := false; -- When true, the procedure is done and has logged a conclusion.
  begin

    -- Use a helper procedure to simplify overloading
    await_stable_calc_time(
      target_last_event                 => target'last_event,
      stable_req                        => stable_req,
      stable_req_from                   => stable_req_from,
      timeout                           => timeout,
      timeout_from                      => timeout_from,
      stable_req_from_now               => v_stable_req_from_now,
      timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
      alert_level                       => alert_level,
      msg                               => msg,
      scope                             => scope,
      msg_id                            => msg_id,
      msg_id_panel                      => msg_id_panel,
      name                              => name,
      stable_req_met                    => v_stable_req_met);

    -- Start waiting for target'event or stable_req time, unless :
    --  - stable_req already achieved, or
    --  - it is already too late to be stable for stable_req before timeout will occur
    while not v_stable_req_met loop
      wait until target'event for v_stable_req_from_now;

      -- Use a helper procedure to simplify overloading
      await_stable_checks (
        start_time                        => start_time,
        stable_req                        => stable_req,
        stable_req_from_now               => v_stable_req_from_now,
        timeout_from_await_stable_entry   => v_timeout_from_proc_entry,
        time_since_last_event             => target'last_event,
        alert_level                       => alert_level,
        msg                               => msg,
        scope                             => scope,
        msg_id                            => msg_id,
        msg_id_panel                      => msg_id_panel,
        name                              => name,
        stable_req_met                    => v_stable_req_met);

    end loop;
  end;

  -----------------------------------------------------------------------------------
  -- gen_pulse(sl)
  -- Generate a pulse on a std_logic for a certain amount of time
  --
  -- If blocking_mode = BLOCKING     : Procedure waits until the pulse is done before returning to the caller.
  -- If blocking_mode = NON_BLOCKING : Procedure starts the pulse, schedules the end of the pulse, then returns to the caller immediately.
  --
  procedure gen_pulse(
    signal   target         : inout std_logic;
    constant pulse_duration : time;
    constant blocking_mode  : t_blocking_mode;
    constant msg            : string;
    constant scope          : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id         : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
    ) is
  begin
    log(msg_id, "Pulse " &
                " for " & to_string(pulse_duration) & ". " & msg, scope);
    target <= '1';  -- Start pulse

    if (blocking_mode = BLOCKING) then
      wait for pulse_duration;
      target <= '0';
    else
      target <= transport '0' after pulse_duration;
    end if;
  end;

  -- Overload to allow excluding the blocking_mode argument:
  -- Make blocking_mode = BLOCKING by default
  procedure gen_pulse(
    signal   target         : inout std_logic;
    constant pulse_duration : time;
    constant msg            : string;
    constant scope          : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id         : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
    ) is
  begin
    gen_pulse(target, pulse_duration, BLOCKING, msg, scope, msg_id, msg_id_panel); -- Blocking mode by default
  end;

  -- gen_pulse(sl)
  -- Generate a pulse on a std_logic for a certain number of clock cycles
  procedure gen_pulse(
    signal   target         : inout std_logic;
    signal   clock_signal   : std_logic;
    constant num_periods    : natural;
    constant msg            : string;
    constant scope          : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id         : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel   : t_msg_id_panel := shared_msg_id_panel
  ) is
  begin
    log(msg_id, "Pulse " &
                " for " & to_string(num_periods) & " clk cycles. " & msg, scope);
    if (num_periods > 0) then
      wait until falling_edge(clock_signal);
      target  <= '1';
      for i in 1 to num_periods loop
        wait until falling_edge(clock_signal);
      end loop;
    else -- Pulse for one delta cycle only
      target  <= '1';
      wait for 0 ns;
    end if;
    target  <= '0';
  end;

  -- gen_pulse(slv)
  procedure gen_pulse(
    signal   target        : inout  std_logic_vector;
    constant pulse_value   : std_logic_vector;
    signal   clock_signal  : std_logic;
    constant num_periods   : natural;
    constant msg           : string;
    constant scope         : string         := C_TB_SCOPE_DEFAULT;
    constant msg_id        : t_msg_id       := ID_GEN_PULSE;
    constant msg_id_panel  : t_msg_id_panel := shared_msg_id_panel
  ) is
  begin
    log(msg_id, "Pulse to " & to_string(pulse_value, HEX, AS_IS, INCL_RADIX) &
                " for " & to_string(num_periods) & " clk cycles. " & msg, scope);

    if (num_periods > 0) then
      wait until falling_edge(clock_signal);
      target <= pulse_value;
      for i in 1 to num_periods loop
        wait until falling_edge(clock_signal);
      end loop;
    else -- Pulse for one delta cycle only
      target <= pulse_value;
      wait for 0 ns;
    end if;
    target(target'range) <= (others => '0');
  end;

  --------------------------------------------
  -- Clock generators :
  -- Include this as a concurrent procedure from your test bench.
  -- ( Including this procedure call as a concurrent statement directly in your architecture
  --   is in fact identical to a process, where the procedure parameters is the sensitivity list )
  --------------------------------------------
  procedure clock_generator(
    signal   clock_signal  : inout std_logic;
    constant clock_period  : in time
  ) is
    -- Making sure any rounding error after calculating period/2 is not accumulated.
    variable v_first_half_clk_period : time := clock_period / 2;
  begin
    loop
      clock_signal <= '1';
      wait for v_first_half_clk_period;
      clock_signal <= '0';
      wait for (clock_period - v_first_half_clk_period);
    end loop;
  end;

  --------------------------------------------
  -- Clock generator overload:
  -- - Enable signal (clock_ena) is added as a parameter
  -- - The clock goes to '1' immediately when the clock is enabled (clock_ena = true)
  -- - Log when the clock_ena changes. clock_name is used in the log message.
  --------------------------------------------
  procedure clock_generator(
    signal   clock_signal  : inout std_logic;
    signal   clock_ena     : in boolean;
    constant clock_period  : in time;
    constant clock_name    : in string
  ) is
    -- Making sure any rounding error after calculating period/2 is not accumulated.
    variable v_first_half_clk_period : time := clock_period / 2;
  begin
    loop
      if not clock_ena then
        log(ID_CLOCK_GEN, "Stopping clock " & clock_name);
        clock_signal <= '0';
        wait until clock_ena;
        log(ID_CLOCK_GEN, "Starting clock " & clock_name);
      end if;
      clock_signal <= '1';
      wait for v_first_half_clk_period;
      clock_signal <= '0';
      wait for (clock_period - v_first_half_clk_period);
    end loop;
  end;

end package body methods_pkg;

