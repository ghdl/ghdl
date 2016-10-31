library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package stream is
  subtype valid_t is std_logic;
  subtype ready_t is std_logic;

  type array_ready_t is array (natural range <>) of ready_t;
  type array_valid_t is array (natural range <>) of valid_t;

  -- dummy functionality which needs to be shared
  procedure split_stream (
    signal outcomb    : out array_valid_t;
    signal outalone   : out ready_t;
    signal incomb     : in array_ready_t;
    signal inalone    : in valid_t);

end package;

package body stream is

  procedure split_stream (
    signal outcomb    : out array_valid_t;
    signal outalone   : out ready_t;
    signal incomb     : in array_ready_t;
    signal inalone    : in valid_t)
    is
  begin
    outcomb <= (outcomb'range => '1');
    outalone <= '1';
  end procedure split_stream;

end stream;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.stream.all;

entity BFPD is
port (
  reset      : in  std_logic;                                  -- asynchronous eset
  clock      : in  std_logic;                                  -- clock
  clock_en   : in  std_logic;                                  -- clock enable
  in_valid   : in  valid_t;
  in_ready   : out ready_t;
  out_valid  : out valid_t;
  out_ready  : in  ready_t
);
end BFPD;

architecture rtl of BFPD is

  type ctrl_t is (MANT_RET_IN, EXP_RET_IN, DIV_RET_IN,
                  MANT_RET_OUT, EXP_RET_OUT, DIV_RET_OUT,
                  MULT);

  subtype ctrl_range_t is integer range ctrl_t'pos(ctrl_t'left) to ctrl_t'pos(ctrl_t'right);
  subtype ret_split_t is integer range ctrl_t'pos(MANT_RET_IN) to ctrl_t'pos(DIV_RET_IN);

  signal ready: array_ready_t(ctrl_range_t);
  signal valid: array_valid_t(ctrl_range_t);

begin

  split_stream (outcomb => valid(ret_split_t), outalone => in_ready,
                incomb  => ready(ret_split_t), inalone  => in_valid);
  
end rtl;
