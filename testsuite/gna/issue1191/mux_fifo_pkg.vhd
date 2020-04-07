library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package mux_fifo_pkg is

  type t_mux_fifo_if_data is array (natural range <>) of std_logic_vector;

  type t_mux_fifo_if is record
    data  : t_mux_fifo_if_data;
    empty : std_logic_vector;
    rd    : std_logic_vector;
    clk   : std_logic;
  end record t_mux_fifo_if;

end mux_fifo_pkg;
