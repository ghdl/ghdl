library ieee;
use ieee.std_logic_1164.all;

package mux is
  generic (
    MUX_DATA_SIZE : natural
  );
  subtype mux_data is std_logic_vector(MUX_DATA_SIZE-1 downto 0);
  type mux_data_array is array (natural range <>) of mux_data;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multiplexer is
  generic (
    MUX_DATA_SIZE : natural;
    MUX_CTRL_SIZE : natural;
    package mux_g is new work.mux generic map (MUX_DATA_SIZE => MUX_DATA_SIZE)
  );
  port (
    MUX_CTRL : in std_logic_vector(MUX_CTRL_SIZE-1 downto 0);
    MUX_IN : in mux_g.mux_data_array(0 to 2**MUX_CTRL_SIZE-1);
    MUX_OUT : out mux_g.mux_data
  );
end entity;

architecture multiplexer_arch of multiplexer is
begin
  MUX_OUT <= MUX_IN(to_integer(unsigned(MUX_CTRL)));
end architecture;
