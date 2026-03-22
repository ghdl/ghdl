library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.axis_pkg.all;

entity view02 is
  port (
    clk_i   : in std_logic;
    rst_i   : in std_logic;
    s_axis  :    view (axis_slave_view) of axis_rec_array(1 to 2);
    );
end;

architecture synthesis of view02 is
begin
  s_axis (1).ready <= '0';
  s_axis (2).ready <= '1';
end architecture synthesis;
