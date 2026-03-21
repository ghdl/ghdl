library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.axis_pkg.all;

entity view01 is
  port (
    clk_i   : in std_logic;
    rst_i   : in std_logic;
    s_axis  :    view axis_slave_view
    );
end;

architecture synthesis of view01 is
begin
  s_axis.ready <= '0';
end architecture synthesis;
