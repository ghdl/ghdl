
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.env.finish;

entity C is
end C;

architecture sim of C is
  constant clk_hz : integer := 100e6;
  constant clk_period : time := 1 sec / clk_hz;
begin

  DUT : entity work.A(str)
    generic map(clk_period => clk_period);

  DUV : entity work.B(rtl)
    generic map(clk_period => clk_period);

  process
  begin
    wait for clk_period * 3;
    finish;
  end process;

end architecture;