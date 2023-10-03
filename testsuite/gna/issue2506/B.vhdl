library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.B_pkg.all;

entity B is
  generic (clk_period : time);
end B;

architecture rtl of B is
begin

  process
  begin
    wait for clk_period * 2;
    p;
    wait for clk_period * 1;
    wait;
  end process;

end architecture;