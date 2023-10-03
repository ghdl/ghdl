library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity A is
  generic (clk_period : time);
end A;

architecture str of A is
  signal d : std_logic_vector(7 downto 0) := (others => '1');
begin

  process
  begin
    wait for clk_period * 1;
    d <= "01010101";
    wait for clk_period * 1;
    wait;
  end process;

end architecture;