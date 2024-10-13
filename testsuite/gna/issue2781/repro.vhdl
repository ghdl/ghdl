library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity A is
  port (clk : in std_logic);
end A;

architecture str of A is
  signal C : unsigned(7 downto 0);
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;

entity B is
end B;

architecture sim of B is
  signal clk : std_logic := '1';
begin

  clk <= not clk after 5 ns;

  DUT : entity work.A(str)
    port map(clk => clk);

  process
  begin
    wait for 10 ns;
    << signal DUT.C : unsigned >> <= force x"ff";
    wait for 10 ns;
    << signal DUT.C : unsigned >> <= force x"00";
    wait for 10 ns;

    finish;
  end process;
end architecture;
