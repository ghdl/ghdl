library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity A1 is
end;

architecture str of A1 is
  signal C : unsigned(7 downto 0);
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;

entity B1 is
end;

architecture sim of B1 is
begin
  DUT : entity work.A1;

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
