library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;

entity tb is
end entity;

architecture arch of tb is
begin
  DUT : entity work.B;

  process
  begin
    wait for 1 ns;
    finish;
  end process;
end architecture;
