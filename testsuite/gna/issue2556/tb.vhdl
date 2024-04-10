library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture tb of test is
begin
   p_main: process
   begin
    std.env.stop;
    wait;
  end process p_main;
end architecture;
