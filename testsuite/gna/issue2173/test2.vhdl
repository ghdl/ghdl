library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
end entity;

architecture rtl of test2 is
  signal s : unsigned(1 downto 0);
begin
  p_proc : process
    alias sig : unsigned  is s ;
  begin
    sig <= force "11";
    wait;
  end process;
end architecture;
