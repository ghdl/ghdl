library ieee;
use ieee.std_logic_1164.all;

entity int_test is
  port (clk : in std_logic;
        a, b : in integer range 0 to 1;
        c : out std_logic);
end int_test;

architecture rtl of int_test is
begin
  process (clk)
  begin
    if rising_edge (clk) then
      if a < b then
        c <= '0';
      end if;
    end if;
  end process;
end rtl;
