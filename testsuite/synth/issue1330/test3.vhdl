library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test3 is
    port(
        clk : in std_logic;
        write_data : in std_ulogic;
        arst : std_ulogic
        );
end;

architecture rtl of test3 is
begin
    test_1: process(clk, arst)
    begin
      if arst = '1' then
        null;
      elsif rising_edge(clk) then
          assert write_data = '0' report "bad" severity failure;
      end if;
    end process test_1;
end architecture rtl;
