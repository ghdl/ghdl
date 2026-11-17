library ieee;
use ieee.std_logic_1164.all;

entity mux01 is
  port (s1, s2, x, y : in std_logic;
        o1, o2 : out std_logic);
end entity;

architecture rtl of mux01 is
begin
  process (all)
    variable g, c : std_logic;
  begin
    if s1 = '1' then
      g := x;
    else
      g := y;
    end if;
    if s2 = '1' then
      c := g;
    else
      c := y;
    end if;
    o1 <= g;
    o2 <= c;
  end process;
end architecture;
