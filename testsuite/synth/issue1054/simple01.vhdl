library ieee;
use ieee.std_logic_1164.all;

entity simple01 is
  port (a, b, c : in std_logic;
        z : out std_logic);
end simple01;

architecture behav of simple01 is
begin
  process(A, B, C)
    variable temp : std_logic;
  begin
    if is_x (a) then
      z <= b;
    else
      z <= b or c;
    end if;
  end process;
end behav;
